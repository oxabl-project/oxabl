//! A stable text rendering of a [`FileFlow`].

use std::fmt::Write;

use oxabl_ast::Span;

use crate::FileFlow;
use crate::model::*;

pub(crate) fn dump(flow: &FileFlow, source: &str) -> String {
    let lines = LineIndex::new(source);
    let mut out = String::new();
    if let Some(d) = flow.directive() {
        let d = match d {
            ErrorDirective::RoutineLevel => "routine-level",
            ErrorDirective::BlockLevel => "block-level",
        };
        let _ = writeln!(out, "directive {d}");
    }
    for cfg in flow.units() {
        unit(&mut out, cfg, source, &lines);
    }
    out
}

fn unit(out: &mut String, cfg: &Cfg, source: &str, lines: &LineIndex) {
    let u = cfg.unit();
    let _ = write!(out, "unit u{} {}", cfg.id().index(), u.kind.as_str());
    if let Some(name) = &u.name {
        let _ = write!(out, " {name}");
    }
    if let Some(p) = u.parent {
        let _ = write!(out, " parent=u{}", p.index());
    }
    out.push('\n');

    for (id, b) in cfg.blocks() {
        let _ = write!(
            out,
            "  b{} {} @{}",
            id.index(),
            b.kind.as_str(),
            lines.pos(b.span.start)
        );
        if let Some(l) = &b.label {
            let _ = write!(out, " label={l}");
        }
        if let Some(p) = b.parent {
            let _ = write!(out, " parent=b{}", p.index());
        }
        for (flag, name) in [
            (b.iterating, "iterating"),
            (b.transaction, "transaction"),
            (b.stop_after, "stop-after"),
            (b.input_blocking, "input"),
        ] {
            if flag {
                let _ = write!(out, " {name}");
            }
        }
        if !b.buffers.is_empty() {
            let _ = write!(out, " buffers={}", b.buffers.join(","));
        }
        if let Some(t) = &b.catch_type {
            let _ = write!(out, " type={t}");
        }
        out.push('\n');
        for c in Condition::ALL {
            if let Some(h) = b.handling(c) {
                let _ = write!(out, "    on {}: {}", c.as_str(), handling(h));
                if c == Condition::Error
                    && matches!(h.action, Action::Retry(_))
                    && h.origin != Origin::Phrase
                {
                    let becomes = match b.retry_becomes() {
                        RetryBehavior::Retry => "retry",
                        RetryBehavior::Next => "next",
                        RetryBehavior::Leave => "leave",
                    };
                    let _ = write!(out, " => {becomes}");
                }
                out.push('\n');
            }
        }
    }

    for (ix, n) in cfg.nodes() {
        let _ = write!(
            out,
            "  n{} {} b{}",
            ix.index(),
            node_kind(n.kind),
            n.block.index()
        );
        if matches!(n.kind, NodeKind::Stmt | NodeKind::Head) {
            let _ = write!(
                out,
                " {} `{}`",
                lines.pos(n.span.start),
                snippet(source, n.span)
            );
        }
        if let Some(r) = n.opaque {
            let _ = write!(out, " opaque={}", r.as_str());
        }
        out.push('\n');
        for e in cfg.successors(ix) {
            let _ = writeln!(out, "    -> n{} {}", e.to.index(), edge_kind(e.kind));
        }
    }
}

fn handling(h: &Handling) -> String {
    let mut s = String::from("UNDO");
    if let Some(l) = &h.undo {
        s.push(' ');
        s.push_str(l);
    }
    s.push_str(", ");
    let (word, label) = match &h.action {
        Action::Retry(l) => ("RETRY", l.as_deref()),
        Action::Leave(l) => ("LEAVE", l.as_deref()),
        Action::Next(l) => ("NEXT", l.as_deref()),
        Action::Return(ReturnKind::Normal) => ("RETURN", None),
        Action::Return(ReturnKind::Error) => ("RETURN ERROR", None),
        Action::Return(ReturnKind::NoApply) => ("RETURN NO-APPLY", None),
        Action::Throw => ("THROW", None),
    };
    s.push_str(word);
    if let Some(l) = label {
        s.push(' ');
        s.push_str(l);
    }
    let origin = match h.origin {
        Origin::Phrase => "phrase",
        Origin::Default => "default",
        Origin::RoutineLevel => "routine-level",
        Origin::BlockLevel => "block-level",
    };
    let _ = write!(s, " ({origin})");
    s
}

fn node_kind(k: NodeKind) -> String {
    match k {
        NodeKind::Entry => "entry".into(),
        NodeKind::Exit(e) => format!("exit({})", e.as_str()),
        NodeKind::Stmt => "stmt".into(),
        NodeKind::Head => "head".into(),
        NodeKind::End => "end".into(),
        NodeKind::After(b) => format!("after(b{})", b.index()),
        NodeKind::Handler(c) => format!("handler({})", c.as_str()),
    }
}

fn edge_kind(k: EdgeKind) -> String {
    match k {
        EdgeKind::Normal => "normal".into(),
        EdgeKind::True => "true".into(),
        EdgeKind::False => "false".into(),
        EdgeKind::When(i) => format!("when({i})"),
        EdgeKind::Otherwise => "otherwise".into(),
        EdgeKind::Preproc => "preproc".into(),
        EdgeKind::LoopBody => "loop-body".into(),
        EdgeKind::LoopDone => "loop-done".into(),
        EdgeKind::LoopBack => "loop-back".into(),
        EdgeKind::Leave => "leave".into(),
        EdgeKind::Next => "next".into(),
        EdgeKind::Retry => "retry".into(),
        EdgeKind::Return => "return".into(),
        EdgeKind::ReturnError => "return-error".into(),
        EdgeKind::Throw => "throw".into(),
        EdgeKind::Raise(c) => format!("raise({})", c.as_str()),
        EdgeKind::Catch => "catch".into(),
    }
}

/// The first line of the span's text, whitespace collapsed, at most 48 chars.
fn snippet(source: &str, span: Span) -> String {
    let end = (span.end as usize).min(source.len());
    let start = (span.start as usize).min(end);
    let text = source.get(start..end).unwrap_or("");
    let line = text.lines().next().unwrap_or("");
    let collapsed = line.split_whitespace().collect::<Vec<_>>().join(" ");
    if collapsed.chars().count() > 48 {
        let cut: String = collapsed.chars().take(45).collect();
        format!("{cut}...")
    } else {
        collapsed
    }
}

struct LineIndex {
    starts: Vec<u32>,
}

impl LineIndex {
    fn new(source: &str) -> Self {
        let mut starts = vec![0];
        starts.extend(
            source
                .bytes()
                .enumerate()
                .filter(|&(_, b)| b == b'\n')
                .map(|(i, _)| i as u32 + 1),
        );
        LineIndex { starts }
    }

    fn pos(&self, offset: u32) -> String {
        let line = self.starts.partition_point(|&s| s <= offset);
        let col = offset - self.starts[line - 1] + 1;
        format!("{line}:{col}")
    }
}
