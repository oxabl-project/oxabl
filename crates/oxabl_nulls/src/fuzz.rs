//! Random ABL programs for the termination and monotonicity tests: nested
//! loops and branches over three-valued conditions on DATE, INTEGER and
//! LOGICAL locals.

pub(crate) struct Programs {
    state: u64,
}

impl Programs {
    pub fn new(seed: u64) -> Self {
        Self { state: seed.max(1) }
    }

    fn next(&mut self) -> u64 {
        // xorshift64*
        let mut x = self.state;
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.state = x;
        x.wrapping_mul(0x2545_f491_4f6c_dd1d)
    }

    fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }

    fn chance(&mut self, percent: usize) -> bool {
        self.below(100) < percent
    }

    fn pick<'a>(&mut self, options: &[&'a str]) -> &'a str {
        options[self.below(options.len())]
    }

    fn var(&mut self, prefix: char, count: usize) -> String {
        format!("{prefix}{}", self.below(count))
    }

    pub fn program(&mut self) -> String {
        let count = 2 + self.below(4);
        let mut out = Vec::new();
        for i in 0..count {
            let init = self.pick(&["", " INITIAL TODAY", " INITIAL ?"]);
            out.push(format!("DEFINE VARIABLE d{i} AS DATE NO-UNDO{init}."));
        }
        for i in 0..count {
            let init = self.pick(&["", " INITIAL ?", " INITIAL 3", " INITIAL TODAY"]);
            out.push(format!("DEFINE VARIABLE i{i} AS INTEGER NO-UNDO{init}."));
        }
        for i in 0..count {
            out.push(format!("DEFINE VARIABLE l{i} AS LOGICAL NO-UNDO."));
        }
        out.push("DEFINE INPUT PARAMETER p0 AS INTEGER NO-UNDO.".into());
        for _ in 0..2 + self.below(4) {
            let s = self.stmt(0, count);
            out.push(s);
        }
        out.push("PROCEDURE p:".into());
        let s = self.stmt(0, count);
        out.push(s);
        out.push("END PROCEDURE.".into());
        out.join("\n")
    }

    fn int(&mut self, n: usize) -> String {
        match self.below(9) {
            0..=2 => self.var('i', n),
            3 => self.pick(&["0", "1", "?", "5"]).into(),
            4 => format!("({} + 1)", self.var('i', n)),
            5 => format!("({} / {})", self.var('i', n), self.var('i', n)),
            6 => format!("LENGTH(STRING({}))", self.var('i', n)),
            7 => format!("NUM-ENTRIES({})", self.var('i', n)),
            _ => "p0".into(),
        }
    }

    fn date(&mut self, n: usize) -> String {
        match self.below(4) {
            0 | 1 => self.var('d', n),
            2 => self.pick(&["TODAY", "?"]).into(),
            _ => format!("({} + 1)", self.var('d', n)),
        }
    }

    fn cond(&mut self, depth: usize, n: usize) -> String {
        let r = self.below(100);
        if depth > 2 || r < 30 {
            return match self.below(8) {
                0 => {
                    let op = self.pick(&["=", "<>", ">", "<", ">=", "<="]);
                    format!("{} {op} {}", self.var('i', n), self.int(n))
                }
                1 => {
                    let op = self.pick(&["=", "<>", ">", "<"]);
                    format!("{} {op} {}", self.var('d', n), self.date(n))
                }
                2 => self.var('l', n),
                3 => "AVAILABLE Customer".into(),
                4 => format!("{} = ?", self.var('i', n)),
                5 => format!("{} <> ?", self.var('d', n)),
                6 => format!("{} <> ?", self.var('i', n)),
                _ => format!("NOT {} BEGINS {}", self.var('i', n), self.int(n)),
            };
        }
        if r < 50 {
            return format!("NOT ({})", self.cond(depth + 1, n));
        }
        let op = if r < 75 { "AND" } else { "OR" };
        format!(
            "({}) {op} ({})",
            self.cond(depth + 1, n),
            self.cond(depth + 1, n)
        )
    }

    fn stmt(&mut self, depth: usize, n: usize) -> String {
        let r = self.below(100);
        if depth > 2 || r < 35 {
            return match self.below(13) {
                0 => format!("{} = {}.", self.var('i', n), self.int(n)),
                1 => format!("{} = {}.", self.var('d', n), self.date(n)),
                2 => {
                    let value = match self.below(3) {
                        0 => "TRUE".into(),
                        1 => "FALSE".into(),
                        _ => self.cond(3, n),
                    };
                    format!("{} = {value}.", self.var('l', n))
                }
                3 => format!("MESSAGE {} + {}.", self.var('i', n), self.int(n)),
                4 => "FIND FIRST Customer NO-LOCK NO-ERROR.".into(),
                5 => "IF ERROR-STATUS:ERROR THEN LEAVE.".into(),
                6 => "RUN p.".into(),
                7 => self.pick(&["LEAVE.", "NEXT.", "RETURN."]).into(),
                8 => format!(
                    "ASSIGN {} = {} {} = {}.",
                    self.var('i', n),
                    self.int(n),
                    self.var('d', n),
                    self.date(n)
                ),
                9 => "MESSAGE Customer.Name.".into(),
                10 => format!("IF {} > TODAY THEN MESSAGE 1.", self.var('d', n)),
                11 => format!("UPDATE {}.", self.var('i', n)),
                _ => format!("{} = ?.", self.var('i', n)),
            };
        }
        let body = |s: &mut Self| {
            let k = 1 + s.below(3);
            (0..k)
                .map(|_| s.stmt(depth + 1, n))
                .collect::<Vec<_>>()
                .join("\n")
        };
        if r < 60 {
            let c = self.cond(0, n);
            let then = body(self);
            let mut s = format!("IF {c} THEN DO:\n{then}\nEND.");
            if self.chance(50) {
                let other = body(self);
                s.push_str(&format!(" ELSE DO:\n{other}\nEND."));
            }
            return s;
        }
        if r < 75 {
            let c = self.cond(0, n);
            return format!("DO WHILE {c}:\n{}\nEND.", body(self));
        }
        if r < 85 {
            let (v, from, to) = (self.var('i', n), self.int(n), self.int(n));
            let kind = self.pick(&["DO", "REPEAT"]);
            return format!("{kind} {v} = {from} TO {to}:\n{}\nEND.", body(self));
        }
        if r < 92 {
            let inner = body(self);
            let c = self.cond(0, n);
            return format!("REPEAT:\n{inner}\nIF {c} THEN LEAVE.\nEND.");
        }
        let (v, a, b) = (self.var('i', n), body(self), body(self));
        format!("CASE {v}:\n WHEN 1 THEN DO:\n{a}\nEND.\n OTHERWISE DO:\n{b}\nEND.\nEND CASE.")
    }
}
