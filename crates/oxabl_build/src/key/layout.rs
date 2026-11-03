//! Where each unit compiles from: its PROPATH, its compile name, and whether
//! it compiles at all.

use std::collections::BTreeMap;

use rustc_hash::FxHashMap;

use crate::paths::propath::{Placement, Search, base_directory, is_class_unit, place};
use crate::paths::{PathMap, declared_name};
use crate::refresh::{Outcome, parallel};

/// One unit's place in the build.
#[derive(Debug)]
pub struct Placed {
    /// The index of its PROPATH in [`Layout::propaths`]; `None` when its
    /// package directory does not match the class it declares.
    pub propath: Option<usize>,
    pub class: bool,
    /// The name a class unit declares, when its own text states one.
    pub declared: Option<String>,
    pub placement: Placement,
}

/// The placements of all units, by position in `Outcome::units`.
#[derive(Debug)]
pub struct Layout {
    /// The distinct unit PROPATHs; a PROPATH id is an index into it.
    pub propaths: Vec<Vec<String>>,
    pub units: Vec<Placed>,
}

fn read_declared(paths: &PathMap, unit: &str) -> Option<String> {
    let bytes = std::fs::read(paths.absolute(unit)).ok()?;
    declared_name(&String::from_utf8_lossy(&bytes))
}

/// The stem r-code of `name` lands at, lower-cased: r-code is often deployed
/// to case-insensitive systems.
fn stem(name: &str) -> String {
    let stem = match name.rsplit_once('.') {
        Some((stem, extension)) if !extension.contains('/') => stem,
        _ => name,
    };
    stem.to_ascii_lowercase()
}

/// Skips every unit of a group that shares an output with another, except the
/// one with the smallest path.
fn skip_collisions(outcome: &Outcome, units: &mut [Placed]) {
    let mut groups: BTreeMap<String, Vec<usize>> = BTreeMap::new();
    for (at, placed) in units.iter().enumerate() {
        if let Placement::Compiles { name } = &placed.placement {
            groups.entry(stem(name)).or_default().push(at);
        }
    }
    for group in groups.values_mut().filter(|group| group.len() > 1) {
        group.sort_by(|a, b| {
            let (a, b) = (outcome.units[*a], outcome.units[*b]);
            outcome.name(a).cmp(outcome.name(b))
        });
        let winner = outcome.name(outcome.units[group[0]]).to_string();
        for at in &group[1..] {
            units[*at].placement = Placement::Skipped(format!("output-collision: {winner}"));
        }
    }
}

/// Places every unit of `outcome`.
pub fn lay_out(outcome: &Outcome, paths: &PathMap, search: &Search) -> Layout {
    let declared: Vec<Option<String>> = parallel::map(&outcome.units, |id| {
        let path = outcome.name(*id);
        is_class_unit(path)
            .then(|| read_declared(paths, path))
            .flatten()
    });
    let mut by_base: FxHashMap<String, usize> = FxHashMap::default();
    let mut ids: FxHashMap<Vec<String>, usize> = FxHashMap::default();
    let mut propaths: Vec<Vec<String>> = Vec::new();
    let mut staged: Vec<(Option<usize>, Result<String, String>)> = Vec::new();
    for (id, declared) in outcome.units.iter().zip(&declared) {
        let path = outcome.name(*id);
        let base = base_directory(path, declared.as_deref());
        let propath = base.as_ref().ok().map(|base| {
            *by_base.entry(base.clone()).or_insert_with(|| {
                let propath = search.propath(paths, base);
                *ids.entry(propath.clone()).or_insert_with(|| {
                    propaths.push(propath);
                    propaths.len() - 1
                })
            })
        });
        staged.push((propath, base));
    }
    let is_file = |canonical: &str| {
        std::fs::metadata(paths.absolute(canonical)).is_ok_and(|meta| meta.is_file())
    };
    let work: Vec<usize> = (0..outcome.units.len()).collect();
    let placements = parallel::map(&work, |at| {
        let path = outcome.name(outcome.units[*at]);
        match &staged[*at] {
            (Some(propath), Ok(base)) => place(
                path,
                base,
                declared[*at].is_some(),
                &propaths[*propath],
                is_file,
            ),
            (_, Err(reason)) => Placement::Skipped(reason.clone()),
            (None, Ok(_)) => unreachable!("a base directory has a PROPATH"),
        }
    });
    let mut units: Vec<Placed> = placements
        .into_iter()
        .zip(staged)
        .zip(declared)
        .zip(&outcome.units)
        .map(|(((placement, (propath, _)), declared), id)| Placed {
            propath,
            class: is_class_unit(outcome.name(*id)),
            declared,
            placement,
        })
        .collect();
    skip_collisions(outcome, &mut units);
    Layout { propaths, units }
}
