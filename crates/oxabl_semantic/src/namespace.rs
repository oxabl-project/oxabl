//! ABL name-space partitioning.
//!
//! ABL resolves different kinds of names in different name spaces: a variable
//! `customer`, a buffer `customer`, and a schema table `customer` can all
//! coexist, and resolution order narrows candidate name spaces based on
//! syntactic context (e.g. `FOR EACH Customer` starts in `Buffers`/`Tables`,
//! not `Values`). Each scope holds one [`BindingMap`](crate::BindingMap) per
//! namespace, indexed by the discriminant below.

/// Discriminant for the ten name spaces v1 models. See plan §Technical
/// Approach → Namespaces.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum NamespaceId {
    /// Variables, parameters, properties, constants, fields of the active
    /// buffer. The default resolution start for a bare identifier.
    Values = 0,
    /// `DEFINE BUFFER` targets and schema tables used as implicit buffers.
    Buffers = 1,
    /// Schema tables — may share names with `Buffers` via default buffer.
    Tables = 2,
    /// Classes and interfaces, declared or `USING`-imported.
    Types = 3,
    /// Internal procedures.
    Procedures = 4,
    /// User-defined functions.
    Functions = 5,
    /// Stream handles.
    Streams = 6,
    /// Frame handles.
    Frames = 7,
    /// User-defined events.
    Events = 8,
    /// Widget identifiers reachable via `:attribute`.
    WidgetHandles = 9,
}

/// Number of name spaces. Used as the fixed size of per-scope binding map
/// arrays.
pub const NUM_NAMESPACES: usize = 10;

impl NamespaceId {
    /// Dense index into a per-scope `[_; NUM_NAMESPACES]`.
    #[inline]
    pub const fn index(self) -> usize {
        self as u8 as usize
    }

    /// Enumerate every namespace in declaration order.
    pub const ALL: [NamespaceId; NUM_NAMESPACES] = [
        NamespaceId::Values,
        NamespaceId::Buffers,
        NamespaceId::Tables,
        NamespaceId::Types,
        NamespaceId::Procedures,
        NamespaceId::Functions,
        NamespaceId::Streams,
        NamespaceId::Frames,
        NamespaceId::Events,
        NamespaceId::WidgetHandles,
    ];
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indices_match_discriminants() {
        assert_eq!(NamespaceId::Values.index(), 0);
        assert_eq!(NamespaceId::Buffers.index(), 1);
        assert_eq!(NamespaceId::WidgetHandles.index(), 9);
    }

    #[test]
    fn all_has_num_namespaces_entries() {
        assert_eq!(NamespaceId::ALL.len(), NUM_NAMESPACES);
    }
}
