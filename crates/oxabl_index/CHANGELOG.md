# Changelog

## [1.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_index-v0.1.2...oxabl_index-v1.0.0) (2026-08-14)


### ⚠ BREAKING CHANGES

* `SymbolTable::inherited_member_type` and `record_inherited_member_type` are removed; read `Symbol::data_type`.

### Features

* answer who depends on this, and what must rebuild ([#164](https://github.com/oxabl-project/oxabl/issues/164)) ([9284e18](https://github.com/oxabl-project/oxabl/commit/9284e1887efb00788532209495108d750b1a97b8))
* judge the cross-file population, and drain the top of the unmodelled-statement suppression ([#153](https://github.com/oxabl-project/oxabl/issues/153)) ([a1e03e6](https://github.com/oxabl-project/oxabl/commit/a1e03e6e1484ebd1bf3a7e6b194c74df8affbc4b))
* **pipeline:** wire cross-file resolution into every client, and prove they agree ([#148](https://github.com/oxabl-project/oxabl/issues/148)) ([967331b](https://github.com/oxabl-project/oxabl/commit/967331bc7b4a0ae288dc5398a9f1c01d4be9278a))
* **semantic:** resolve cross-file names, with the rules still held still ([#147](https://github.com/oxabl-project/oxabl/issues/147)) ([8873de9](https://github.com/oxabl-project/oxabl/commit/8873de9a13f5ef5c30d584f13e63e21cea6300e0))
* the daemon session core, and the wire a client can afford ([#165](https://github.com/oxabl-project/oxabl/issues/165)) ([5184c04](https://github.com/oxabl-project/oxabl/commit/5184c0470ebe55c83241ef8c72a5976d4e6de235))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 1.0.1 to 1.0.2
    * oxabl_schema bumped from 0.2.1 to 0.2.2
    * oxabl_semantic bumped from 1.0.1 to 1.0.2
    * oxabl_workspace bumped from 1.0.1 to 1.0.2
  * dev-dependencies
    * oxabl_schema bumped from 0.2.1 to 0.2.2
