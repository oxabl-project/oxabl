# Changelog

## [0.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.2.0...oxabl_daemon-v0.3.0) (2026-09-21)


### ⚠ BREAKING CHANGES

* **daemon:** the daemon's socket, lock and registration live under $XDG_RUNTIME_DIR/oxabl/daemon instead of the cache directory, and discovery is keyed on the canonical workspace root. A client built before this change will not discover a daemon built after it, and vice versa; a daemon left running at the old location is not adopted and exits on its own idle timeout. When XDG_RUNTIME_DIR is unset or empty the chain falls back to XDG_CACHE_HOME, then $HOME/.cache, then the system temp directory, each warning about the guarantee it gives up. A value that is set but is not an absolute path is refused outright, naming the variable and the value, rather than falling through.

### Bug Fixes

* analyze include roots as fragments ([#166](https://github.com/oxabl-project/oxabl/issues/166)) ([dba8a3c](https://github.com/oxabl-project/oxabl/commit/dba8a3cb0cdea9f753ae6e1161bd36aeb3014409))
* **daemon:** close the registry's symlink hole and stop the probe taking the lock it tests ([#173](https://github.com/oxabl-project/oxabl/issues/173)) ([3fa0bc2](https://github.com/oxabl-project/oxabl/commit/3fa0bc2c402cab88218372fed7d9c3400148bc05))
* **daemon:** detect added files in freshness, and correct span extents and lookup cost ([#187](https://github.com/oxabl-project/oxabl/issues/187)) ([d50be6a](https://github.com/oxabl-project/oxabl/commit/d50be6a897742e622b0c4f281de4a6d1d9c2c355))
* **daemon:** move the socket and lock to the runtime directory, and key discovery on a canonical root ([#186](https://github.com/oxabl-project/oxabl/issues/186)) ([68c6a6f](https://github.com/oxabl-project/oxabl/commit/68c6a6f571013e47d694e7685de1ace222e3b7ce))
* **daemon:** shared session correctness, and the contract bump that covers the series ([#174](https://github.com/oxabl-project/oxabl/issues/174)) ([39bba72](https://github.com/oxabl-project/oxabl/commit/39bba72396c8c4a8b241056b91d44b776d068a31))
* **daemon:** the socket moved, so an older client no longer finds a daemon ([77ab645](https://github.com/oxabl-project/oxabl/commit/77ab645f5d1813e98d4c5d21ceb14473f68da920))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.1 to 1.1.2
    * oxabl_daemon_protocol bumped from 0.2.0 to 0.3.0
    * oxabl_index bumped from 1.0.0 to 1.0.1
    * oxabl_pipeline bumped from 1.1.1 to 1.1.2
    * oxabl_semantic bumped from 1.0.2 to 1.0.3
    * oxabl_workspace bumped from 1.0.2 to 1.0.3
  * dev-dependencies
    * oxabl_schema bumped from 0.2.2 to 0.2.3
    * oxabl_pipeline bumped from 1.1.1 to 1.1.2

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.1.1...oxabl_daemon-v0.2.0) (2026-08-14)


### Features

* the daemon session core, and the wire a client can afford ([#165](https://github.com/oxabl-project/oxabl/issues/165)) ([5184c04](https://github.com/oxabl-project/oxabl/commit/5184c0470ebe55c83241ef8c72a5976d4e6de235))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.0 to 1.1.1
    * oxabl_daemon_protocol bumped from 0.1.0 to 0.2.0
    * oxabl_index bumped from 0.1.2 to 1.0.0
    * oxabl_pipeline bumped from 1.1.0 to 1.1.1
    * oxabl_semantic bumped from 1.0.1 to 1.0.2
    * oxabl_workspace bumped from 1.0.1 to 1.0.2
  * dev-dependencies
    * oxabl_schema bumped from 0.2.1 to 0.2.2
    * oxabl_pipeline bumped from 1.1.0 to 1.1.1
