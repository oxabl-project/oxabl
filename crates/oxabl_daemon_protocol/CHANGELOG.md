# Changelog

## [0.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon_protocol-v0.2.0...oxabl_daemon_protocol-v0.3.0) (2026-09-21)


### ⚠ BREAKING CHANGES

* **daemon:** CONTRACT_VERSION is 4. It is compared by exact value at the handshake and there is no negotiation window, so a client and a daemon built against different contract versions refuse to talk to each other. The daemon and every client of it must be rebuilt and deployed together.

### Bug Fixes

* **daemon:** close the registry's symlink hole and stop the probe taking the lock it tests ([#173](https://github.com/oxabl-project/oxabl/issues/173)) ([3fa0bc2](https://github.com/oxabl-project/oxabl/commit/3fa0bc2c402cab88218372fed7d9c3400148bc05))
* **daemon:** contract 4 is not compatible with contract 3 ([ec517b1](https://github.com/oxabl-project/oxabl/commit/ec517b16084504b8eac2657b0eb689449a7a82c3))
* **daemon:** move the socket and lock to the runtime directory, and key discovery on a canonical root ([#186](https://github.com/oxabl-project/oxabl/issues/186)) ([68c6a6f](https://github.com/oxabl-project/oxabl/commit/68c6a6f571013e47d694e7685de1ace222e3b7ce))
* **daemon:** shared session correctness, and the contract bump that covers the series ([#174](https://github.com/oxabl-project/oxabl/issues/174)) ([39bba72](https://github.com/oxabl-project/oxabl/commit/39bba72396c8c4a8b241056b91d44b776d068a31))

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon_protocol-v0.1.0...oxabl_daemon_protocol-v0.2.0) (2026-08-14)


### Features

* the daemon session core, and the wire a client can afford ([#165](https://github.com/oxabl-project/oxabl/issues/165)) ([5184c04](https://github.com/oxabl-project/oxabl/commit/5184c0470ebe55c83241ef8c72a5976d4e6de235))
