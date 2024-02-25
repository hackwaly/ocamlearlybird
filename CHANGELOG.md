## 1.3.2 - 2024-02-25

### Added

* Add OCaml 5.2 support (#60, #61).

## 1.3.1 - 2024-01-11

### Fixed

* Fix bytecode-only installation (#55).

## 1.3.0 - 2024-01-05

### Added

* Add opaque value inspection using runtime representation (#9, #52, #53).

## 1.2.1 - 2023-08-18

### Added

* Add OCaml 5.1 support.

## 1.2.0 - 2023-07-11

### Added

* Add OCaml 4.13, 4.14 and 5.0 support.
* Add `--version` command line option.

### Changed

* Relicense under MIT (from GPL).
* Deprecate own VSCode extension in favor of VSCode OCaml Platform integration: https://github.com/ocamllabs/vscode-ocaml-platform/pull/1148.

### Fixed

* Fix being stuck if program doesn't exist (#49).

### Removed

* Remove OCaml 4.11 support.

## 1.1.0 - 2021-03-31

### Added

* Added OCaml 4.12.0 Support.
Note: Ocamlearlybird built under ocamlc 4.12 can not debug bytecode produced by ocamlc 4.11, and vice versa.

## 1.0.3 - 2021-02-23

### Fixed

* Fix breakpoints resolution algorithm.
* Fix variables pane sometimes flooding by `Assertion_failure(...)` raised at Env_hack.ml.
* Fix incorrectly inspect 'a type as int.
* Output to debug console when uncaught_exc occurs.

## 1.0.2 - 2021-02-22

### Added

* Show %accu pseudo variable at Event_after event.
* Added variable context menu "Goto Closure Code Location".

### Fixed

* Respect linesStartAt1 and columnsStartAt1 options.
* Fix occasional breakpointLocations command exception.
* Fix stopped at first event in main module cannot display stack frames when using onlyDebugGlob option.

## 1.0.1

### Added

* Allow to set breakpoints on files which has same digest of sources.

### Fixed

* Fix variables pane sometimes flooding by `Typenv__Envaux_hack.Error(...)`.
* Fix inspect array variables cause infinite loading.

## 1.0.0

Initial release.
