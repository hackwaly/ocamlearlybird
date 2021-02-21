## 1.0.2 (beta2) - 2021-02-22

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
