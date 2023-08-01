# OCaml earlybird

[![ci workflow status](https://github.com/hackwaly/ocamlearlybird/actions/workflows/ci.yml/badge.svg)](https://github.com/hackwaly/ocamlearlybird/actions/workflows/ci.yml)
[![GitHub release status](https://img.shields.io/github/v/release/hackwaly/ocamlearlybird)](https://github.com/hackwaly/ocamlearlybird/releases)
[![opam package status](https://badgen.net/opam/v/earlybird)](https://opam.ocaml.org/packages/earlybird)

OCaml debug adapter.

## Installation

```console
opam install earlybird
```

## Usage

### VS Code

See [Debugging OCaml programs (experimental)](https://github.com/ocamllabs/vscode-ocaml-platform#debugging-ocaml-programs-experimental) in VSCode OCaml Platform README.
This requires VSCode OCaml Platform 1.13 or newer.

> PS. Since the integration into VSCode OCaml Platform, the old "Ocamlearlybird" VSCode extension is deprecated.

## Troubleshooting

### Breakpoints not hit with `(lang dune 3.0)` and above

Change to `(lang dune 3.7)` or above and add `(map_workspace_root false)` to your `dune-project`.
See [dune documentation](https://dune.readthedocs.io/en/stable/dune-files.html#map-workspace-root) for more information.

## Examples

### utop

[Screen capture of debugging utop](https://i.imgur.com/U3GDHXM.gif).

Launch configuration used:
```json
{
    // Use IntelliSense to learn about possible attributes.
    // Hover to view descriptions of existing attributes.
    // For more information, visit: https://go.microsoft.com/fwlink/?linkid=830387
    "version": "0.2.0",
    "configurations": [
        {
            "name": "test_program",
            "type": "ocaml.earlybird",
            "request": "launch",
            "stopOnEntry": true,
            "console": "integratedTerminal",
            "program": "${workspaceFolder}/_build/default/examples/interact/test_program.bc",
            "onlyDebugGlob": "<${workspaceFolder}/**/*>",
            "yieldSteps": 1024,
            "cwd": "${workspaceFolder}"
        }
    ]
}
```
