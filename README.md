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

## Configuration

| Field | Type | Default value | Description |
| ----- | ---- | ------- | ----------- |
| `program` | `string` | _(required)_ | The path of debuggee program. |
| `arguments` | `string[]` | `[]` | The command-line arguments for the debuggee program. |
| `cwd` | `string` |  | The working directory for debuggee program. |
| `env` | `{[var: string]: string}` | `{}` | Environment variables passed to the debuggee program. |
| `stopOnEntry` | `boolean` | `false` | Automatically stop after launch. |
| `console` | `'internalConsole' \|`<br>`'integratedTerminal' \|`<br>` 'externalTerminal'` | `internalConsole` | Where to launch the debug target: internal console, integrated terminal, or external terminal. |
| `followForkMode` | `'forkChild' \| 'forkParent'` | `forkParent` | Set which process the debugger follows on fork. |
| `source_dirs` | `string[]` | `[]` | The path to search sources. |
| `onlyDebugGlob` | `string` | `true` | Only debug sources which match `onlyDebugGlob`. |
| `yieldSteps` | `number` | `4096` | Max steps to execute in batch. Debugger can not response other requests when executing steps in batch. |
| `_debugLog` | `string` |  | File to Log debug messages. |

## Troubleshooting

### Breakpoints not hit with `(lang dune 3.0)` and above

Change to `(lang dune 3.7)` or above and add `(map_workspace_root false)` to your `dune-project`.
See [dune documentation](https://dune.readthedocs.io/en/stable/reference/dune-project/map_workspace_root.html) for more information.

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
