# ocamlearlybird

## How to use

1. Switch to ocaml 4.11
2. Install earlybird and make sure ocamlearlybird in your PATH.
3. Install vscode extension hackwaly/ocamlearlybird

## Quick debug

You can select context menu "Start an OCaml Debug Session" on bytecode file in vscode explorer to quickly start an debug session.

## Example

[Debug utop](https://i.imgur.com/U3GDHXM.gif)

## Example launch configuration

Used to debug utop examples.

```
{
    // Use IntelliSense to learn about possible attributes.
    // Hover to view descriptions of existing attributes.
    // For more information, visit: https://go.microsoft.com/fwlink/?linkid=830387
    "version": "0.2.0",
    "configurations": [
        {
            "name": "test_program",
            "type": "ocamlearlybird",
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
