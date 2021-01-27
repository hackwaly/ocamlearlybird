# ocamlearlybird

## How to use

1. Switch to ocaml 4.11
2. Install earlybird-411 and make sure ocamlearlybird in your PATH.
3. Install vscode extension hackwaly/ocamlearlybird

## How to install

```
git clone https://github.com/hackwaly/ocamlearlybird.git
cd ocamlearlybird
git checkout 411
opam install .
```

## Quick debug

You can select context menu "Start an OCaml Debug Session" on bytecode file in vscode explorer to quickly start an debug session.

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
            "type": "ocaml",
            "request": "launch",
            "stopOnEntry": true,
            "console": "integratedTerminal",
            "program": "${workspaceRoot}/_build/default/examples/interact/test_program.bc",
            "onlyDebugGlob": "<${workspaceRoot}/_build/default/**/*>",
            "yieldSteps": 1024,
            "cwd": "${workspaceRoot}"
        }
    ]
}
```
