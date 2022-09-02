const vscode = require("vscode");
const FS = require("fs");
const Path = require("path");

const EXT_NAME = "ocamlearlybird";
const SUPPORTED_MAGICS = ["Caml1999X028", "Caml1999X029", "Caml1999X031"];
const MAGIC_LENGTH = SUPPORTED_MAGICS[0].length;

const log = (() => {
  const logger = vscode.window.createOutputChannel(EXT_NAME);
  const logWithLevel = (level) => (text) => {
    logger.appendLine(`${level}: ${text}`);
    logger.show();
  };
  return {
    info: logWithLevel("info"),
    error: logWithLevel("error"),
  };
})();

const isBytecodeFile = async (path) => {
  const stat = await FS.promises.stat(path).catch((err) => {
    if (err.code === "ENOENT") return null;
    throw err;
  });
  if (!stat || !stat.isFile()) return false;
  const file = await FS.promises.open(path);
  try {
    const buffer = Buffer.alloc(MAGIC_LENGTH);
    await file.read(buffer, 0, MAGIC_LENGTH, stat.size - MAGIC_LENGTH);
    const magic = buffer.toString("ascii");
    return SUPPORTED_MAGICS.includes(magic);
  } finally {
    file.close();
  }
};

const jsonStringifyFnSerializer = (k, v) =>
  k ? (typeof v === "function" ? `[fn ${v.name || k}]` : v) : v;
const asJson = (str) => JSON.stringify(str, jsonStringifyFnSerializer, 2);

module.exports = {
  /**@param {vscode.ExtensionContext} context */
  activate(context) {
    try {
      log.info(`activating`);
      const config = vscode.workspace.getConfiguration("ocamlearlybird");
      const serializedConfig = asJson(config);
      log.info(`configuration detected: ${serializedConfig}`);
      const ocamlearlybirdPath = config.get("path");
      context.subscriptions.push(
        vscode.debug.registerDebugAdapterDescriptorFactory("ocamlearlybird", {
          createDebugAdapterDescriptor(session, executable) {
            log.info(
              `on:createDebugAdapterDescriptor, ${asJson({
                session,
                executable,
              })}`
            );
            if (config.connectToLocalDebugAdapterServer) {
              return new vscode.DebugAdapterServer(4711, "localhost");
            }
            return new vscode.DebugAdapterExecutable(ocamlearlybirdPath, [
              "debug",
            ]);
          },
        })
      );
      context.subscriptions.push(
        vscode.commands.registerCommand(
          "ocamlearlybird.startDebug",
          async () => {
            try {
              log.info(`on:ocamlearlybird.startDebug`);
              const uri = vscode.window.activeTextEditor.document.uri;
              const folder = vscode.workspace.getWorkspaceFolder(uri);
              if (!folder) {
                throw new Error(
                  `No active text editor document found to start debug session on`
                );
              }
              const options = {
                name: Path.basename(uri.fsPath),
                type: "ocamlearlybird",
                request: "launch",
                stopOnEntry: true,
                yieldSteps: 4096,
                program: uri.fsPath,
              };
              log.info(`debug session starting with: ${asJson(options)}`);
              await vscode.debug.startDebugging(folder, options);
              log.info("debug session complete");
            } catch (err) {
              log.error(err);
            }
          }
        )
      );
      context.subscriptions.push(
        vscode.commands.registerCommand(
          "ocamlearlybird.variableGotoClosureCodeLocation",
          async (context) => {
            log.info(`on:ocamlearlybird.variableGotoClosureCodeLocation`);
            const result = await vscode.debug.activeDebugSession.customRequest(
              "variableGetClosureCodeLocation",
              { handle: context.variable.variablesReference }
            );
            if (result.location != null) {
              const loc = result.location;
              const doc = vscode.workspace.openTextDocument(
                result.location.source
              );
              vscode.window.showTextDocument(doc, {
                preview: true,
                selection: new vscode.Range(
                  new vscode.Position(loc.pos[0] - 1, loc.pos[1] - 1),
                  new vscode.Position(loc.end_[0] - 1, loc.end_[1] - 1)
                ),
              });
            } else {
              vscode.window.showInformationMessage("No closure code location");
            }
          }
        )
      );
      context.subscriptions.push(
        vscode.debug.registerDebugConfigurationProvider("ocamlearlybird", {
          async provideDebugConfigurations(folder, token) {
            return [
              {
                name: "OCaml Debug",
                type: "ocamlearlybird",
                request: "launch",
                program: "${workspaceFolder}/a.out",
                stopOnEntry: false,
                yieldSteps: 4096,
                onlyDebugGlob: "<${workspaceFolder}/**/*>",
              },
            ];
          },
          async resolveDebugConfiguration(folder, config, token) {
            if (!config.type) {
              config = {
                name: "${fileBasename}",
                type: "ocamlearlybird",
                request: "launch",
                program: "${file}",
              };
            }
            return config;
          },
          async resolveDebugConfigurationWithSubstitutedVariables(
            folder,
            config,
            token
          ) {
            if (!(await isBytecodeFile(config.program))) {
              vscode.window.showErrorMessage(
                `Expected OCaml bytecode file. ${config.program} is not supported`
              );
              return;
            }
            return config;
          },
        })
      );
    } catch (err) {
      log.error(`failed to activate extension: ${err}`);
    }
  },
  deactivate() {
    log("deactivated");
  },
};
