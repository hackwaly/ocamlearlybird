const vscode = require('vscode');
const FS = require('fs');
const Path = require('path');
const CP = require('child_process');
const Util = require('util');

const SUPPORTED_MAGICS = ['Caml1999X028', 'Caml1999X029'];
const MAGIC_LENGTH = SUPPORTED_MAGICS[0].length;


const isBytecodeFile = async (path) => {
  try {
    const stat = await FS.promises.stat(path);
    const file = await FS.promises.open(path);
    try {
      const buffer = Buffer.alloc(MAGIC_LENGTH);
      await file.read(buffer, 0, MAGIC_LENGTH, stat.size - MAGIC_LENGTH);
      const magic = buffer.toString('ascii');
      return SUPPORTED_MAGICS.includes(magic);
    } finally {
      file.close();
    }
  } catch {
    return false;
  }
};

const debugConfigProvider = {
  async provideDebugConfigurations(folder, token) {
    return [{
      name: 'OCaml Debug',
      type: 'ocamlearlybird',
      request: 'launch',
      program: '${workspaceFolder}/a.out',
      stopOnEntry: false,
      yieldSteps: 4096,
      onlyDebugGlob: "<${workspaceFolder}/**/*>"
    }];
  },
  async resolveDebugConfiguration(folder, config, token) {
    if (!config.type) {
      config = {
        name: '${fileBasename}',
        type: 'ocamlearlybird',
        request: 'launch',
        program: '${file}',
      };
    }
    return config;
  },
  async resolveDebugConfigurationWithSubstitutedVariables(folder, config, token) {
    if (!await isBytecodeFile(config.program)) {
      vscode.window.showErrorMessage('Unsupported file format');
      return;
    }
    return config;
  }
};

module.exports = {
  /**@param {vscode.ExtensionContext} context */
  activate(context) {
    const config = vscode.workspace.getConfiguration('ocamlearlybird');
    const ocamlearlybirdPath = config.get('path');
    context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory('ocamlearlybird', {
      createDebugAdapterDescriptor(session, executable) {
        return new vscode.DebugAdapterExecutable(ocamlearlybirdPath, ['debug']);
      }
    }));
    context.subscriptions.push(vscode.commands.registerCommand('ocamlearlybird.startDebug', async (uri) => {
      const folder = vscode.workspace.getWorkspaceFolder(uri);
      await vscode.debug.startDebugging(folder, {
        name: Path.basename(uri.fsPath),
        type: "ocamlearlybird",
        request: "launch",
        stopOnEntry: true,
        yieldSteps: 4096,
        program: uri.fsPath,
      });
    }));
    context.subscriptions.push(vscode.commands.registerCommand('ocamlearlybird.variableGotoClosureCodeLocation', async (context) => {
      const result = await vscode.debug.activeDebugSession.customRequest("variableGetClosureCodeLocation", { handle: context.variable.variablesReference });
      if (result.location != null) {
        const loc = result.location;
        const doc = vscode.workspace.openTextDocument(result.location.source);
        vscode.window.showTextDocument(doc, {
          preview: true,
          selection: new vscode.Range(
            new vscode.Position(loc.pos[0] - 1, loc.pos[1] - 1),
            new vscode.Position(loc.end_[0] - 1, loc.end_[1] - 1),
          ),
        });
      } else {
        vscode.window.showInformationMessage("No closure code location");
      }
    }));
    context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('ocamlearlybird', debugConfigProvider));
  },
  deactivate() { }
};
