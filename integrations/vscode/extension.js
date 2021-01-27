const vscode = require('vscode');
const FS = require('fs');
const Path = require('path');
const CP = require('child_process');
const Util = require('util');

const magic = 'Caml1999X028';

const isBytecodeFile = async (path) => {
  try {
    const stat =  await FS.promises.stat(path);
    const file =  await FS.promises.open(path);
    try {
      const buffer = Buffer.alloc(magic.length);
      await file.read(buffer, 0, magic.length, stat.size - magic.length);
      return buffer.toString('ascii') === magic;
    } finally {
      file.close();
    }
  } catch {
    return false;
  }
};

const escapeRegex = (str) => {
  return str.replace(/[-[\]{}()*+!<=:?.\/\\^$|#\s,]/g, '\\$&');
};

const escapeHtml = (unsafe) => {
  return unsafe
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;");
}

const debugConfigProvider = {
  async provideDebugConfigurations(folder, token) {
      return [{
        name: 'OCaml Debug',
        type: 'ocaml',
        request: 'launch',
        program: 'a.out',
        stopOnEntry: false,
        yieldSteps: 4096,
        onlyDebugGlob: "<${workspaceRoot}/**/*>"
      }];
  },
  async resolveDebugConfiguration(folder, config, token) {
    if (!config.type) {
      config = {
        name: '${fileBasename}',
        type: 'ocaml',
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

const bytecodeViewProvider = {
  /**
  *
  * @param {vscode.Uri} uri
  * @param {vscode.CustomDocumentOpenContext} openContext
  * @param {vscode.CancellationToken} token
  * @returns {{objdump: string}}
  */
  async openCustomDocument(uri, openContext, token) {
    const result = await Util.promisify(CP.exec)(`ocamlobjinfo ${uri.fsPath}`);
    return {
      uri,
      objdump: result.stdout,
      dispose: () => {}
    };
  },
  /**
   *
   * @param {{objdump: string}} document
   * @param {vscode.WebviewPanel} webviewPanel
   * @param {vscode.CancellationToken} token
   */
  async resolveCustomEditor(document, webviewPanel, token) {
    webviewPanel.webview.html = `<pre><code>${escapeHtml(document.objdump)}</code></pre>`;
  }
};

module.exports = {
  activate(context) {
    const config = vscode.workspace.getConfiguration('ocamlDebugger');
    const extensions = config.get('extensions', []);
    const extensionsRegex = new RegExp(extensions.map(ext => `${escapeRegex(ext)}$`).join('|'), 'i');
    context.subscriptions.push(vscode.commands.executeCommand('setContext', 'ocamlDebugger.extensionsRegex', extensionsRegex));
    context.subscriptions.push(vscode.commands.registerCommand('ocamlDebugger.startDebug', async (uri) => {
      const folder = vscode.workspace.getWorkspaceFolder(uri);
      await vscode.debug.startDebugging(folder, {
        name: Path.basename(uri.fsPath),
        type: "ocaml",
        request: "launch",
        stopOnEntry: true,
        yieldSteps: 4096,
        program: uri.fsPath,
      });
    }));
    context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('ocaml', debugConfigProvider));
    context.subscriptions.push(vscode.window.registerCustomEditorProvider('ocamlDebugger.bytecode', bytecodeViewProvider))
  },
  deactivate() {}
};
