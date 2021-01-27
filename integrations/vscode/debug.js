const child_process = require('child_process');

const cp = child_process.spawn('ocamlearlybird', ['debug']);
process.stdin.pipe(cp.stdin);
cp.stdout.pipe(process.stdout);
cp.stderr.pipe(process.stderr);
