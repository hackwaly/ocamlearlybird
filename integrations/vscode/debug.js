const child_process = require('child_process');

child_process.spawn('ocamlearlybird', ['debug'], {
  stdio: ['inherit', 'inherit', 'inherit']
});
