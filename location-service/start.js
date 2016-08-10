#!/usr/bin/env node
var program = require('commander');
var Server = require('./server');

function noop(x) {
  return x;
}

function port(x) {
  return parseInt(x, 10);
}

function collect(val, memo) {
  memo.push(val);
  return memo;
}

function increaseVerbosity(v, total) {
  return total + 1;
}

program
  .version('0.0.1', '-V, --version')
  .usage('[options]')
  .option('-p, --port <n>', 'Service port', port, 30000)
  .option('-r, --rest-port <n>', 'Rest interface port', port, 3000)
  .option('-h, --host <n>', 'Hostname (both for rest $ service)', noop, '0.0.0.0')
  .option('-c, --couchbase-address <n>', 'Hostname of Couchbase', noop, null)
  .option('-b, --bootstrap-node [value]', 'Node to connect to', collect, [])
  .option('-v, --verbose', 'Add logs verbosity', increaseVerbosity, 0)
  .parse(process.argv);

var server = Server({
  bootstrapNodes: program.bootstrapNode,
  verbosity: program.verbose,
  port: program.port,
  restPort: program.restPort,
  host: program.host,
  couchbaseAddress: program.couchbaseAddress
});

server.startDHT(function () {
  server.startREST()
}, process.exit);

