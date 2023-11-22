// Run a command like this to generate purs.eventlog
// (probably need to build with profiling enabled)
//     purs +RTS -l-agu -i1.5 -hc -RTS compile -g corefn $(spago sources)
// Process it with
//     eventlog2html --json purs.eventlog
//     node trace.js purs.eventlog.json
var filename = process.argv[2];
if (!filename) throw new Error("Provide a file name");
var eventlog = JSON.parse(require("fs").readFileSync(filename, "utf-8"));

var traces = {};
var name_length = 0;
var timings = [];
var minTx = Infinity;
var maxTx = -Infinity;
var total = 0;

for (let trace of eventlog.traces) {
    var m = /^([\w.]+) (start|end)$/.exec(trace.desc);
    if (!m) continue;
    var name = m[1];
    if (!(name in traces)) traces[name] = {};
    if (name.length > name_length) name_length = name.length;
    var ev = m[2];
    if (traces[name][ev]) {
        console.log("Warn: duplicate event", trace.desc);
    }
    traces[name][ev] = trace.tx;
    if (trace.tx < minTx) minTx = trace.tx;
    if (trace.tx > maxTx) maxTx = trace.tx;
    if ("start" in traces[name] && "end" in traces[name]) {
        traces[name].time = traces[name].end - traces[name].start;
        timings.push([name, traces[name].time]);
        total += traces[name].time;
    }
}
for (let name in traces) {
    if (!("time" in traces[name]))
        console.log("Warn: missing timing for", name, traces[name]);
}

timings.sort(([n1,t1], [n2,t2]) => t1 - t2);
timings.push(["stats", "----"]);
timings.push(["total", total]);
timings.push(["timespan", maxTx - minTx]);
timings.push(["ratio", total/(maxTx - minTx)]);

for (let [name, time] of timings) {
    console.log(name.padEnd(name_length, " "), time);
}

