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
var con = [];
var max_cons = [[]];

// I guess some buffering makes it out of order?
eventlog.traces.sort(({tx: tx1}, {tx: tx2}) => tx1 - tx2);

for (let trace of eventlog.traces) {
    var m = /^([\w.]+) (start|end)$/.exec(trace.desc);
    if (!m) continue;
    var name = m[1];
    if (!(name in traces)) traces[name] = {};
    if (name.length > name_length) name_length = name.length;
    var ev = m[2];
    if (traces[name][ev]) {
        if (traces[name].time === 0) {
            console.log("Warn: start after end", name, traces[name].start, trace.tx);
        } else {
            console.log("Warn: duplicate event", trace.desc);
        }
        continue;
    }
    traces[name][ev] = trace.tx;
    if (ev === "end" && !("start" in traces[name])) {
        console.log("Warn: missing start for", name);
        traces[name].start = trace.tx;
        traces[name].time = 0;
        continue;
    }
    if (trace.tx < minTx) minTx = trace.tx;
    if (trace.tx > maxTx) maxTx = trace.tx;
    if ("start" in traces[name] && "end" in traces[name]) {
        traces[name].time = traces[name].end - traces[name].start;
        timings.push([name, traces[name].time]);
        total += traces[name].time;
    }

    if (ev === "start") con = con.concat([name]);
    if (ev === "end") {
        var l = con.length;
        con = con.filter(n => n !== name);
        if (con.length !== l - 1) {
            console.log(con, name);
        }
    }
    if (con.length >= max_cons[0].length) {
        if (con.length > max_cons[0].length)
            max_cons = [];
        max_cons.push(con);
    }
}
for (let name in traces) {
    if (!("time" in traces[name]))
        console.log("Warn: missing timing for", name, traces[name]);
}

var timespan = maxTx - minTx;

timings.sort(([n1,t1], [n2,t2]) => t1 - t2);
timings.push(["stats", "----"]);
timings.push(["total", total]);
timings.push(["timespan", timespan]);
timings.push(["ratio (avg concurrency?)", total/timespan]);
var max_con_time = 0;
var concurrencies = max_cons.map(max_con => {
    if (max_con.length !== max_cons[0].length)
        throw new Error("max_con length error");
    var modules = max_con.map(name => [name, traces[name]]);
    var start = Math.max(...modules.map(([name, {start}]) => start));
    var end = Math.min(...modules.map(([name, {end}]) => end));
    var time = end - start;
    max_con_time += time;
    return {
        modules,
        start,
        end,
        time,
    };
});
timings.push(["max concurrency", max_cons[0].length]);
timings.push(["time at max concurrency (s)", max_con_time]);
timings.push(["time at max concurrency (%)", 100*max_con_time/timespan]);

for (let [name, time] of timings) {
    console.log(name.padEnd(name_length, " "), time);
}

//require("fs").writeFileSync("concurrencies.json", JSON.stringify(concurrencies, null, 2), "utf-8");

