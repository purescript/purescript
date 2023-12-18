// Debug compilation times of modules from eventlog profiling
//
// Build and run purs with profiling enabled:
//     cabal build --enable-profiling
//     cabal exec -- purs ......
// Or with stack:
//     stack build --profile
//     stack --profile exec -- purs ......
// Run a command like this to generate purs.eventlog:
//     purs +RTS -l-agu -i1.5 -hc -RTS compile -g corefn $(spago sources)
// (If you want accurate stats for individual modules, add -N1.)
// Process it with
//     eventlog2html --json purs.eventlog
//     node eventlog.js purs.eventlog.json
//
// See the GHC docs for descriptions of the RTS flags:
//   - https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#rts-options-for-heap-profiling
//   - https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#rts-eventlog
//   - https://downloads.haskell.org/ghc/latest/docs/users_guide/using-concurrent.html?highlight=threaded#rts-options-for-smp-parallelism
var mainFile = process.argv[2];
if (!mainFile) throw new Error("Provide a file name");

var name_length = 0;

function summarizeEventlog(filename) {
    var eventlog = JSON.parse(require("fs").readFileSync(filename, "utf-8"));
    // eventlog.heap
    //   c: Set(3) { 'Heap Size', 'Live Bytes', 'Blocks Size' }
    // eventlog.samples
    // eventlog.traces

    var traces = {};
    var minTx = Infinity;
    var maxTx = -Infinity;
    var maxMem = -Infinity;
    var total = 0;
    var con = [];
    var max_cons = [[]];
    var cursor = 0;

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

        var time = trace.tx;
        if (time < minTx) minTx = time;
        if (time > maxTx) maxTx = time;

        while (cursor < eventlog.heap.length && eventlog.heap[cursor].x < trace.tx) {
            cursor++;
            if (eventlog.heap[cursor].c !== 'Heap Size') {
                cursor = eventlog.heap.length;
            }
        }
        if (ev === "start") {
            traces[name].cursor = cursor;
        }

        traces[name][ev] = time;
        if (ev === "end" && !("start" in traces[name])) {
            console.log("Warn: missing start for", name);
            traces[name].start = time;
            traces[name].time = 0;
            continue;
        }
        if ("start" in traces[name] && "end" in traces[name]) {
            traces[name].time = traces[name].end - traces[name].start;
            var mems = eventlog.heap.slice(traces[name].cursor, cursor).map(e => e.y);
            var mem_min = Math.min(...mems);
            var mem_max = Math.max(...mems);
            var maxMem = Math.max(maxMem, mem_max);
            Object.assign(traces[name], {mem_min,mem_max});
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

    var timespan = maxTx - minTx;

    return { traces, total, minTx, maxTx, timespan, max_cons, maxMem };
}

var mainFiles = process.argv.slice(2);

if (mainFiles.length > 1) {
    for (let file of mainFiles) {
        console.log(file);
        var { traces, total, timespan, max_cons, maxMem } = summarizeEventlog(file);
        if (timespan === -Infinity && total === 0 && max_cons[0].length === 0) continue;
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
        console.log("timespan                   ", timespan);
        console.log("ratio (avg concurrency?)   ", total/timespan);
        console.log("max concurrency            ", max_cons[0].length);
        console.log("time at max concurrency (%)", 100*max_con_time/timespan);
        console.log("peak heap size             ", space(maxMem));
    }
    process.exit(0);
}

var { traces, total, timespan, max_cons } = summarizeEventlog(mainFile);

var timings = [];
for (let name in traces) {
    let trace = traces[name];
    if (!("time" in trace)) {
        console.log("Warn: missing timing for", name, trace);
    } else if (trace.time > 0) {
        timings.push([name, trace.time]);
    }
}

timings.sort(([n1,t1,_1,m1], [n2,t2,_2,m2]) => t1 - t2);

timings.push(["stats", "-----"]);
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
    // console.log(name.padEnd(name_length, " "), (""+time).substring(0, 5).padStart(5, " "));
    console.log(name.padEnd(name_length, " "), time);
}

//require("fs").writeFileSync("concurrencies.json", JSON.stringify(concurrencies, null, 2), "utf-8");


function space(v) {
    if (!isFinite(v)) return "----";
    if (v === Infinity) return "+Inf";
    if (v === -Infinity) return "-Inf";
    if (v !== v) return " NaN";
    var sizes = [
        [1_000_000_000, "G"],
        [1_000_000,     "M"],
        [1_000,         "K"],
        [0,             ""],
    ]
    for (let [value, suffix] of sizes) {
        if (v < value) continue;
        if (!suffix) return (""+v).padStart(4, " ");
        var adj = v/value;
        var str = ""+adj;
        if (adj >= 100) return str.substring(0,3)+suffix;
        if (adj >= 10) return " "+str.substring(0,2)+suffix;
        return str.substring(0,3)+suffix;
    }
}
function signed(fmt, v) {
    if (!isFinite(v)) return " "+fmt(v);
    if (v < 0) return "-"+fmt(-v);
    return "+"+fmt(v);
}
