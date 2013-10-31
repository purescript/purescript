function select(s) { return $(s); }
function create(s) { return $(s); }
function attr(j, a) { return j.attr(o); }
function css(j, a) { return j.css(o); }
function append(j1, j2) { return j1.append(j2); }
function appendTo(j1, j2) { return j1.appendTo(j2); }
function appendText(j1, s) { return j1.append(s); }
var main = (function () {var div = create("<div>");div = css(appendText(div)("Hello World!"))({color: "#FF0000"});div = appendTo(div)(select("body"));return {}})();
main()
