console.log(Maybe.bind(Maybe.Just("a"))(function(a) {
	return Maybe.bind(Maybe.Just("b"))(function(b) {
		return Maybe.Just(a + b);
	});
}));
