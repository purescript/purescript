console.log(bind(Just("a"))(function(a) {
	return bind(Just("b"))(function(b) {
		return Just(a + b);
	});
}));
