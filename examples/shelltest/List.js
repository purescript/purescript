var l1 = Cons({ head: 1, tail: Nil });
var l2 = Cons({ head: 2, tail: Nil });
var l3 = Cons({ head: l1, tail: Cons({ head: l2, tail: Nil }) });
var flat = flatten(l3);
var f1 = flat.value.head;
var f2 = flat.value.tail.value.head
console.log([f1, f2]);
