var l1 = List.Cons({ head: 1, tail: List.Nil });
var l2 = List.Cons({ head: 2, tail: List.Nil });
var l3 = List.Cons({ head: l1, tail: List.Cons({ head: l2, tail: List.Nil }) });
var flat = List.flatten(l3);
var f1 = flat.value.head;
var f2 = flat.value.tail.value.head
console.log([f1, f2]);
