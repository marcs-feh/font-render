package set

import "core:mem"
import "base:builtin"

Empty :: struct {}

Set :: struct($T: typeid){
    items: map[T]Empty,
}

init :: proc(set: ^Set($E), allocator := context.allocator) -> mem.Allocator_Error {
    set.items = make(map[T]Empty, allocator)
}

create :: proc($T: typeid) -> (set: Set(T), err: mem.Allocator_Error) {
    set : Set(T)
    err = init(&set)
    return
}

destroy :: proc(set: ^Set($E)) {
    delete(set.items)
}

put :: proc(set: ^Set($E), elem: E){
    set.items[elem] = {}
}

pop :: proc(set: ^Set($E), elem: E) -> bool {
    ok := elem in set.items
    delete_key(&set.items, elem)
    return ok
}

has_value :: proc(set: Set($E), elem: E) -> bool {
    return elem in set.items
}

has_value_ptr :: proc(set: ^Set($E), elem: E) -> bool {
    return elem in set.items
}

has :: proc {
    has_value,
    has_value_ptr,
}

len :: proc(set: ^Set($E)) -> int {
    return builtin.len(set)
}

cap :: proc(set: ^Set($E)) -> int {
    return builtin.cap(set)
}