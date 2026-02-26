# Nex Object Layout v1

Object layout is subobject composition:

`[ObjHeader | SharedBases... | BaseSubobject(B) | BaseSubobject(C) | DerivedFields...]`

- deterministic base order
- one vtable pointer per class subobject
- metadata records size, align, pointer map, base offsets, vtable shape

## ObjHeader

Every heap-allocated object begins with an 8-byte `ObjHeader`:

`[type_id: i64]`

- `type_id` is the index into the reflection type registry (assigned by `nex_reflect_register_type` at startup)
- Used by `is` type-check patterns in `match` expressions: the runtime loads the type ID and calls `nex_reflect_instanceof(type_id, "TypeName")` to walk the class hierarchy and interface list
- 8 bytes of overhead per object — standard for any language with runtime type checking
- Constructors (`init`) stamp the type ID as the first operation after allocation
