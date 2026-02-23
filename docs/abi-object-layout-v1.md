# Nex Object Layout v1

Object layout is subobject composition:

`[ObjHeader | SharedBases... | BaseSubobject(B) | BaseSubobject(C) | DerivedFields...]`

- deterministic base order
- one vtable pointer per class subobject
- metadata records size, align, pointer map, base offsets, vtable shape
