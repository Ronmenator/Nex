#ifndef NEX_RUNTIME_ABI_H
#define NEX_RUNTIME_ABI_H

typedef struct NexType NexType;
typedef struct NexObj NexObj;

NexObj* nex_gc_alloc(const NexType* type_desc, unsigned int size);
void    nex_gc_collect(void);
void    nex_gc_safepoint(void);
void    nex_gc_write_barrier(NexObj* obj);
void    nex_throw(NexObj* ex) __attribute__((noreturn));
NexObj* nex_new_exception(const char* msg);

NexObj* nex_var_from_i64(long long v);
NexObj* nex_var_from_obj(NexObj* obj);
NexObj* nex_var_add(NexObj* lhs, NexObj* rhs);
NexObj* nex_var_eq(NexObj* lhs, NexObj* rhs);
NexObj* nex_var_invoke_member(NexObj* recv, NexObj* name, NexObj** args, unsigned int argc);
const NexType* nex_var_typeof(NexObj* value);

#endif
