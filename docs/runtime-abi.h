#ifndef AURUM_RUNTIME_ABI_H
#define AURUM_RUNTIME_ABI_H

typedef struct AurType AurType;
typedef struct AurObj AurObj;

AurObj* aur_gc_alloc(const AurType* type_desc, unsigned int size);
void    aur_gc_collect(void);
void    aur_gc_safepoint(void);
void    aur_gc_write_barrier(AurObj* obj);
void    aur_throw(AurObj* ex) __attribute__((noreturn));
AurObj* aur_new_exception(const char* msg);

AurObj* aur_var_from_i64(long long v);
AurObj* aur_var_from_obj(AurObj* obj);
AurObj* aur_var_add(AurObj* lhs, AurObj* rhs);
AurObj* aur_var_eq(AurObj* lhs, AurObj* rhs);
AurObj* aur_var_invoke_member(AurObj* recv, AurObj* name, AurObj** args, unsigned int argc);
const AurType* aur_var_typeof(AurObj* value);

#endif

