#include "symbtab.hh"

SymbTabEntry::SymbTabEntry() {
    size = 0;
    offset = 0;
    symbtab = nullptr;
    is_struct_global_entry = false;
    fn_decl = nullptr;
    print_original = false;
}

SymbTabEntry::SymbTabEntry(string name, string type_st, string scope, int size, int offset, Type* type_ptr, SymbTab* st) {
    this->name = name;
    this->type_symtab = type_st;
    this->scope = scope;
    this->size = size;
    this->offset = offset;
    this->type_ptr = type_ptr;
    this->symbtab = st;
    is_struct_global_entry = false;
    print_original = false;
}

void SymbTabEntry::print_list() {
    cout << "[";
    cout << "\"" << name << "\"" << ", ";
    cout << "\"" << type_symtab << "\"" << ", ";
    cout << "\"" << scope << "\"" << ", ";
    cout << size << ", ";
    if (is_struct_global_entry) cout << "\"-\"" << ", ";
    else cout << offset << ", ";
    cout << "\"" << (print_original ? original_type_ptr->get_type() : type_ptr->get_type()) << "\"";
    cout << "]" << endl;
}

void SymbTab::print() {
    cout << "[" << endl;
    for(auto it=Entries.begin(); it!=Entries.end(); it++) {
        it->second.print_list();
        it++;
        if (it!=Entries.end()) cout << "," << endl;
        it--;
    }
    cout << "]" << endl;
}