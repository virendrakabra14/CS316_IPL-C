#ifndef __SYMBTAB__
#define __SYMBTAB__

#include <iostream>
#include <map>
#include "type.hh"
using namespace std;

class SymbTab;

class SymbTabEntry {
public:
    string name, type_symtab, scope;
    int size, offset;
    Type* type_ptr;
    Type* original_type_ptr;
    SymbTab* symbtab;
    bool is_struct_global_entry;
    fun_declarator_class* fn_decl;
    bool print_original;

    SymbTabEntry();
    SymbTabEntry(string name, string type_st, string scope, int size, int offset, Type* type_ptr, SymbTab* st);
    void print_list();
};

class SymbTab {
public:
    map<string,SymbTabEntry> Entries;           // used in global STs (see main.cpp)

    SymbTab() : Entries() {}   // empty map
    void print();
};

#endif