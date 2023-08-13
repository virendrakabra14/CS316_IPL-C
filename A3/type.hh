#ifndef __TYPE__
#define __TYPE__

#include <vector>
#include <string>
#include <iostream>
#include <cstdlib>
using namespace std;

#define DEFAULT_SIZE 4

class Type {
public:
    string base_type;       // e.g. "int" or "struct a"
    int base_type_size;

    int stars;

    /**
     * e.g. {x+1,1} in a[x+1][1]
     * but since this class is mainly
     * used for declarations,
     * most would be int's only
    */
    vector<string> arr_indices;

    /**
     * e.g. 1 in int*(*)[5]
     * result of int* x[5]; &x
     */
    int stars_brackets;
    
    /**
     * &(.) - ""
     * x in int x[5]; - "unmodifiable" [lvalue, but not modifiable]
     * *&*&a in int a; - "modifiable"
     */
    string lvalue;

    bool was_array_before_decay;
    
    Type();
    Type(string base);
    string get_type();
    int get_size();
    bool is_array();
    bool is_pointer();
    bool is_array_or_pointer();
    void decay_array_to_pointer();
    bool dereference();     // dereference, and possibly modify lvalue
};


// Other classes, not used in ASTs

class decl_class: public Type {
    // used for declarators and parameters
    // declarators don't use the base_type field
public:
    string instance_name;   // "x" in `int x;`
};

class decln_class {
    // for declarations
public:
    string base_type;
    vector<decl_class*> decls;
};

class fun_declarator_class {
public:
    string fun_name;
    vector<decl_class*>* params;
};

#endif