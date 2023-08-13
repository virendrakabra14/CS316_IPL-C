#ifndef __AST__
#define __AST__

#include <iostream>
#include <vector>
#include <string>
#include <map>
#include "type.hh"
#include "symbtab.hh"
using namespace std;

extern SymbTab globalST;
extern SymbTab* curr_fn_ST_ptr;
extern map<string, int> map_string_label;
extern int curr_stack_offset;
extern string indent;
extern int next_label;
extern bool print_asm_comments;

extern string curr_fn_name;
extern int curr_fn_leave_ret_label;

enum Node_Type {
    ABSTRACT_ASTNODE,
    STATEMENT_ASTNODE,
    EXP_ASTNODE,
    REF_ASTNODE,
    EMPTY_ASTNODE,
    SEQ_ASTNODE,
    ASSIGNS_ASTNODE,
    RETURN_ASTNODE,
    PROCCALL_ASTNODE,
    IF_ASTNODE,
    WHILE_ASTNODE,
    FOR_ASTNODE,
    OP_BINARY_ASTNODE,
    OP_UNARY_ASTNODE,
    ASSIGNE_ASTNODE,
    FUNCALL_ASTNODE,
    FLOATCONST_ASTNODE,
    INTCONST_ASTNODE,
    STRING_ASTNODE,
    IDENTIFIER_ASTNODE,
    MEMBER_ASTNODE,
    ARROW_ASTNODE,
    ARRAYREF_ASTNODE
};

class abstract_astnode {
public:
    Node_Type node_type;
    Type type;
    int stack_offset;
    bool to_dereference;
    int member_start_offset;

    virtual void print() = 0;
    virtual void gencode() = 0;
    abstract_astnode();
};

class statement_astnode: public abstract_astnode {
public:
    statement_astnode();
};

class exp_astnode: public abstract_astnode {
public:
    exp_astnode();
};

class ref_astnode: public exp_astnode {
public:
    ref_astnode();
};

class empty_astnode: public statement_astnode {
public:
    empty_astnode();
    virtual void print();
    virtual void gencode();
};

class seq_astnode: public statement_astnode {
public:
    vector<statement_astnode*> vec_stmt_astnode;

    seq_astnode();
    virtual void print();
    virtual void gencode();
};

class assignS_astnode: public statement_astnode {
public:
    exp_astnode *left, *right;

    assignS_astnode();
    assignS_astnode(exp_astnode* left, exp_astnode* right);
    virtual void print();
    virtual void gencode();
};

class return_astnode: public statement_astnode {
public:
    exp_astnode *ret;

    return_astnode();
    return_astnode(exp_astnode* ret);
    virtual void print();
    virtual void gencode();
};

class proccall_astnode: public statement_astnode {
public:
    vector<exp_astnode*> vec_exp_astnode;

    proccall_astnode();
    virtual void print();
    virtual void gencode();
};

class if_astnode: public statement_astnode {
public:
    exp_astnode* cond;
    statement_astnode *then_node, *else_node;

    if_astnode();
    if_astnode(exp_astnode* cond, statement_astnode* then_node, statement_astnode* else_node);
    virtual void print();
    virtual void gencode();
};

class while_astnode: public statement_astnode {
public:
    exp_astnode* cond;
    statement_astnode* stmt;

    while_astnode();
    while_astnode(exp_astnode* cond, statement_astnode* stmt);
    virtual void print();
    virtual void gencode();
};

class for_astnode: public statement_astnode {
public:
    exp_astnode *init, *guard, *step;
    statement_astnode* body;

    for_astnode();
    for_astnode(exp_astnode* init, exp_astnode* guard, exp_astnode* step, statement_astnode* body);
    virtual void print();
    virtual void gencode();
};

class op_binary_astnode: public exp_astnode {
public:
    string op;
    exp_astnode *left, *right;

    op_binary_astnode();
    op_binary_astnode(string op, exp_astnode* left, exp_astnode* right);
    virtual void print();
    virtual void gencode();
};

class op_unary_astnode: public exp_astnode {
public:
    string op;
    exp_astnode* child;

    op_unary_astnode();
    op_unary_astnode(string op, exp_astnode* child);
    virtual void print();
    virtual void gencode();
};

class assignE_astnode: public exp_astnode {
public:
    exp_astnode *left, *right;

    assignE_astnode();
    assignE_astnode(exp_astnode* left, exp_astnode* right);
    virtual void print();
    virtual void gencode();
};

class funcall_astnode: public exp_astnode {
public:
    vector<exp_astnode*> vec_exp_astnode;

    funcall_astnode();
    virtual void print();
    virtual void gencode();
};

class floatconst_astnode: public exp_astnode {
public:
    float f;

    floatconst_astnode();
    floatconst_astnode(float f);
    virtual void print();
    virtual void gencode() {};
};

class intconst_astnode: public exp_astnode {
public:
    int in;

    intconst_astnode();
    intconst_astnode(int in);
    virtual void print();
    virtual void gencode();
};

class string_astnode: public exp_astnode {
public:
    string s;

    string_astnode();
    string_astnode(string s);
    virtual void print();
    virtual void gencode() {};
};

class identifier_astnode: public ref_astnode {
public:
    string s;

    identifier_astnode();
    identifier_astnode(string s);
    virtual void print();
    virtual void gencode();
};

class member_astnode: public ref_astnode {
public:
    exp_astnode* struct_name;
    identifier_astnode* field;

    member_astnode();
    member_astnode(exp_astnode* struct_name, identifier_astnode* field);
    virtual void print();
    virtual void gencode();
};

class arrow_astnode: public ref_astnode {
public:
    exp_astnode* pointer;
    identifier_astnode* field;

    arrow_astnode();
    arrow_astnode(exp_astnode* pointer, identifier_astnode* field);
    virtual void print();
    virtual void gencode();
};

class arrayref_astnode: public ref_astnode {
public:
    exp_astnode *array, *index;

    arrayref_astnode();
    arrayref_astnode(exp_astnode* array, exp_astnode* index);
    virtual void print();
    virtual void gencode();
};


// bool is_compatible(Type* t1, Type* t2);
pair<bool,exp_astnode*> compatible_convert(exp_astnode* node, Type* type_ptr);

#endif