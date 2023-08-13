#include "ast.hh"

void assignment_gencode(exp_astnode* left, exp_astnode* right) {
    // helper function to perform assignment

    left->gencode();
    right->gencode();

    int total_size = DEFAULT_SIZE;      // see for arrays (need them decayed)

    // if it's a struct (and not an array/ptr), get size from globalST
    string right_type = right->type.get_type();
    if(right->type.is_array_or_pointer() == 0 && right_type.find("struct ") == 0) {
        total_size = globalST.Entries.find(right_type)->second.size;
    }

    cout << "# assignment: type, size " << right_type << ' ' << total_size << endl;

    /**
     * following gcc code:
     * 
     * 1. obj = obj         // repeat for entire size: right to eax, eax to left
     * 2. obj = *ptr        // load ptr in eax, then repeat: move i(%eax) to edx, edx to (left_top+i)(%ebp)
     * 3. *ptr = obj        // load ptr in eax, repeat: (right_top+i)(%ebp) to edx, edx to i(%eax)
     * 4. *ptr1 = *ptr2     // load ptr1 in eax, load ptr2 in edx, repeat:
     *                      // i(%edx) to ecx, ecx to i(%eax)
    */

    if(left->to_dereference == 0 && right->to_dereference == 0) {
        // 1.
        for(int i=0; i<total_size; i+=DEFAULT_SIZE) {
            cout << indent << "movl\t" << right->stack_offset + i << "(%ebp), %eax" << endl;  // eax = right
            cout << indent << "movl\t%eax, " << left->stack_offset + i << "(%ebp)" << endl;   // left = eax
        }
    }
    else if(left->to_dereference == 0 && right->to_dereference == 1) {
        // 2.
        cout << indent << "movl\t" << right->stack_offset << "(%ebp), %eax" << endl;  // eax = ptr
        for(int i=0; i<total_size; i+=DEFAULT_SIZE) {
            cout << indent << "movl\t" << right->member_start_offset + i << "(%eax), %edx" << endl;
            cout << indent << "movl\t%edx, " << left->stack_offset + i << "(%ebp)" << endl;
        }
    }
    else if(left->to_dereference == 1 && right->to_dereference == 0) {
        // 3.
        cout << indent << "movl\t" << left->stack_offset << "(%ebp), %eax" << endl;  // eax = ptr
        for(int i=0; i<total_size; i+=DEFAULT_SIZE) {
            cout << indent << "movl\t" << right->stack_offset + i << "(%ebp), %edx" << endl;
            cout << indent << "movl\t%edx, " << left->member_start_offset + i << "(%eax)" << endl;
        }
    }
    else if(left->to_dereference == 1 && right->to_dereference == 1) {
        // 4.
        cout << indent << "movl\t" << left->stack_offset << "(%ebp), %eax" << endl;   // eax = ptr1
        cout << indent << "movl\t" << right->stack_offset << "(%ebp), %edx" << endl;  // edx = ptr2
        for(int i=0; i<total_size; i+=DEFAULT_SIZE) {
            cout << indent << "movl\t" << right->member_start_offset + i << "(%edx), %ecx" << endl;
            cout << indent << "movl\t%ecx, " << left->member_start_offset + i << "(%eax)" << endl;
        }
    }
}

void call_gencode(vector<exp_astnode*>& vec_exp_astnode) {
    string fn_name = static_cast<identifier_astnode*>(vec_exp_astnode[0])->s;

    if(fn_name == "printf") {
        // evaluate and push params (reverse order)
        for(int i=vec_exp_astnode.size()-1; i>=2; i--) {
            vec_exp_astnode[i]->gencode();
        }
        for(int i=vec_exp_astnode.size()-1; i>=2; i--) {
            if(vec_exp_astnode[i]->to_dereference) {
                // dereference and push
                cout << indent << "movl\t" << vec_exp_astnode[i]->stack_offset << "(%ebp), %eax" << endl;
                cout << indent << "pushl\t" << vec_exp_astnode[i]->member_start_offset << "(%eax)" << endl;
            }
            else {
                cout << indent << "pushl\t" << vec_exp_astnode[i]->stack_offset << "(%ebp)" << endl;
            }
        }
        cout << indent << "pushl\t$.LC" << map_string_label[static_cast<string_astnode*>(vec_exp_astnode[1])->s] << endl;
        curr_stack_offset -= DEFAULT_SIZE * (vec_exp_astnode.size()-1);

        cout << indent << "call\tprintf" << endl;

        // reclaim space of parameters; for stringconst, it is an address from .rodata [SO: 5325326]
        // return value in eax
        cout << indent << "addl\t$" << DEFAULT_SIZE * (vec_exp_astnode.size()-1) << ", %esp" << endl;
        curr_stack_offset += DEFAULT_SIZE * (vec_exp_astnode.size()-1);
    }
    else {

        // evaluate params
        for(int i=1; i<(int)vec_exp_astnode.size(); i++) {
            vec_exp_astnode[i]->gencode();
        }

        // create space for returned value
        string ret_type = globalST.Entries.find(fn_name)->second.type_ptr->get_type();
        cout << "# fn_name, ret_type: " << fn_name << ", " << ret_type << endl;
        int ret_val_size = 0;
        if(ret_type != "void") {
            if(ret_type == "int" || ret_type == "float") {
                ret_val_size = DEFAULT_SIZE;
            }
            else {
                // struct
                ret_val_size = globalST.Entries.find(ret_type)->second.size;
            }
            cout << indent << "subl\t$" << ret_val_size << ", %esp" << endl;
            curr_stack_offset -= ret_val_size;
        }

        // push params (earlier param pushed first)
        int total_params_size = 0;      // this does NOT include space used for params' evaluation
        for(int i=1; i<(int)vec_exp_astnode.size(); i++) {
            // if it's a struct (and not an array/ptr), get size from globalST
            int param_size = DEFAULT_SIZE;
            string param_type = vec_exp_astnode[i]->type.get_type();
            if(vec_exp_astnode[i]->type.is_array_or_pointer() == 0 && param_type.find("struct ") == 0) {
                param_size = globalST.Entries.find(param_type)->second.size;
            }
            // push stack members in reverse order (so that callee can access in correct order)
            if(vec_exp_astnode[i]->type.was_array_before_decay) {
                if(vec_exp_astnode[i]->to_dereference == 1) {
                    cout << "# param: array and deref " << param_type << endl;
                    cout << indent << "pushl\t" << vec_exp_astnode[i]->stack_offset << "(%ebp)" << endl;
                }
                else {
                    cout << "# param: array no deref " << param_type << endl;
                    cout << indent << "leal\t" << vec_exp_astnode[i]->stack_offset << "(%ebp), %eax" << endl;
                    cout << indent << "pushl\t%eax" << endl;
                }
            }
            else if(vec_exp_astnode[i]->to_dereference == 1) {
                // dereference and push
                cout << indent << "movl\t" << vec_exp_astnode[i]->stack_offset << "(%ebp), %eax" << endl;
                for(int j=param_size-DEFAULT_SIZE; j>=0; j-=DEFAULT_SIZE) {
                    cout << indent << "pushl\t" << j + vec_exp_astnode[i]->member_start_offset << "(%eax)" << endl;
                }
            }
            else {
                for(int j=param_size-DEFAULT_SIZE; j>=0; j-=DEFAULT_SIZE) {
                    cout << indent << "pushl\t" << vec_exp_astnode[i]->stack_offset + j << "(%ebp)" << endl;
                }
            }
            total_params_size += param_size;
        }
        int stack_offset_without_params = curr_stack_offset;    // equivalent to "ret_val_offset"
                                                                // that is, start of return value
        curr_stack_offset -= total_params_size;

        cout << indent << "call\t" << fn_name << endl;

        // reclaim space used by parameters and callee
        // (could also use `addl`, as space used by callee is
        // reclaimed by `leave` instruction: `esp=ebp` and `pop ebp`)
        cout << indent << "leal\t" << stack_offset_without_params << "(%ebp), %esp" << endl;
        curr_stack_offset = stack_offset_without_params;
    }
}

void dereference_and_push(abstract_astnode* node_ptr) {
    cout << indent << "movl\t" << node_ptr->stack_offset << "(%ebp), %eax" << endl;
    cout << indent << "pushl\t(%eax)" << endl;
    curr_stack_offset -= DEFAULT_SIZE;
    node_ptr->stack_offset = curr_stack_offset;
}

// int get_dereferenced_type_size(abstract_astnode* node_ptr) {
//     int type_size = DEFAULT_SIZE;       // for int*, struct s**, etc. : size=4
//     Type tmp_type = node_ptr->type;
//     tmp_type.dereference();
//     if(tmp_type.is_array_or_pointer() == 0 && tmp_type.base_type.find("struct ") == 0) {
//         // struct size only for struct (single) pointer (or decayed array)
//         type_size = globalST.Entries.find(node_ptr->type.base_type)->second.size;
//     }
//     cout << "# array elem size " << type_size << endl;
// }

abstract_astnode::abstract_astnode() : type() {
    node_type = ABSTRACT_ASTNODE;
    this->to_dereference = 0;
    this->member_start_offset = 0;
}

statement_astnode::statement_astnode() {
    node_type = STATEMENT_ASTNODE;
}

exp_astnode::exp_astnode() {
    node_type = EXP_ASTNODE;
}

ref_astnode::ref_astnode() {
    node_type = REF_ASTNODE;
}

empty_astnode::empty_astnode() {
    node_type = EMPTY_ASTNODE;
}

seq_astnode::seq_astnode() {
    node_type = SEQ_ASTNODE;
    vec_stmt_astnode.clear();
}

assignS_astnode::assignS_astnode() {
    node_type = ASSIGNS_ASTNODE;
}

return_astnode::return_astnode() {
    node_type = RETURN_ASTNODE;
}

proccall_astnode::proccall_astnode() {
    node_type = PROCCALL_ASTNODE;
    vec_exp_astnode.clear();
}

if_astnode::if_astnode() {
    node_type = IF_ASTNODE;
}

while_astnode::while_astnode() {
    node_type = WHILE_ASTNODE;
}

for_astnode::for_astnode() {
    node_type = FOR_ASTNODE;
}

op_binary_astnode::op_binary_astnode() {
    node_type = OP_BINARY_ASTNODE;
}

op_unary_astnode::op_unary_astnode() {
    node_type = OP_UNARY_ASTNODE;
}

assignE_astnode::assignE_astnode() {
    node_type = ASSIGNE_ASTNODE;
}

funcall_astnode::funcall_astnode() {
    node_type = FUNCALL_ASTNODE;
    vec_exp_astnode.clear();
}

floatconst_astnode::floatconst_astnode() {
    node_type = FLOATCONST_ASTNODE;
}

intconst_astnode::intconst_astnode() {
    node_type = INTCONST_ASTNODE;
}

string_astnode::string_astnode() {
    node_type = STRING_ASTNODE;
}

identifier_astnode::identifier_astnode() {
    node_type = IDENTIFIER_ASTNODE;
}

member_astnode::member_astnode() {
    node_type = MEMBER_ASTNODE;
}

arrow_astnode::arrow_astnode() {
    node_type = ARROW_ASTNODE;
}

arrayref_astnode::arrayref_astnode() {
    node_type = ARRAYREF_ASTNODE;
}

void empty_astnode::print() {
    cout << "\"empty\"" << endl;
}

void empty_astnode::gencode() {
    if(print_asm_comments) cout << "#empty_astnode" << endl;
}

void seq_astnode::print() {
    cout << "{" << endl;
    cout << "\"seq\":" << endl;
    cout << "[" << endl;
    for(size_t i=0; i<vec_stmt_astnode.size(); i++) {
        vec_stmt_astnode[i]->print();
        if(i!=vec_stmt_astnode.size()-1) {
            cout << "," << endl;
        }
        cout << endl;
    }
    cout << "]" << endl;
    cout << "}" << endl;
}

void seq_astnode::gencode() {
    if(print_asm_comments) cout << "#seq_astnode" << endl;

    for(auto&& e:this->vec_stmt_astnode) {
        e->gencode();
    }
}

assignS_astnode::assignS_astnode(exp_astnode* left, exp_astnode* right) {
    node_type = ASSIGNS_ASTNODE;
    this->left = left;
    this->right = right;
}

void assignS_astnode::print() {
    cout << "{" << endl;
    cout << "\"assignS\":" << endl;
    cout << "{" << endl;
    cout << "\"left\":" << endl;
    left->print();
    cout << "," << endl;
    cout << "\"right\":" << endl;
    right->print();
    cout << "}" << endl;
    cout << "}" << endl;
}

void assignS_astnode::gencode() {
    if(print_asm_comments) cout << "#assignS_astnode" << endl;
    assignment_gencode(this->left, this->right);
}

void return_astnode::print() {
    cout << "{" << endl;
    cout << "\"return\":" << endl;
    ret->print();
    cout << "}" << endl;
}

return_astnode::return_astnode(exp_astnode* ret) {
    node_type = RETURN_ASTNODE;
    this->ret = ret;
}

void return_astnode::gencode() {
    this->ret->gencode();

    if(curr_fn_name != "main") {

        // store return value for caller to use

        // compute params size
        int curr_fn_params_size = 0;
        for(auto&& e: curr_fn_ST_ptr->Entries) {
            if(e.second.scope == "param") {
                curr_fn_params_size += e.second.size;
            }
        }

        // starting offset of ret val wrt ebp:
        // (space for ebp) + (space for ret addr) + (space for params)
        int ret_val_offset = DEFAULT_SIZE + DEFAULT_SIZE + curr_fn_params_size;
        cout << "# ret_val_offset " << ret_val_offset << endl;

        // get return value size
        string ret_type = globalST.Entries.find(curr_fn_name)->second.type_ptr->get_type();
        int curr_fn_ret_val_size = 0;
        if(ret_type != "void") {
            if(ret_type == "int" || ret_type == "float") {
                curr_fn_ret_val_size = DEFAULT_SIZE;
            }
            else {
                // struct
                curr_fn_ret_val_size = globalST.Entries.find(ret_type)->second.size;
            }
        }

        // copy `ret` result into ret val space

        if(this->ret->to_dereference == 1) {
            cout << indent << "movl\t" << this->ret->stack_offset << "(%ebp), %eax" << endl;
            for(int i=0; i<curr_fn_ret_val_size; i+=DEFAULT_SIZE) {
                cout << indent << "movl\t" << i << "(%eax), %edx" << endl;
                cout << indent << "movl\t%edx, " << ret_val_offset+i << "(%ebp)" << endl;
            }
        }
        else {
            for(int i=0; i<curr_fn_ret_val_size; i+=DEFAULT_SIZE) {
                cout << indent << "movl\t" << this->ret->stack_offset+i << "(%ebp), %eax" << endl;
                cout << indent << "movl\t%eax, " << ret_val_offset+i << "(%ebp)" << endl;
            }
        }

    }

    cout << indent << "jmp\t.L" << curr_fn_leave_ret_label << endl;
}

void proccall_astnode::print() {            // need to check size>0?
    cout << "{" << endl;
    cout << "\"proccall\":" << endl;
    cout << "{" << endl;
    cout << "\"fname\":" << endl;
    vec_exp_astnode[0]->print();
    cout << "," << endl;
    cout << "\"params\":" << endl;
    cout << "[" << endl;
    for(size_t i=1; i<vec_exp_astnode.size(); i++) {
        vec_exp_astnode[i]->print();
        if(i!=vec_exp_astnode.size()-1) {
            cout << "," << endl;
        }
    }
    cout << "]" << endl;
    cout << "}" << endl;
    cout << "}" << endl;
}

void proccall_astnode::gencode() {
    if(print_asm_comments) cout << "#proccall_astnode" << endl;
    call_gencode(this->vec_exp_astnode);
    this->stack_offset = curr_stack_offset;
}

void if_astnode::print() {
    cout << "{" << endl;
    cout << "\"if\":" << endl;
    cout << "{" << endl;
    cout << "\"cond\":" << endl;
    cond->print();
    cout << "," << endl;
    cout << "\"then\":" << endl;
    then_node->print();
    cout << "," << endl;
    cout << "\"else\":" << endl;
    else_node->print();
    cout << "}" << endl;
    cout << "}" << endl;
}

if_astnode::if_astnode(exp_astnode* cond, statement_astnode* then_node, statement_astnode* else_node) {
    node_type = IF_ASTNODE;
    this->cond = cond;
    this->then_node = then_node;
    this->else_node = else_node;
}

void if_astnode::gencode() {
    this->cond->gencode();

    int label1, label2;
    label1 = next_label;
    label2 = next_label+1;
    next_label += 2;

    if(this->cond->to_dereference) {
        cout << indent << "movl\t" << this->cond->stack_offset << "(%ebp), %eax" << endl;
        cout << indent << "cmpl\t$0, (%eax)" << endl;
    }
    else {
        cout << indent << "cmpl\t$0, " << this->cond->stack_offset << "(%ebp)" << endl;
    }
    cout << indent << "je\t.L" << label1 << endl;
    
    // fall (cond evals to true)
    this->then_node->gencode();
    cout << indent << "jmp\t.L" << label2 << endl;
    
    // cond evals to false
    cout << ".L" << label1 << ":" << endl;
    cout << indent << "leal\t" << curr_stack_offset << "(%ebp), %esp" << endl;
    this->else_node->gencode();

    cout << ".L" << label2 << ":" << endl;

    cout << indent << "leal\t" << curr_stack_offset << "(%ebp), %esp" << endl;

    // space for both then_node and else_node on stack
    // (maintained using curr_stack_offset and leal)
}

void while_astnode::print() {
    cout << "{" << endl;
    cout << "\"while\":" << endl;
    cout << "{" << endl;
    cout << "\"cond\":" << endl;
    cond->print();
    cout << "," << endl;
    cout << "\"stmt\":" << endl;
    stmt->print();
    cout << "}" << endl;
    cout << "}" << endl;
}

while_astnode::while_astnode(exp_astnode* cond, statement_astnode* stmt) {
    node_type = WHILE_ASTNODE;
    this->cond = cond;
    this->stmt = stmt;
}

void while_astnode::gencode() {

    int label1, label2;
    label1 = next_label;
    label2 = next_label+1;
    next_label += 2;

    // condition
    cout << ".L" << label1 << ":" << endl;

    // in each loop iteration, below leal increases esp (decreases stack size)
    // this is OK, since there are only temporaries
    cout << indent << "leal\t" << curr_stack_offset << "(%ebp), %esp" << endl;
    
    this->cond->gencode();
    if(this->cond->to_dereference) {
        cout << indent << "movl\t" << this->cond->stack_offset << "(%ebp), %eax" << endl;
        cout << indent << "cmpl\t$0, (%eax)" << endl;
    }
    else {
        cout << indent << "cmpl\t$0, " << this->cond->stack_offset << "(%ebp)" << endl;
    }
    cout << indent << "je\t.L" << label2 << endl;   // get out of loop

    // fall to body
    // cout << indent << "leal\t" << curr_stack_offset << "(%ebp), %esp" << endl;  // redundant?
    this->stmt->gencode();
    cout << indent << "jmp\t.L" << label1 << endl;  // loop back

    // next block
    cout << ".L" << label2 << ":" << endl;

    // leal needed, as body might not be executed, but pushes by gencode take up space
    cout << indent << "leal\t" << curr_stack_offset << "(%ebp), %esp" << endl;

}

void for_astnode::print() {
    cout << "{" << endl;
    cout << "\"for\":" << endl;
    cout << "{" << endl;
    cout << "\"init\":" << endl;
    init->print();
    cout << "," << endl;
    cout << "\"guard\":" << endl;
    guard->print();
    cout << "," << endl;
    cout << "\"step\":" << endl;
    step->print();
    cout << "," << endl;
    cout << "\"body\":" << endl;
    body->print();
    cout << "}" << endl;
    cout << "}" << endl;
}

for_astnode::for_astnode(exp_astnode* init, exp_astnode* guard, exp_astnode* step, statement_astnode* body) {
    node_type = FOR_ASTNODE;
    this->init = init;
    this->guard = guard;
    this->step = step;
    this->body = body;
}

void for_astnode::gencode() {
    this->init->gencode();

    // now similar to while loop

    int label1, label2;
    label1 = next_label;
    label2 = next_label+1;
    next_label += 2;

    // guard
    cout << ".L" << label1 << ":" << endl;
    // similar to `while`, leal at the start of iteration
    cout << indent << "leal\t" << curr_stack_offset << "(%ebp), %esp" << endl;
    
    this->guard->gencode();

    if(this->guard->to_dereference) {
        cout << indent << "movl\t" << this->guard->stack_offset << "(%ebp), %eax" << endl;
        cout << indent << "cmpl\t$0, (%eax)" << endl;
    }
    else {
        cout << indent << "cmpl\t$0, " << this->guard->stack_offset << "(%ebp)" << endl;
    }
    cout << indent << "je\t.L" << label2 << endl;   // get out of loop

    // fall to body and step
    // (leal here is redundant)
    this->body->gencode();
    this->step->gencode();
    cout << indent << "jmp\t.L" << label1 << endl;  // loop back

    // next block
    cout << ".L" << label2 << ":" << endl;

    // leal needed, as body/step might not be executed, but pushes by gencode take up space
    cout << indent << "leal\t" << curr_stack_offset << "(%ebp), %esp" << endl;
}

void op_binary_astnode::print() {
    cout << "{" << endl;
    cout << "\"op_binary\":" << endl;
    cout << "{" << endl;
    cout << "\"op\":" << endl;
    cout << "\"" << op << "\"" << endl;
    cout << "," << endl;
    cout << "\"left\":" << endl;
    left->print();
    cout << "," << endl;
    cout << "\"right\":" << endl;
    right->print();
    cout << "}" << endl;
    cout << "}" << endl;
}

op_binary_astnode::op_binary_astnode(string op, exp_astnode* left, exp_astnode* right) {
    node_type = OP_BINARY_ASTNODE;
    this->op = op;
    this->left = left;
    this->right = right;
}

void op_binary_astnode::gencode() {
    if(print_asm_comments) cout << "#op_binary_astnode: " << this->left->type.get_type() << " "
                                << this->op << " " << this->right->type.get_type() << endl;

    // if int+ptr, swap to get left=ptr, right=int
    if(this->op == "PLUS_INT") {
        if(this->left->type.get_type() == "int" && this->right->type.is_array_or_pointer()) {
            auto tmp = this->left;
            this->left = this->right;
            this->right = tmp;
        }
    }

    // gencode for left operand
    if(print_asm_comments) cout << "#left" << endl;
    this->left->gencode();
    if(print_asm_comments) cout << "#left done" << endl;

    if(this->left->to_dereference == 1) {
        dereference_and_push(this->left);
    }

    // evaluate right if not short-circuit
    string right_operand;
    if(this->op != "AND_OP" && this->op != "OR_OP") {
        this->right->gencode();

        if(this->right->to_dereference == 1) {
            dereference_and_push(this->right);
        }

        if(this->op == "PLUS_INT" || this->op == "MINUS_INT") {
            if(this->left->type.is_array_or_pointer()) {
                cout << "# " << this->op << " imull" << endl;
                cout << indent << "movl\t" << this->right->stack_offset << "(%ebp), %eax" << endl;
                
                // int type_size = DEFAULT_SIZE;       // for int*, struct s**, etc. : size=4
                Type tmp_type = this->left->type;
                tmp_type.dereference();
                // if(tmp_type.is_array_or_pointer() == 0 && tmp_type.base_type.find("struct ") == 0) {
                //     // struct size only for struct (single) pointer (or decayed array)
                //     type_size = globalST.Entries.find(this->left->type.base_type)->second.size;
                // }
                cout << indent << "imull\t$" << tmp_type.get_size() << ", %eax" << endl;
                cout << "# mult with size " << tmp_type.get_size() << endl;
                cout << indent << "pushl\t%eax" << endl;
                
                curr_stack_offset -= DEFAULT_SIZE;
                this->right->stack_offset = curr_stack_offset;
            }
        }

        right_operand = to_string(this->right->stack_offset) + "(%ebp)";
    }

    // move left operand into eax
    // NOTE: this must be done AFTER gencode of right (if applicable)
    if(this->left->type.was_array_before_decay) {
        cout << indent << "leal\t" << this->left->stack_offset << "(%ebp), %eax" << endl;
    }
    else {
        cout << indent << "movl\t" << this->left->stack_offset << "(%ebp), %eax" << endl;
    }

    if(this->op == "PLUS_INT") {
        cout << indent << "addl\t" << right_operand << ", %eax" << endl;
    }
    else if(this->op == "MINUS_INT") {
        cout << indent << "subl\t" << right_operand << ", %eax" << endl;
    }
    else if(this->op == "MULT_INT") {
        cout << indent << "imull\t" << right_operand << ", %eax" << endl;
    }
    else if(this->op == "DIV_INT") {
        // stackoverflow.com/questions/39658992
        // left is dividend, put in eax above

        cout << indent << "cltd" << endl;       // signed-extension of eax into edx:eax
        cout << indent << "idivl\t" << this->right->stack_offset << "(%ebp)" << endl;   // divisor
        // quotient in eax, remainder in edx
    }
    else if(
        this->op == "LT_OP_INT" || this->op == "LE_OP_INT" ||
        this->op == "GT_OP_INT" || this->op == "GE_OP_INT" ||
        this->op == "EQ_OP_INT" || this->op == "NE_OP_INT"
    ) {

        string jmp_inst_true = "";       // instruction to jump if (left relop right) is true
        if (this->op == "LT_OP_INT") {
            jmp_inst_true = "jl";
        }
        else if (this->op == "LE_OP_INT") {
            jmp_inst_true = "jle";
        }
        else if (this->op == "GT_OP_INT") {
            jmp_inst_true = "jg";
        }
        else if (this->op == "GE_OP_INT") {
            jmp_inst_true = "jge";
        }
        else if(this->op == "EQ_OP_INT") {
            jmp_inst_true = "je";
        }
        else if(this->op == "NE_OP_INT") {
            jmp_inst_true = "jne";
        }

        cout << indent << "cmpl\t" << right_operand << ", %eax" << endl;
        cout << indent << jmp_inst_true << "\t.L" << next_label << endl;     // (left relop right) is true
        
        // False branch: fall means !(left relop right), so set eax to 0
        cout << indent << "movl\t$0, %eax" << endl;
        cout << indent << "jmp\t.L" << next_label+1 << endl;
        
        // True branch: set eax to 1
        cout << ".L" << next_label << ":" << endl;
        cout << indent << "movl\t$1, %eax" << endl;

        // after setting eax to 0 or 1, both paths fall to .L{next_label+1}
        cout << ".L" << next_label+1 << ":" << endl;
        
        next_label += 2;
    }
    else if(this->op == "AND_OP" || this->op == "OR_OP") {

        // short-circuit-value: 0 for AND, 1 for OR
        int short_circuit_value = (this->op == "AND_OP" ? 0 : 1);
        // since comparing with 0, respective instructions are je and jne
        string short_circuit_inst = (this->op == "AND_OP" ? "je" : "jne");

        int label1, label2;
        label1 = next_label;        // for short-circuit
        label2 = next_label+1;      // for next code block
        next_label += 2;

        cout << indent << "cmpl\t$0, %eax" << endl;
        cout << indent << short_circuit_inst << "\t.L" << label1 << endl;   // left is short-circuit-value, so jump

        // next instruction (esp = ebp + curr_stack_offset) is to bring actual esp to the offset maintained here;
        // this is needed for AND_OP and OR_OP as only one branch executes, while we gencode for both of them
        // [so curr_stack_offset includes space for BOTH left and right; could do max(|left_offset|, |right_offset|) too]
        cout << indent << "leal\t" << curr_stack_offset << "(%ebp), %esp" << endl;

        // fall means left is not short-circuit-value, so evaluate right
        this->right->gencode();

        if(this->right->to_dereference == 1) {
            dereference_and_push(this->right);
        }

        cout << indent << "cmpl\t$0, " << this->right->stack_offset << "(%ebp)" << endl;
        cout << indent << short_circuit_inst << "\t.L" << label1 << endl; // right is short-circuit-value, so jump

        // fall means both are !(short-circuit-value), so set eax to !(short-circuit-value)
        // that is, x&&y (both 1) or x||y (both 0)
        cout << indent << "movl\t$" << !(short_circuit_value) << ", %eax" << endl;
        cout << indent << "jmp\t.L" << label2 << endl;
        
        // now, code for short-circuit branch (reach here by short-circuit or complete eval)
        // set eax to short-circuit-value
        cout << ".L" << label1 << ":" << endl;
        cout << indent << "movl\t$" << short_circuit_value << ", %eax" << endl;

        // after setting eax to 0 or 1, both paths fall to next code block
        cout << ".L" << label2 << ":" << endl;
        
    }

    // curr_stack_offset includes space for BOTH left and right
    cout << indent << "leal\t" << curr_stack_offset << "(%ebp), %esp" << endl;

    // push eax (result) onto stack
    cout << indent << "pushl\t%eax" << endl;
    curr_stack_offset -= DEFAULT_SIZE;
    this->stack_offset = curr_stack_offset;
}

void op_unary_astnode::print() {
    cout << "{" << endl;
    cout << "\"op_unary\":" << endl;
    cout << "{" << endl;
    cout << "\"op\":" << endl;
    cout << "\"" << op << "\"" << endl;
    cout << "," << endl;
    cout << "\"child\":" << endl;
    child->print();
    cout << "}" << endl;
    cout << "}" << endl;
}

op_unary_astnode::op_unary_astnode(string op, exp_astnode* child) {
    node_type = OP_UNARY_ASTNODE;
    this->op = op;
    this->child = child;
}

void op_unary_astnode::gencode() {
    if(print_asm_comments) cout << "#op_unary_astnode" << endl;

    this->child->gencode();

    if(this->child->to_dereference && this->op != "ADDRESS") {
        // NOTE: cannot use copy for ADDRESS - hence dealt separately
        cout << indent << "movl\t" << this->child->stack_offset << "(%ebp), %eax" << endl;
        cout << indent << "pushl\t(%eax)" << endl;
        curr_stack_offset -= DEFAULT_SIZE;
        this->child->stack_offset = curr_stack_offset;
    }

    if(this->op == "UMINUS") {
        cout << indent << "movl\t" << this->child->stack_offset << "(%ebp), %eax" << endl;
        cout << indent << "negl\t%eax" << endl;     // replaces value of operand with its 2's compl
    }
    else if(this->op == "NOT") {
        int label1, label2;
        label1 = next_label;
        label2 = next_label+1;
        next_label += 2;
        
        cout << indent << "cmpl\t$0, " << this->child->stack_offset << "(%ebp)" << endl;
        cout << indent << "je\t.L" << label1 << endl;

        cout << indent << "movl\t$0, %eax" << endl;     // fall means child != 0, so set eax to 0
        cout << indent << "jmp\t.L" << label2 << endl;
        
        cout << indent << ".L" << label1 << ":" << endl;
        cout << indent << "movl\t$1, %eax" << endl;     // child = 0, so set eax to 1
        
        cout << indent << ".L" << label2 << ":" << endl;
    }
    else if(this->op == "PP") {
        // move value into eax, and then increment in-place
        cout << indent << "movl\t" << this->child->stack_offset << "(%ebp), %eax" << endl;
        cout << indent << "addl\t$1, " << this->child->stack_offset << "(%ebp)" << endl;
    }
    else if(this->op == "ADDRESS") {
        if(this->child->to_dereference) {
            // movl
            cout << indent << "movl\t" << this->child->stack_offset << "(%ebp), %eax" << endl;
        }
        else {
            // leal
            cout << indent << "leal\t" << this->child->stack_offset << "(%ebp), %eax" << endl;
        }
    }
    else if(this->op == "DEREF") {
        // move (address) into eax, dereference while using
        // cannot deref and push, as it might be `*ptr = ...`
        cout << "# deref unary node" << endl;
        this->to_dereference = true;
        cout << indent << "movl\t" << this->child->stack_offset << "(%ebp), %eax" << endl;
    }

    // push result onto stack
    cout << indent << "pushl\t%eax" << endl;
    curr_stack_offset -= DEFAULT_SIZE;
    this->stack_offset = curr_stack_offset;
}

void assignE_astnode::print() {
    cout << "{" << endl;
    cout << "\"assignE\":" << endl;
    cout << "{" << endl;
    cout << "\"left\":" << endl;
    left->print();
    cout << "," << endl;
    cout << "\"right\":" << endl;
    right->print();
    cout << "}" << endl;
    cout << "}" << endl;
}

assignE_astnode::assignE_astnode(exp_astnode* left, exp_astnode* right) {
    node_type = ASSIGNE_ASTNODE;
    this->left = left;
    this->right = right;
}

void assignE_astnode::gencode() {
    if(print_asm_comments) cout << "#assignE_astnode" << endl;
    assignment_gencode(this->left, this->right);
}

void funcall_astnode::print() {             // similar to proccall, need to check size>0?
    cout << "{" << endl;
    cout << "\"funcall\":" << endl;
    cout << "{" << endl;
    cout << "\"fname\":" << endl;
    vec_exp_astnode[0]->print();
    cout << "," << endl;
    cout << "\"params\":" << endl;
    cout << "[" << endl;
    for(size_t i=1; i<vec_exp_astnode.size(); i++) {
        vec_exp_astnode[i]->print();
        if(i!=vec_exp_astnode.size()-1) {
            cout << "," << endl;
        }
    }
    cout << "]" << endl;
    cout << "}" << endl;
    cout << "}" << endl;
}

void funcall_astnode::gencode() {
    if(print_asm_comments) cout << "#funcall_astnode" << endl;

    call_gencode(this->vec_exp_astnode);
    // access returned value
    this->stack_offset = curr_stack_offset;
}

void floatconst_astnode::print() {
    cout << "{" << endl;
    cout << "\"floatconst\":" << endl;
    cout << f << endl;
    cout << "}" << endl;
}

floatconst_astnode::floatconst_astnode(float f) {
    node_type = FLOATCONST_ASTNODE;
    this->f = f;
}

void intconst_astnode::print() {
    cout << "{" << endl;
    cout << "\"intconst\":" << endl;
    cout << in << endl;
    cout << "}" << endl;
}

intconst_astnode::intconst_astnode(int in) {
    node_type = INTCONST_ASTNODE;
    this->in = in;
}

void intconst_astnode::gencode() {
    if(print_asm_comments) cout << "#intconst_astnode" << endl;

    cout << indent << "pushl\t$" << this->in << endl;
    curr_stack_offset -= DEFAULT_SIZE;
    this->stack_offset = curr_stack_offset;
}

void string_astnode::print() {
    cout << "{" << endl;
    cout << "\"stringconst\":" << endl;
    cout << s << endl;      // string_astnode created from STRING_LITERAL
                            // so it already has quotes
    cout << "}" << endl;
}

string_astnode::string_astnode(string s) {
    node_type = STRING_ASTNODE;
    this->s = s;
    map_string_label.insert({s, map_string_label.size()});
}

void identifier_astnode::print() {
    cout << "{" << endl;
    cout << "\"identifier\":" << endl;
    cout << "\"" << s << "\"" << endl;
    cout << "}" << endl;
}

identifier_astnode::identifier_astnode(string s) {
    node_type = IDENTIFIER_ASTNODE;
    this->s = s;
}

void identifier_astnode::gencode() {
    if(print_asm_comments) cout << "#identifier_astnode" << endl;

    auto it = curr_fn_ST_ptr->Entries.find(this->s);
    if(it != curr_fn_ST_ptr->Entries.end()) {
        this->stack_offset = it->second.offset;
    }
}

void member_astnode::print() {
    cout << "{" << endl;
    cout << "\"member\":" << endl;
    cout << "{" << endl;
    cout << "\"struct\":" << endl;
    struct_name->print();
    cout << "," << endl;
    cout << "\"field\":" << endl;
    field->print();
    cout << "}" << endl;
    cout << "}" << endl;
}

member_astnode::member_astnode(exp_astnode* struct_name, identifier_astnode* field) {
    node_type = MEMBER_ASTNODE;
    this->struct_name = struct_name;
    this->field = field;
}

void member_astnode::gencode() {
    this->struct_name->gencode();

    int member_offset;
    auto struct_global_it = globalST.Entries.find(this->struct_name->type.get_type());
    if(struct_global_it != globalST.Entries.end()) {
        auto struct_member_it = struct_global_it->second.symbtab->Entries.find(this->field->s);
        if(struct_member_it != struct_global_it->second.symbtab->Entries.end()) {
            member_offset = struct_member_it->second.offset;
        }
    }

    if(this->struct_name->to_dereference == 1) {
        this->to_dereference = true;
        cout << indent << "movl\t" << this->struct_name->stack_offset << "(%ebp), %eax" << endl;
        cout << indent << "addl\t$" << member_offset << ", %eax" << endl;
        cout << indent << "pushl\t%eax" << endl;
        curr_stack_offset -= DEFAULT_SIZE;
        this->stack_offset = curr_stack_offset;
    }
    else {
        this->stack_offset = this->struct_name->stack_offset + member_offset;
    }
}

void arrow_astnode::print() {
    cout << "{" << endl;
    cout << "\"arrow\":" << endl;
    cout << "{" << endl;
    cout << "\"pointer\":" << endl;
    pointer->print();
    cout << "," << endl;
    cout << "\"field\":" << endl;
    field->print();
    cout << "}" << endl;
    cout << "}" << endl;
}

arrow_astnode::arrow_astnode(exp_astnode* pointer, identifier_astnode* field) {
    node_type = ARROW_ASTNODE;
    this->pointer = pointer;
    this->field = field;
}

void arrow_astnode::gencode() {
    this->pointer->gencode();

    cout << "# arrow base type " << this->pointer->type.base_type << endl;
    auto struct_global_it = globalST.Entries.find(this->pointer->type.base_type);
    if(struct_global_it != globalST.Entries.end()) {
        auto struct_member_it = struct_global_it->second.symbtab->Entries.find(this->field->s);
        if(struct_member_it != struct_global_it->second.symbtab->Entries.end()) {
            this->to_dereference = true;

            // similar to arrayref; for cases like `struct s arr[10]; arr->x=1;`
            if(
                this->pointer->node_type == IDENTIFIER_ASTNODE &&
                curr_fn_ST_ptr->Entries.find(static_cast<identifier_astnode*>(this->pointer)->s)->second.type_ptr->is_array()
            ) {
                cout << "# arrow id is array" << endl;
                cout << indent << "leal\t" << this->pointer->stack_offset << "(%ebp), %eax" << endl;
            }
            else {
                cout << indent << "movl\t" << this->pointer->stack_offset << "(%ebp), %eax" << endl;
            }

            // cout << "movl\t(%eax), %eax" << endl;
            cout << indent << "addl\t$" << struct_member_it->second.offset << ", %eax" << endl;
            cout << indent << "pushl\t%eax" << endl;
            curr_stack_offset -= DEFAULT_SIZE;
            this->stack_offset = curr_stack_offset;

            // this->stack_offset = this->pointer->stack_offset;
            // this->member_start_offset = struct_member_it->second.offset;
            // cout << "# pointer offset, member offset = " << this->pointer->stack_offset << ' ' << this->member_start_offset << endl;
        }
    }
}

void arrayref_astnode::print() {
    cout << "{" << endl;
    cout << "\"arrayref\":" << endl;
    cout << "{" << endl;
    cout << "\"array\":" << endl;
    array->print();
    cout << "," << endl;
    cout << "\"index\":" << endl;
    index->print();
    cout << "}" << endl;
    cout << "}" << endl;
}

arrayref_astnode::arrayref_astnode(exp_astnode* array, exp_astnode* index) {
    node_type = ARRAYREF_ASTNODE;
    this->array = array;
    this->index = index;
}

void arrayref_astnode::gencode() {
    this->array->gencode();
    this->index->gencode();

    this->to_dereference = true;
    if(
        this->array->node_type == IDENTIFIER_ASTNODE &&
        curr_fn_ST_ptr->Entries.find(static_cast<identifier_astnode*>(this->array)->s)->second.type_ptr->is_array()
    ) {
        cout << "# array id is array" << endl;
        cout << indent << "leal\t" << this->array->stack_offset << "(%ebp), %eax" << endl;
    }
    else {
        cout << indent << "movl\t" << this->array->stack_offset << "(%ebp), %eax" << endl;
    }

    if(this->index->to_dereference == 1) {
        cout << indent << "movl\t" << this->index->stack_offset << "(%ebp), %edx" << endl;
        cout << indent << "movl\t(%edx), %edx" << endl;
    }
    else {
        cout << indent << "movl\t" << this->index->stack_offset << "(%ebp), %edx" << endl;
    }

    Type tmp_type = this->array->type;
    tmp_type.dereference();
    cout << indent << "imull\t$" << tmp_type.get_size() << ", %edx" << endl;

    cout << indent << "addl\t%edx, %eax" << endl;
    cout << indent << "pushl\t%eax" << endl;
    curr_stack_offset -= DEFAULT_SIZE;
    this->stack_offset = curr_stack_offset;
}

pair<bool,exp_astnode*> compatible_convert(exp_astnode* node, Type* type_ptr) {
    string given_type = node->type.get_type();
    string req_type = type_ptr->get_type();     // required type

    // cout << "compatible_convert: " << given_type << ' ' << req_type << endl;

    // same, and pointer<->void*
    if(
        (given_type == req_type) ||
        (node->type.is_array_or_pointer() && req_type=="void*") ||
        (type_ptr->is_array_or_pointer() && given_type=="void*")
    ) return {true,node};

    // int<->float
    if(given_type=="int" && req_type=="float") {        // "int" => node must be (derived of) exp_astnode
        op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", node);
        new_node->type = Type(req_type);
        return {true,new_node};
    }
    else if(given_type=="float" && req_type=="int") {
        op_unary_astnode* new_node = new op_unary_astnode("TO_INT", node);
        new_node->type = Type(req_type);
        return {true,new_node};
    }

    // array->pointer [via decay]
    if(node->type.is_array()) {
        node->type.decay_array_to_pointer();
        if(node->type.get_type() == req_type) return {true,node};
    }

    return {false,nullptr};
}