%skeleton "lalr1.cc"
%require  "3.0.1"
%language "c++"

%defines 
%define api.namespace {IPL}
%define api.parser.class {Parser}
/* %define api.location.type {IPL::location} */

%locations

%define parse.trace

%code requires{
   namespace IPL {
      class Scanner;
   }
   #include "ast.hh"
   #include "type.hh"
   #include "symbtab.hh"
   #include "location.hh"
}


%printer { std::cerr << $$; } VOID
%printer { std::cerr << $$; } INT
%printer { std::cerr << $$; } FLOAT
%printer { std::cerr << $$; } STRUCT
%printer { std::cerr << $$; } INT_CONSTANT
%printer { std::cerr << $$; } FLOAT_CONSTANT
%printer { std::cerr << $$; } RETURN
%printer { std::cerr << $$; } OR_OP
%printer { std::cerr << $$; } AND_OP
%printer { std::cerr << $$; } EQ_OP
%printer { std::cerr << $$; } NE_OP
%printer { std::cerr << $$; } LE_OP
%printer { std::cerr << $$; } GE_OP
%printer { std::cerr << $$; } INC_OP
%printer { std::cerr << $$; } PTR_OP
%printer { std::cerr << $$; } IF
%printer { std::cerr << $$; } ELSE
%printer { std::cerr << $$; } WHILE
%printer { std::cerr << $$; } FOR
%printer { std::cerr << $$; } STRING_LITERAL
%printer { std::cerr << $$; } IDENTIFIER
%printer { std::cerr << $$; } OTHERS
     /* others too... */


%parse-param { Scanner  &scanner  }

%code{

   #include <iostream>
   #include <cstdlib>
   #include <fstream>
   #include <string>
   #include <vector>
   #include "scanner.hh"

   // globals
   SymbTab globalST;
   SymbTab* localST = nullptr;
   std::map<string, abstract_astnode*> ast;
   std::map<std::string, Type> predefined_fns {
      {"printf", Type("void")},
      {"scanf", Type("void")},
      {"mod", Type("int")}
   };
   std::string fn_return_type;   // current function's return type
   std::string struct_or_fn;     // distinguish for declaration_list
   std::string param_or_local;   // parameter or local variable
   int curr_offset;     // current offset
   std::string curr_struct_name;

   // can put functions too
   // but cannot access @$ ig (without passing more params)

#undef yylex
#define yylex IPL::Parser::scanner.yylex

}

%define api.value.type variant
%define parse.assert

%start translation_unit  // start symbol


%token <std::string> VOID INT FLOAT STRUCT
%token <std::string> INT_CONSTANT FLOAT_CONSTANT
%token <std::string> RETURN
%token <std::string> OR_OP AND_OP EQ_OP NE_OP LE_OP GE_OP INC_OP PTR_OP
%token <std::string> IF ELSE
%token <std::string> WHILE FOR
%token <std::string> STRING_LITERAL
%token <std::string> IDENTIFIER
%token <std::string> OTHERS
%token '=' '+' '-' '*' '/' '>' '<' '{' '}' '[' ']' ',' '.' ';' '!' '&'

// later rule: higher precedence
// en.cppreference.com

%left OR_OP
%left AND_OP
%left EQ_OP NE_OP
%left '<' LE_OP
%left '>' GE_OP
%left '+' '-'
%left '*'
%left '/'
%right '&'
                  // section 5.4 of bison manual: Context-Dependent Precedence
                  // but we don't have rules like expr -> '-' expr
                  // (grammar handles this precedence)
%left PTR_OP
%left '.'
%left INC_OP

%nterm <abstract_astnode*> translation_unit
%nterm <abstract_astnode*> struct_specifier
%nterm <abstract_astnode*> function_definition

%nterm <std::string> type_specifier

%nterm <fun_declarator_class*> fun_declarator
%nterm <std::vector<decl_class*>*> parameter_list         // using ptr to avoid copying in actions like $$=$1
%nterm <decl_class*> parameter_declaration

%nterm <decl_class*> declarator_arr
%nterm <decl_class*> declarator
%nterm <std::vector<decl_class*>*> declarator_list

%nterm <decln_class*> declaration
%nterm <std::vector<decln_class*>*> declaration_list

%nterm <abstract_astnode*> compound_statement

%nterm <seq_astnode*> statement_list
%nterm <statement_astnode*> statement
%nterm <assignS_astnode*> assignment_statement
%nterm <statement_astnode*> selection_statement
%nterm <statement_astnode*> iteration_statement

%nterm <exp_astnode*> expression

%nterm <assignE_astnode*> assignment_expression
%nterm <exp_astnode*> logical_and_expression
%nterm <exp_astnode*> equality_expression
%nterm <exp_astnode*> relational_expression
%nterm <exp_astnode*> additive_expression
%nterm <exp_astnode*> unary_expression
%nterm <exp_astnode*> multiplicative_expression
%nterm <exp_astnode*> postfix_expression
%nterm <exp_astnode*> primary_expression
%nterm <std::string> unary_operator
%nterm <funcall_astnode*> expression_list

%nterm <proccall_astnode*> procedure_call

%%

/* default action: $$=$1 */

// earlier: many warnings, coz $$ and $1 have different types

  translation_unit: 
                    struct_specifier
                  | function_definition
                  | translation_unit struct_specifier
                  | translation_unit function_definition
  ;

  struct_specifier:  STRUCT IDENTIFIER
                     {
                        struct_or_fn = "struct";
                        param_or_local = "local";
                        curr_struct_name = $1+" "+$2;

                        if(globalST.Entries.find(curr_struct_name) != globalST.Entries.end()) {
                           error(@$, "\"" + curr_struct_name + "\" has a previous definition");
                        }

                        localST = new SymbTab();      // new ST for this struct
                        curr_offset = 0;
                     }
                     '{' declaration_list '}' ';'
                     {
                        std::string struct_name = $1+" "+$2;
                        Type* type_ptr = new Type("-");

                        // make globalST entry of this struct
                        SymbTabEntry ste(
                           struct_name,
                           "struct",
                           "global",
                           curr_offset,
                           0,
                           type_ptr,
                           localST
                        );
                        ste.is_struct_global_entry = true;
                        globalST.Entries.insert({struct_name,ste});
                        localST = nullptr;         // reset
                        $$ = nullptr;
                        ast[struct_name] = $$;
                        curr_struct_name = "";
                     }
  ;

  function_definition:  type_specifier
                        {
                           // if returning struct, check if it's declared
                           if($1.find("struct ") == 0) {
                              auto ste_it = globalST.Entries.find($1);
                              if(ste_it == globalST.Entries.end()) {
                                 error(@1, "\"" + $1 + "\" not declared");
                              }
                           }

                           fn_return_type = $1;
                           struct_or_fn = "fn";
                           param_or_local = "param";
                           localST = new SymbTab();      // new ST for this function
                        }
                        fun_declarator
                        {
                           if(globalST.Entries.find($3->fun_name) != globalST.Entries.end()) {
                              error(@3, "The function \"" + $3->fun_name + "\" has a previous definition");
                           }

                           // fill in parameter sizes and offsets
                           curr_offset = 8;  // for A2, was 12 to match ref output
                           for(auto decl=$3->params->rbegin(); decl!=$3->params->rend(); decl++) {
                              Type* original_type_ptr = new decl_class();
                              *original_type_ptr = **decl;
                              (*decl)->decay_array_to_pointer();     // fn param: decay array to pointer [if applicable]

                              SymbTabEntry* ste_ptr = &(localST->Entries[(*decl)->instance_name]);
                              ste_ptr->original_type_ptr = original_type_ptr;
                              ste_ptr->print_original = true;
                              
                              ste_ptr->size = (*decl)->get_size();
                              ste_ptr->offset = curr_offset;
                              curr_offset += ste_ptr->size;
                           }

                           param_or_local = "local";     // now locals
                           curr_offset = 0;

                           // make globalST entry of this function
                           // recursion allowed, so mid-rule, and not end-rule

                           Type* type_ptr = new Type($1);

                           SymbTabEntry ste(
                              $3->fun_name,
                              "fun",
                              "global",
                              0,
                              0,
                              type_ptr,
                              localST
                           );
                           ste.fn_decl = $3;
                           globalST.Entries.insert({$3->fun_name,ste});
                        }
                        compound_statement
                        {
                           ast[$3->fun_name] = $5;
                           localST = nullptr;         // reset
                           $$ = nullptr;
                        }
  ;

  type_specifier:   VOID { $$ = "void"; }
                  | INT { $$ = "int"; }
                  | FLOAT { $$ = "float"; }
                  | STRUCT IDENTIFIER { $$ = "struct "+$2; }
  ;

  fun_declarator: IDENTIFIER '(' parameter_list ')'
                  {
                     $$ = new fun_declarator_class();
                     $$->fun_name = $1;
                     $$->params = $3;
                  }
                | IDENTIFIER '(' ')'
                  {
                     $$ = new fun_declarator_class();
                     $$->fun_name = $1;
                     $$->params = new vector<decl_class*>;
                     $$->params->clear();
                  }
  ;

  parameter_list: parameter_declaration
                     {
                        $$ = new vector<decl_class*>;
                        $$->clear();
                        $$->push_back($1);
                     }
                | parameter_list ',' parameter_declaration
                     {
                        // std::cout << @$ << " " << @1 << " " << @2 << " " << @3 << std::endl;

                        $$ = $1;
                        $$->push_back($3);
                     }
  ;

  parameter_declaration: type_specifier declarator
                           {
                              if ($1=="void" && $2->stars==0) {   // void x[10] also not allowed
                                 error(@2, "Cannot declare the type of a parameter as \"" + $1 + "\"");
                              }
                              // if struct, check if it's declared (vs for struct declarations: can be ptr to same struct)
                              if($1.find("struct ") == 0) {
                                 auto ste_it = globalST.Entries.find($1);
                                 if(ste_it == globalST.Entries.end()) {
                                    error(@1, "\"" + $1 + "\" is not defined");
                                 }
                              }

                              $$ = $2;
                              $$->base_type = $1;
                              if($1.find("struct ") == 0) {
                                 $$->base_type_size = globalST.Entries.find($1)->second.size;
                              }
                              else {
                                 $$->base_type_size = DEFAULT_SIZE;
                              }
                           }
  ;

  declarator_arr: IDENTIFIER
                  {
                     $$ = new decl_class();
                     $$->instance_name = $1;

                     if(localST->Entries.find($1) != localST->Entries.end()) {
                        error(@1, "\"" + $1 + "\"" + " has a previous declaration");
                     }
                     else {
                        localST->Entries[$1] = SymbTabEntry(
                           $1,
                           "var",
                           param_or_local,
                           0,
                           0,
                           $$,
                           localST
                        );
                        localST->Entries[$1].type_ptr->lvalue = "modifiable";
                     }
                  }
                | declarator_arr '[' INT_CONSTANT ']'
                  {
                     $$ = $1;
                     $$->arr_indices.push_back($3);
                     localST->Entries[$1->instance_name].type_ptr->lvalue = "unmodifiable";
                  }
  ;

  declarator: declarator_arr
               {
                  $$ = $1;
               }
            | '*' declarator
               {
                  $$ = $2;
                  $$->stars++;
               }
  ;

  compound_statement:  '{' '}'
                        {
                           $$ = new seq_astnode();    // has empty vector
                        }
                     | '{' statement_list '}'
                        {
                           $$ = $2;
                        }
                     | '{' declaration_list '}'
                        {
                           $$ = new seq_astnode();
                        }
                     | '{' declaration_list statement_list '}'
                        {
                           $$ = $3;
                        }
  ;

  statement_list: statement
                  {
                     $$ = new seq_astnode();
                     $$->vec_stmt_astnode.push_back($1);
                  }
                | statement_list statement
                  {
                     $$ = $1;
                     $$->vec_stmt_astnode.push_back($2);
                  }
  ;

  statement: ';'
               {
                  $$ = new empty_astnode();
               }
            | '{' statement_list '}'   { $$ = $2; }
            | selection_statement      { $$ = $1; }
            | iteration_statement      { $$ = $1; }
            | assignment_statement     { $$ = $1; }
            | procedure_call           { $$ = $1; }
            | RETURN expression ';'
               {
                  exp_astnode* ret_expr = $2;

                  // check return type (grammar only allows void, int, float, struct)
                  if(struct_or_fn=="fn") {
                     std::string given_type = $2->type.get_type();
                     if(given_type != fn_return_type) {
                        if(given_type=="int" && fn_return_type=="float") {
                           ret_expr = new op_unary_astnode("TO_FLOAT", $2);
                        }
                        else if(given_type=="float" && fn_return_type=="int") {
                           ret_expr = new op_unary_astnode("TO_INT", $2);
                        }
                        else {
                           error(@2, "Incompatible type \"" + given_type + "\" returned, expected \"" + fn_return_type + "\"");
                        }
                     }
                  }

                  $$ = new return_astnode(ret_expr);
               }
  ;

  assignment_expression: unary_expression '=' expression
                           {
                              if($1->type.lvalue == "") {
                                 error(@1, "Left operand of assignment should have an lvalue");
                              }
                              if($1->type.lvalue != "modifiable") {  // can put redundant check: $1->type.is_array()
                                 error(@1, "Left operand of assignment should have a modifiable lvalue");
                              }

                              auto pair_bool_node = compatible_convert($3, &($1->type));  // RHS might be decayed
                              bool rhs_zero_literal = ($3->node_type == INTCONST_ASTNODE && static_cast<intconst_astnode*>($3)->in == 0);    // note that int<->float would have already been handled in compatible_convert()

                              if(!(pair_bool_node.first) && !rhs_zero_literal) {
                                 error(@1, "Incompatible assignment when assigning to type \"" + $1->type.get_type() + "\" from type \"" + $3->type.get_type() + "\"");
                              }
                              if(pair_bool_node.first) $3 = pair_bool_node.second;

                              $$ = new assignE_astnode($1, $3);
                              $$->type = Type("int");    // no lvalue
                           }
  ;

  assignment_statement: assignment_expression ';'
                        {
                           $$ = new assignS_astnode($1->left, $1->right);
                           $$->type = Type("void");
                        }
  ;

  procedure_call: IDENTIFIER '(' ')' ';'
                  {
                     bool predefined = false;

                     // check if function exists (in predefined too)
                     auto ste_it = globalST.Entries.find($1);
                     if(ste_it == globalST.Entries.end()) {
                        if(predefined_fns.find($1) != predefined_fns.end()) {
                           predefined = true;
                        }
                        else {
                           error(@1, "Function \""+ $1 +"\" not declared");
                        }
                     }
                     
                     SymbTabEntry* ste_ptr;
                     
                     if(!predefined) {

                        ste_ptr = &(ste_it->second);
                        if(ste_ptr->fn_decl->params->size() != 0) {
                           error(@1, "Function \"" + $1 + "\"  called with too few arguments");
                        }

                     }

                     identifier_astnode* id_node = new identifier_astnode($1);

                     $$ = new proccall_astnode();
                     $$->vec_exp_astnode.push_back(id_node);
                     $$->type = Type("void");   // and no lvalue
                  }
                | IDENTIFIER '(' expression_list ')' ';'
                  {
                     bool predefined = false;

                     // check if function exists (in predefined too)
                     auto ste_it = globalST.Entries.find($1);
                     if(ste_it == globalST.Entries.end()) {
                        if(predefined_fns.find($1) != predefined_fns.end()) {
                           predefined = true;
                        }
                        else {
                           error(@1, "Function \""+ $1 +"\" not declared");
                        }
                     }

                     SymbTabEntry* ste_ptr;

                     if(!predefined) {

                        // check for number of params
                        ste_ptr = &(ste_it->second);
                        if(ste_ptr->fn_decl->params->size() < $3->vec_exp_astnode.size()) {
                           error(@1, "Function \"" + $1 + "\" called with too many arguments");
                        }
                        if(ste_ptr->fn_decl->params->size() > $3->vec_exp_astnode.size()) {
                           error(@1, "Function \"" + $1 + "\" called with too few arguments");
                        }

                        // check param types
                        for(size_t j=0; j<$3->vec_exp_astnode.size(); j++) {
                           auto pair_bool_node = compatible_convert($3->vec_exp_astnode[j], (*ste_ptr->fn_decl->params)[j]);
                           if(!(pair_bool_node.first)) {
                              error(@3, "Expected \"" + (*ste_ptr->fn_decl->params)[j]->get_type() + "\" but argument is of type \"" + $3->vec_exp_astnode[j]->type.get_type() + "\"");
                           }
                           $3->vec_exp_astnode[j] = pair_bool_node.second;
                        }

                     }

                     identifier_astnode* id_node = new identifier_astnode($1);
                     
                     $$ = new proccall_astnode();
                     $$->vec_exp_astnode.push_back(id_node);
                     for(auto&& e:$3->vec_exp_astnode) {
                        $$->vec_exp_astnode.push_back(e);
                     }
                     $$->type = Type("void");   // and no lvalue
                  }
  ;

  expression: logical_and_expression
               {
                  $$ = $1;
               }
            | expression OR_OP logical_and_expression
               {
                  if(!($1->type.get_type()=="int" || $1->type.get_type()=="float" || $1->type.is_array_or_pointer())) {
                     error(@1, "Invalid operand of ||, not scalar or pointer");
                  }
                  if(!($3->type.get_type()=="int" || $3->type.get_type()=="float" || $3->type.is_array_or_pointer())) {
                     error(@3, "Invalid operand of ||, not scalar or pointer");
                  }
                  $$ = new op_binary_astnode("OR_OP", $1, $3);
                  $$->type = Type("int");    // no lvalue
               }
  ;

  logical_and_expression: equality_expression
                           {
                              $$ = $1;
                           }
                        | logical_and_expression AND_OP equality_expression
                           {
                              if(!($1->type.get_type()=="int" || $1->type.get_type()=="float" || $1->type.is_array_or_pointer())) {
                                 error(@1, "Invalid operand of &&, not scalar or pointer");
                              }
                              if(!($3->type.get_type()=="int" || $3->type.get_type()=="float" || $3->type.is_array_or_pointer())) {
                                 error(@3, "Invalid operand of &&, not scalar or pointer");
                              }
                              $$ = new op_binary_astnode("AND_OP", $1, $3);
                              $$->type = Type("int");    // no lvalue
                           }
  ;
  
  equality_expression: relational_expression
                        {
                           $$ = $1;
                        }
                      | equality_expression EQ_OP relational_expression
                        {
                           exp_astnode* left_node = $1;
                           exp_astnode* right_node = $3;
                           left_node->type.decay_array_to_pointer();
                           right_node->type.decay_array_to_pointer();

                           std::string op_bin_str = "";
                           std::string lhs_type = left_node->type.get_type();
                           std::string rhs_type = right_node->type.get_type();

                           if(lhs_type=="int" && rhs_type=="int") {
                              op_bin_str = "EQ_OP_INT";
                           }
                           else if(lhs_type=="float" && rhs_type=="float") {
                              op_bin_str = "EQ_OP_FLOAT";
                           }
                           else if(lhs_type=="int" && rhs_type=="float") {
                              op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $1);
                              new_node->type = Type("float");
                              op_bin_str = "EQ_OP_FLOAT";
                              left_node = new_node;
                           }
                           else if(lhs_type=="float" && rhs_type=="int") {
                              op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $3);
                              new_node->type = Type("float");
                              op_bin_str = "EQ_OP_FLOAT";
                              right_node = new_node;
                           }
                           else if(lhs_type==rhs_type && $1->type.is_array_or_pointer() && $3->type.is_array_or_pointer()) {
                              op_bin_str = "EQ_OP_INT";
                           }
                           else if(
                              ($1->type.is_array_or_pointer() && $3->node_type == INTCONST_ASTNODE &&
                              static_cast<intconst_astnode*>($3)->in == 0)
                              ||
                              ($3->type.is_array_or_pointer() && $1->node_type == INTCONST_ASTNODE &&
                              static_cast<intconst_astnode*>($1)->in == 0)
                           ) {
                              op_bin_str = "EQ_OP_INT";
                           }
                           else {
                              error(@1, "Invalid operand types for binary ==, \"" + left_node->type.get_type() + "\" and \"" + right_node->type.get_type() + "\"");
                           }

                           $$ = new op_binary_astnode(op_bin_str, left_node, right_node);
                           $$->type = Type("int");    // no lvalue
                        }
                      | equality_expression NE_OP relational_expression
                        {
                           exp_astnode* left_node = $1;
                           exp_astnode* right_node = $3;
                           left_node->type.decay_array_to_pointer();
                           right_node->type.decay_array_to_pointer();

                           std::string op_bin_str = "";
                           std::string lhs_type = left_node->type.get_type();
                           std::string rhs_type = right_node->type.get_type();

                           if(lhs_type=="int" && rhs_type=="int") {
                              op_bin_str = "NE_OP_INT";
                           }
                           else if(lhs_type=="float" && rhs_type=="float") {
                              op_bin_str = "NE_OP_FLOAT";
                           }
                           else if(lhs_type=="int" && rhs_type=="float") {
                              op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $1);
                              new_node->type = Type("float");
                              op_bin_str = "NE_OP_FLOAT";
                              left_node = new_node;
                           }
                           else if(lhs_type=="float" && rhs_type=="int") {
                              op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $3);
                              new_node->type = Type("float");
                              op_bin_str = "NE_OP_FLOAT";
                              right_node = new_node;
                           }
                           else if(lhs_type==rhs_type && $1->type.is_array_or_pointer() && $3->type.is_array_or_pointer()) {
                              op_bin_str = "NE_OP_INT";
                           }
                           else if(
                              ($1->type.is_array_or_pointer() && $3->node_type == INTCONST_ASTNODE &&
                              static_cast<intconst_astnode*>($3)->in == 0)
                              ||
                              ($3->type.is_array_or_pointer() && $1->node_type == INTCONST_ASTNODE &&
                              static_cast<intconst_astnode*>($1)->in == 0)
                           ) {
                              op_bin_str = "NE_OP_INT";
                           }
                           else {
                              error(@1, "Invalid operand types for binary ==, \"" + left_node->type.get_type() + "\" and \"" + right_node->type.get_type() + "\"");
                           }

                           $$ = new op_binary_astnode(op_bin_str, left_node, right_node);
                           $$->type = Type("int");    // no lvalue
                        }
  ;

  relational_expression:  additive_expression
                           {
                              $$ = $1;
                           }
                        | relational_expression '<' additive_expression
                           {
                              exp_astnode* left_node = $1;
                              exp_astnode* right_node = $3;
                              left_node->type.decay_array_to_pointer();
                              right_node->type.decay_array_to_pointer();

                              std::string op_bin_str = "";
                              std::string lhs_type = left_node->type.get_type();
                              std::string rhs_type = right_node->type.get_type();

                              if(lhs_type=="int" && rhs_type=="int") {
                                 op_bin_str = "LT_OP_INT";
                              }
                              else if(lhs_type=="float" && rhs_type=="float") {
                                 op_bin_str = "LT_OP_FLOAT";
                              }
                              else if(lhs_type=="int" && rhs_type=="float") {
                                 op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $1);
                                 new_node->type = Type("float");
                                 op_bin_str = "LT_OP_FLOAT";
                                 left_node = new_node;
                              }
                              else if(lhs_type=="float" && rhs_type=="int") {
                                 op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $3);
                                 new_node->type = Type("float");
                                 op_bin_str = "LT_OP_FLOAT";
                                 right_node = new_node;
                              }
                              else if(lhs_type==rhs_type && $1->type.is_array_or_pointer() && $3->type.is_array_or_pointer()) {
                                 op_bin_str = "LT_OP_INT";
                              }
                              else if(
                                 ($1->type.is_array_or_pointer() && $3->node_type == INTCONST_ASTNODE &&
                                 static_cast<intconst_astnode*>($3)->in == 0)
                                 ||
                                 ($3->type.is_array_or_pointer() && $1->node_type == INTCONST_ASTNODE &&
                                 static_cast<intconst_astnode*>($1)->in == 0)
                              ) {
                                 op_bin_str = "LT_OP_INT";
                              }
                              else {
                                 error(@1, "Invalid operand types for binary <, \"" + left_node->type.get_type() + "\" and \"" + right_node->type.get_type() + "\"");
                              }

                              $$ = new op_binary_astnode(op_bin_str, left_node, right_node);
                              $$->type = Type("int");
                              $$->type.lvalue = "";
                           }
                        | relational_expression '>' additive_expression
                           {
                              exp_astnode* left_node = $1;
                              exp_astnode* right_node = $3;
                              left_node->type.decay_array_to_pointer();
                              right_node->type.decay_array_to_pointer();

                              std::string op_bin_str = "";
                              std::string lhs_type = left_node->type.get_type();
                              std::string rhs_type = right_node->type.get_type();

                              if(lhs_type=="int" && rhs_type=="int") {
                                 op_bin_str = "GT_OP_INT";
                              }
                              else if(lhs_type=="float" && rhs_type=="float") {
                                 op_bin_str = "GT_OP_FLOAT";
                              }
                              else if(lhs_type=="int" && rhs_type=="float") {
                                 op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $1);
                                 new_node->type = Type("float");
                                 op_bin_str = "GT_OP_FLOAT";
                                 left_node = new_node;
                              }
                              else if(lhs_type=="float" && rhs_type=="int") {
                                 op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $3);
                                 new_node->type = Type("float");
                                 op_bin_str = "GT_OP_FLOAT";
                                 right_node = new_node;
                              }
                              else if(lhs_type==rhs_type && $1->type.is_array_or_pointer() && $3->type.is_array_or_pointer()) {
                                 op_bin_str = "GT_OP_INT";
                              }
                              else if(
                                 ($1->type.is_array_or_pointer() && $3->node_type == INTCONST_ASTNODE &&
                                 static_cast<intconst_astnode*>($3)->in == 0)
                                 ||
                                 ($3->type.is_array_or_pointer() && $1->node_type == INTCONST_ASTNODE &&
                                 static_cast<intconst_astnode*>($1)->in == 0)
                              ) {
                                 op_bin_str = "GT_OP_INT";
                              }
                              else {
                                 error(@1, "Invalid operand types for binary >, \"" + left_node->type.get_type() + "\" and \"" + right_node->type.get_type() + "\"");
                              }

                              $$ = new op_binary_astnode(op_bin_str, left_node, right_node);
                              $$->type = Type("int");
                              $$->type.lvalue = "";
                           }
                        | relational_expression LE_OP additive_expression
                           {
                              exp_astnode* left_node = $1;
                              exp_astnode* right_node = $3;
                              left_node->type.decay_array_to_pointer();
                              right_node->type.decay_array_to_pointer();

                              std::string op_bin_str = "";
                              std::string lhs_type = left_node->type.get_type();
                              std::string rhs_type = right_node->type.get_type();

                              if(lhs_type=="int" && rhs_type=="int") {
                                 op_bin_str = "LE_OP_INT";
                              }
                              else if(lhs_type=="float" && rhs_type=="float") {
                                 op_bin_str = "LE_OP_FLOAT";
                              }
                              else if(lhs_type=="int" && rhs_type=="float") {
                                 op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $1);
                                 new_node->type = Type("float");
                                 op_bin_str = "LE_OP_FLOAT";
                                 left_node = new_node;
                              }
                              else if(lhs_type=="float" && rhs_type=="int") {
                                 op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $3);
                                 new_node->type = Type("float");
                                 op_bin_str = "LE_OP_FLOAT";
                                 right_node = new_node;
                              }
                              else if(lhs_type==rhs_type && $1->type.is_array_or_pointer() && $3->type.is_array_or_pointer()) {
                                 op_bin_str = "LE_OP_INT";
                              }
                              else if(
                                 ($1->type.is_array_or_pointer() && $3->node_type == INTCONST_ASTNODE &&
                                 static_cast<intconst_astnode*>($3)->in == 0)
                                 ||
                                 ($3->type.is_array_or_pointer() && $1->node_type == INTCONST_ASTNODE &&
                                 static_cast<intconst_astnode*>($1)->in == 0)
                              ) {
                                 op_bin_str = "LE_OP_INT";
                              }
                              else {
                                 error(@1, "Invalid operand types for binary <=, \"" + left_node->type.get_type() + "\" and \"" + right_node->type.get_type() + "\"");
                              }

                              $$ = new op_binary_astnode(op_bin_str, left_node, right_node);
                              $$->type = Type("int");
                              $$->type.lvalue = "";
                           }
                        | relational_expression GE_OP additive_expression
                           {
                              exp_astnode* left_node = $1;
                              exp_astnode* right_node = $3;
                              left_node->type.decay_array_to_pointer();
                              right_node->type.decay_array_to_pointer();

                              std::string op_bin_str = "";
                              std::string lhs_type = left_node->type.get_type();
                              std::string rhs_type = right_node->type.get_type();

                              if(lhs_type=="int" && rhs_type=="int") {
                                 op_bin_str = "GE_OP_INT";
                              }
                              else if(lhs_type=="float" && rhs_type=="float") {
                                 op_bin_str = "GE_OP_FLOAT";
                              }
                              else if(lhs_type=="int" && rhs_type=="float") {
                                 op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $1);
                                 new_node->type = Type("float");
                                 op_bin_str = "GE_OP_FLOAT";
                                 left_node = new_node;
                              }
                              else if(lhs_type=="float" && rhs_type=="int") {
                                 op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $3);
                                 new_node->type = Type("float");
                                 op_bin_str = "GE_OP_FLOAT";
                                 right_node = new_node;
                              }
                              else if(lhs_type==rhs_type && $1->type.is_array_or_pointer() && $3->type.is_array_or_pointer()) {
                                 op_bin_str = "GE_OP_INT";
                              }
                              else if(
                                 ($1->type.is_array_or_pointer() && $3->node_type == INTCONST_ASTNODE &&
                                 static_cast<intconst_astnode*>($3)->in == 0)
                                 ||
                                 ($3->type.is_array_or_pointer() && $1->node_type == INTCONST_ASTNODE &&
                                 static_cast<intconst_astnode*>($1)->in == 0)
                              ) {
                                 op_bin_str = "GE_OP_INT";
                              }
                              else {
                                 error(@1, "Invalid operand types for binary >=, \"" + left_node->type.get_type() + "\" and \"" + right_node->type.get_type() + "\"");
                              }

                              $$ = new op_binary_astnode(op_bin_str, left_node, right_node);
                              $$->type = Type("int");
                              $$->type.lvalue = "";
                           }
  ;

  additive_expression: multiplicative_expression
                        {
                           $$ = $1;    // same types
                        }
                      | additive_expression '+' multiplicative_expression
                        {
                           exp_astnode* left_node = $1;
                           exp_astnode* right_node = $3;
                           left_node->type.decay_array_to_pointer();
                           right_node->type.decay_array_to_pointer();

                           std::string op_bin_str = "";
                           std::string lhs_type = left_node->type.get_type();
                           std::string rhs_type = right_node->type.get_type();
                           Type ty;

                           if(lhs_type=="int" && rhs_type=="int") {
                              op_bin_str = "PLUS_INT";
                              ty = left_node->type;
                           }
                           else if(lhs_type=="float" && rhs_type=="float") {
                              op_bin_str = "PLUS_FLOAT";
                              ty = left_node->type;
                           }
                           else if(lhs_type=="int" && rhs_type=="float") {
                              op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $1);
                              new_node->type = Type("float");
                              op_bin_str = "PLUS_FLOAT";
                              left_node = new_node;
                              ty = left_node->type;
                           }
                           else if(lhs_type=="float" && rhs_type=="int") {
                              op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $3);
                              new_node->type = Type("float");
                              op_bin_str = "PLUS_FLOAT";
                              right_node = new_node;
                              ty = left_node->type;
                           }
                           else if($1->type.is_array_or_pointer() && rhs_type=="int") {   // won't be array due to above decay
                              op_bin_str = "PLUS_INT";
                              ty = $1->type;
                              // ty.decay_array_to_pointer();  // if array type, will decay
                           }
                           else if(lhs_type=="int" && $3->type.is_array_or_pointer()) {
                              op_bin_str = "PLUS_INT";
                              ty = $3->type;
                              // ty.decay_array_to_pointer();  // if array type, will decay
                           }
                           else {
                              error(@1, "Invalid operand types for binary +, \"" + left_node->type.get_type() + "\" and \"" + right_node->type.get_type() + "\"");
                           }

                           $$ = new op_binary_astnode(op_bin_str, left_node, right_node);
                           $$->type = ty;
                           $$->type.lvalue = "";
                        }
                      | additive_expression '-' multiplicative_expression
                        {
                           exp_astnode* left_node = $1;
                           exp_astnode* right_node = $3;
                           left_node->type.decay_array_to_pointer();
                           right_node->type.decay_array_to_pointer();

                           std::string op_bin_str = "";
                           std::string lhs_type = left_node->type.get_type();
                           std::string rhs_type = right_node->type.get_type();
                           Type ty;
                           // bool is_ptr_ptr_sub = false;

                           if(lhs_type=="int" && rhs_type=="int") {
                              op_bin_str = "MINUS_INT";
                              ty = left_node->type;
                           }
                           else if(lhs_type=="float" && rhs_type=="float") {
                              op_bin_str = "MINUS_FLOAT";
                              ty = left_node->type;
                           }
                           else if(lhs_type=="int" && rhs_type=="float") {
                              op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $1);
                              new_node->type = Type("float");
                              op_bin_str = "MINUS_FLOAT";
                              left_node = new_node;
                              ty = left_node->type;
                           }
                           else if(lhs_type=="float" && rhs_type=="int") {
                              op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $3);
                              new_node->type = Type("float");
                              op_bin_str = "MINUS_FLOAT";
                              right_node = new_node;
                              ty = left_node->type;
                           }
                           else if($1->type.is_array_or_pointer() && rhs_type=="int") {
                              // ptr-int allowed, but not int-ptr
                              op_bin_str = "MINUS_INT";
                              ty = $1->type;
                              // ty.decay_array_to_pointer();  // if array type, will decay
                           }
                           else if(lhs_type==rhs_type && $1->type.is_array_or_pointer() && $3->type.is_array_or_pointer()) {
                              op_bin_str = "MINUS_INT";
                              ty = Type("int");    // sub ptrs of same type: int
                           }
                           else {
                              error(@1, "Invalid operand types for binary -, \"" + left_node->type.get_type() + "\" and \"" + right_node->type.get_type() + "\"");
                           }

                           $$ = new op_binary_astnode(op_bin_str, left_node, right_node);
                           $$->type = ty;
                           $$->type.lvalue = "";
                        }
  ;

  unary_expression: postfix_expression
                     {
                        $$ = $1;
                     }
                  | unary_operator unary_expression
                     {
                        std::string expr_type = $2->type.get_type();
                        if($1=="&" && $2->type.lvalue=="") {
                           error(@2, "Operand of & should have lvalue");   // @2: different from gcc [gcc shows @1 (equiv @$)]
                                                                           // for all actions here
                        }
                        if(
                           $1=="*" && (
                              !($2->type.is_array_or_pointer()) ||
                              expr_type=="void*"
                           )
                        ) {
                           // can't dereference void* or non-array-or-pointer types
                           error(@2, "Invalid operand type \"" + $2->type.get_type() + "\" of unary *");
                        }
                        if($1=="-" && !(expr_type=="int" || expr_type=="float")) {
                           error(@2, "Operand of unary - should be an int or float");
                        }
                        if($1=="!" && !(expr_type=="int" || expr_type=="float" || $2->type.is_array_or_pointer())) {
                           error(@2, "Operand of NOT should be an int or float or pointer");
                        }

                        std::string op_un_str = "";
                        if($1 == "-") op_un_str = "UMINUS";
                        else if($1 == "&") op_un_str = "ADDRESS";
                        else if($1 == "!") op_un_str = "NOT";
                        else if($1 == "*") op_un_str = "DEREF";
                        
                        $$ = new op_unary_astnode(op_un_str, $2);
                        $$->type = $2->type;
                        if($1=="&") {
                           if($$->type.arr_indices.size()>0) {
                              $$->type.stars_brackets += 1;
                           }
                           else {
                              $$->type.stars += 1;
                           }
                           $$->type.lvalue = "";
                        }
                        else if($1=="*") {
                           if(!($$->type.dereference())) {     // modifies lvalue too
                              error(@2, "Invalid operand type \"" + $2->type.get_type() + "\" of unary *");
                           }
                           // std::cout << "deref type " << $$->type.get_type() << "; lvalue: " << $$->type.lvalue << std::endl;
                        }
                        else if($1=="!") {
                           $$->type = Type("int");
                        }
                        else if($1=="-") {         // no lvalue
                           $$->type.lvalue = "";
                        }
                     }
  ;

  multiplicative_expression: unary_expression
                              {
                                 $$ = $1;
                              }
                            | multiplicative_expression '*' unary_expression
                              {
                                 std::string op_bin_str = "";
                                 std::string lhs_type = $1->type.get_type();
                                 std::string rhs_type = $3->type.get_type();
                                 exp_astnode* left_node = $1;
                                 exp_astnode* right_node = $3;

                                 if(lhs_type=="int" && rhs_type=="int") {
                                    op_bin_str = "MULT_INT";
                                 }
                                 else if(lhs_type=="float" && rhs_type=="float") {
                                    op_bin_str = "MULT_FLOAT";
                                 }
                                 else if(lhs_type=="int" && rhs_type=="float") {
                                    op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $1);
                                    new_node->type = Type("float");
                                    op_bin_str = "MULT_FLOAT";
                                    left_node = new_node;
                                 }
                                 else if(lhs_type=="float" && rhs_type=="int") {
                                    op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $3);
                                    new_node->type = Type("float");
                                    op_bin_str = "MULT_FLOAT";
                                    right_node = new_node;
                                 }
                                 else {
                                    error(@1, "Invalid operand types for binary *, \"" + left_node->type.get_type() + "\" and \"" + right_node->type.get_type() + "\"");
                                 }

                                 $$ = new op_binary_astnode(op_bin_str, left_node, right_node);
                                 $$->type = left_node->type;   // both left and right have the same type
                                 $$->type.lvalue = "";         // no lvalue
                              }
                            | multiplicative_expression '/' unary_expression
                              {
                                 std::string op_bin_str = "";
                                 std::string lhs_type = $1->type.get_type();
                                 std::string rhs_type = $3->type.get_type();
                                 exp_astnode* left_node = $1;
                                 exp_astnode* right_node = $3;

                                 if(lhs_type=="int" && rhs_type=="int") {
                                    op_bin_str = "DIV_INT";
                                 }
                                 else if(lhs_type=="float" && rhs_type=="float") {
                                    op_bin_str = "DIV_FLOAT";
                                 }
                                 else if(lhs_type=="int" && rhs_type=="float") {
                                    op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $1);
                                    new_node->type = Type("float");
                                    op_bin_str = "DIV_FLOAT";
                                    left_node = new_node;
                                 }
                                 else if(lhs_type=="float" && rhs_type=="int") {
                                    op_unary_astnode* new_node = new op_unary_astnode("TO_FLOAT", $3);
                                    new_node->type = Type("float");
                                    op_bin_str = "DIV_FLOAT";
                                    right_node = new_node;
                                 }
                                 else {
                                    error(@1, "Invalid operand types for binary /, \"" + left_node->type.get_type() + "\" and \"" + right_node->type.get_type() + "\"");
                                 }

                                 $$ = new op_binary_astnode(op_bin_str, left_node, right_node);
                                 $$->type = left_node->type;
                                 $$->type.lvalue = "";         // no lvalue
                              }
  ;

  postfix_expression: primary_expression
                        {
                           $$ = $1;    // same as default action
                        }
                    | postfix_expression '[' expression ']'
                        {
                           if($3->type.get_type() != "int") {
                              error(@3, "Array subscript is not an integer");
                           }
                           if($1->type.stars_brackets==0 && $1->type.stars==0 && $1->type.arr_indices.size()==0) {
                              error(@1, "Subscripted value is neither array nor pointer");
                           }
                           $$ = new arrayref_astnode($1, $3);
                           $$->type = $1->type;
                           $$->type.dereference();    // modifies lvalue if required
                        }
                    | IDENTIFIER '(' ')'
                        {
                           bool predefined = false;

                           // check if function exists (in predefined too)
                           auto ste_it = globalST.Entries.find($1);
                           if(ste_it == globalST.Entries.end()) {
                              if(predefined_fns.find($1) != predefined_fns.end()) {
                                 predefined = true;
                              }
                              else {
                                 error(@1, "Function \""+ $1 +"\" not declared");
                              }
                           }
                           
                           SymbTabEntry* ste_ptr;
                           
                           if(!predefined) {

                              ste_ptr = &(ste_it->second);
                              if(ste_ptr->fn_decl->params->size() != 0) {
                                 error(@1, "Function \"" + $1 + "\"  called with too few arguments");
                              }

                           }

                           identifier_astnode* id_node = new identifier_astnode($1);

                           $$ = new funcall_astnode();
                           static_cast<funcall_astnode*>($$)->vec_exp_astnode.push_back(id_node);
                           $$->type = predefined ? predefined_fns.find($1)->second : *(ste_ptr->type_ptr);  // return type,
                                                                                                            // no lvalue

                        }
                    | IDENTIFIER '(' expression_list ')'
                        {
                           bool predefined = false;

                           // check if function exists (in predefined too)
                           auto ste_it = globalST.Entries.find($1);
                           if(ste_it == globalST.Entries.end()) {
                              if(predefined_fns.find($1) != predefined_fns.end()) {
                                 predefined = true;
                              }
                              else {
                                 error(@1, "Function \""+ $1 +"\" not declared");
                              }
                           }

                           SymbTabEntry* ste_ptr;

                           if(!predefined) {

                              // check for number of params
                              ste_ptr = &(ste_it->second);
                              if(ste_ptr->fn_decl->params->size() < $3->vec_exp_astnode.size()) {
                                 error(@1, "Function \"" + $1 + "\" called with too many arguments");
                              }
                              if(ste_ptr->fn_decl->params->size() > $3->vec_exp_astnode.size()) {
                                 error(@1, "Function \"" + $1 + "\" called with too few arguments");
                              }

                              // check param types
                              for(size_t j=0; j<$3->vec_exp_astnode.size(); j++) {
                                 auto pair_bool_node = compatible_convert($3->vec_exp_astnode[j], (*ste_ptr->fn_decl->params)[j]);
                                 if(!(pair_bool_node.first)) {
                                    error(@3, "Expected \"" + (*ste_ptr->fn_decl->params)[j]->get_type() + "\" but argument is of type \"" + $3->vec_exp_astnode[j]->type.get_type() + "\"");
                                 }
                                 $3->vec_exp_astnode[j] = pair_bool_node.second;
                              }

                           }

                           identifier_astnode* id_node = new identifier_astnode($1);
                           
                           $$ = new funcall_astnode();
                           static_cast<funcall_astnode*>($$)->vec_exp_astnode.push_back(id_node);
                           for(auto&& e:$3->vec_exp_astnode) {
                              (static_cast<funcall_astnode*>($$))->vec_exp_astnode.push_back(e);
                           }
                           $$->type = predefined ? predefined_fns.find($1)->second : *(ste_ptr->type_ptr);  // return type
                                                                                                            // no lvalue

                        }
                    | postfix_expression '.' IDENTIFIER
                        {
                           // check type (struct)
                           std::string struct_type = $1->type.get_type();
                           auto ste_it = globalST.Entries.find(struct_type);
                           if(ste_it == globalST.Entries.end()) {    // this won't occur
                              error(@1, "\"" + struct_type + "\" not declared");
                           }
                           else if(ste_it->second.type_symtab != "struct") {
                              error(@1, "Left operand of \".\" is not a structure");
                           }

                           SymbTabEntry* gste_ptr = &(ste_it->second);  // pointer to global symbtab entry
                           if(gste_ptr->symbtab->Entries.find($3) == gste_ptr->symbtab->Entries.end()) {
                              error(@1, "Struct \"" + struct_type + "\" has no member named \"" + $3 + "\"");
                           }

                           identifier_astnode* id_node = new identifier_astnode($3);
                           $$ = new member_astnode($1, id_node);
                           $$->type = *(gste_ptr->symbtab->Entries.find($3)->second.type_ptr);
                        }
                    | postfix_expression PTR_OP IDENTIFIER
                        {
                           // check type (struct)
                           auto tmp_type = $1->type;
                           tmp_type.dereference();
                           std::string struct_type = tmp_type.get_type();

                           auto ste_it = globalST.Entries.find(struct_type);
                           if(ste_it == globalST.Entries.end()) {
                              error(@1, "Left operand of \"->\" is not a pointer to structure");
                           }
                           else if(ste_it->second.type_symtab != "struct") {
                              error(@1, "Left operand of \"->\" is not a pointer to structure");
                           }

                           SymbTabEntry* gste_ptr = &(ste_it->second);  // pointer to global symbtab entry
                           if(gste_ptr->symbtab->Entries.find($3) == gste_ptr->symbtab->Entries.end()) {
                              error(@1, "Struct \"" + struct_type + "\" has no member named \"" + $3 + "\"");
                           }

                           identifier_astnode* id_node = new identifier_astnode($3);
                           $$ = new arrow_astnode($1, id_node);
                           $$->type = *(gste_ptr->symbtab->Entries.find($3)->second.type_ptr);
                        }
                    | postfix_expression INC_OP
                        {
                           if($1->type.lvalue != "modifiable") {
                              // ref: Operand of \"++\" should be a int, float or pointer
                              // we give an error similar to gcc:
                              error(@1, "modifiable lvalue required as increment operand");
                           }
                           $$ = new op_unary_astnode("PP", $1);
                           $$->type = $1->type;
                           // std::cout << "pp type " << $$->type.get_type() << std::endl;
                           $$->type.lvalue = "";      // not an lvalue
                        }
  ;

  primary_expression: IDENTIFIER
                        {
                           if(localST->Entries.find($1) == localST->Entries.end()) {
                              error(@1, "Variable \""+ $1 +"\" not declared");
                           }
                           $$ = new identifier_astnode($1);
                           $$->type = *(localST->Entries.find($1)->second.type_ptr);
                        }
                    | INT_CONSTANT
                        {
                           try {
                              $$ = new intconst_astnode(stoi($1));
                              $$->type = Type("int");
                           }
                           catch(const std::exception& e) {
                              error(@$, "stoi failed with "+$1);
                           }
                        }
                    | FLOAT_CONSTANT
                        {
                           try {
                              $$ = new floatconst_astnode(stof($1));
                              $$->type = Type("float");
                           }
                           catch(const std::exception& e) {
                              error(@$, "stof failed with "+$1);
                           }
                        }
                    | STRING_LITERAL
                        {
                           $$ = new string_astnode($1);
                           $$->type = Type("string");
                        }
                    | '(' expression ')'
                        {
                           $$ = $2;       // new node not required
                        }
  ;

  expression_list: expression
                     {
                        $$ = new funcall_astnode();
                        $$->vec_exp_astnode.push_back($1);
                     }
                  | expression_list ',' expression
                     {
                        $$ = $1;
                        $$->vec_exp_astnode.push_back($3);
                     }
  ;

  unary_operator: '-' { $$ = "-"; }
                | '!' { $$ = "!"; }
                | '&' { $$ = "&"; }
                | '*' { $$ = "*"; }
  ;

  selection_statement: IF '(' expression ')' statement ELSE statement
                        {
                           $$ = new if_astnode($3, $5, $7);
                        }
  ;

  iteration_statement: WHILE '(' expression ')' statement
                        {
                           $$ = new while_astnode($3, $5);
                        }
                      | FOR '(' assignment_expression ';' expression ';' assignment_expression ')' statement
                        {
                           $$ = new for_astnode($3, $5, $7, $9);
                        }
  ;

  declaration_list: declaration
                     {
                        // $$ = new vector<decln_class*>;
                        // $$->push_back($1);

                        $$ = nullptr;
                     }
                  | declaration_list declaration
                     {
                        // $$ = $1;
                        // $$->push_back($2);

                        $$ = nullptr;
                     }
  ;

  declaration: type_specifier declarator_list ';'
                  {
                     // $$ = new decln_class();
                     // $$->base_type = $1;
                     // for(auto&& decl:(*$2)) {
                     //    $$->decls.push_back(decl);
                     // }

                     for(auto&& decl:*($2)) {
                        decl->base_type = $1;
                        if (decl->base_type=="void" && decl->stars==0) {
                           error(@2, "Cannot declare variable of type \"" + $1 + "\"");
                        }

                        // if struct, check if it's declared; but [array of ... too]
                        // pointer type to current struct allowed
                        if($1.find("struct ") == 0) {
                           auto ste_it = globalST.Entries.find($1);
                           if(ste_it == globalST.Entries.end() && !($1==curr_struct_name && decl->stars>0)) {
                                                                                    // stars_brackets won't be applicable
                                                                                    // since grammar doesn't allow that in
                                                                                    // declarations
                              error(@1, "\"" + $1 + "\" is not defined");
                           }
                        }

                        if($1.find("struct ") == 0) {
                           decl->base_type_size = globalST.Entries.find($1)->second.size;
                        }
                        else {
                           decl->base_type_size = DEFAULT_SIZE;
                        }

                        SymbTabEntry* ste_ptr = &(localST->Entries[decl->instance_name]);
                        ste_ptr->type_ptr->base_type = $1;
                        ste_ptr->size = decl->get_size();
                        if(struct_or_fn=="fn") curr_offset -= ste_ptr->size;
                        ste_ptr->offset = curr_offset;
                        if(struct_or_fn=="struct") curr_offset += ste_ptr->size;
                     }

                     $$ = nullptr;
                  }
  ;

  declarator_list: declarator
                     {
                        $$ = new vector<decl_class*>;
                        $$->push_back($1);
                     }
                  | declarator_list ',' declarator
                     {
                        $$ = $1;
                        $$->push_back($3);
                     }
  ;



%%
void IPL::Parser::error( const location_type &l, const std::string &err_message )
{
   std::cout << "Error at line " << l.begin.line << ": " << err_message << "\n";
   exit(1);
}


