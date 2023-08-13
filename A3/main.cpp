
#include "scanner.hh"
#include "parser.tab.hh"
#include "ast.hh"		// remove this later
#include "symbtab.hh"	// remove this later
#include <fstream>
#include <map>
#include <set>
using namespace std;

extern SymbTab globalST;
SymbTab gstfun, gststruct;
string filename;
extern map<string, abstract_astnode*> ast;
int curr_stack_offset;
map<string, int> map_string_label;	// string -> label
string indent;
SymbTab* curr_fn_ST_ptr;
int next_label;
bool print_asm_comments = 0;

// current function info used in return_astnode::gencode
string curr_fn_name;
int curr_fn_leave_ret_label;


int main(int argc, char **argv)
{
	using namespace std;
	fstream in_file, out_file;
	

	in_file.open(argv[1], ios::in);

	IPL::Scanner scanner(in_file);

	IPL::Parser parser(scanner);

#ifdef YYDEBUG
	parser.set_debug_level(1);
#endif
parser.parse();

next_label = 0;

// create gstfun with function entries only
for (const auto &entry : globalST.Entries) {
	if (entry.second.type_symtab == "fun")
	gstfun.Entries.insert({entry.first, entry.second});
}

// create gststruct with struct entries only
for (const auto &entry : globalST.Entries) {
	if (entry.second.type_symtab == "struct")
	gststruct.Entries.insert({entry.first, entry.second});
}

indent = "\t";

// string constants
cout << indent << ".section .rodata" << endl;
for(auto&& p: map_string_label) {
	cout << ".LC" << p.second << ":" << endl;
	cout << indent << ".string\t" << p.first << endl;
}

for (auto it = gstfun.Entries.begin(); it != gstfun.Entries.end(); ++it) {
	curr_fn_name = it->first;
	curr_fn_ST_ptr = it->second.symbtab;

	cout << indent << ".text" << endl;
	cout << indent << ".globl\t" << it->first << endl;
	cout << indent << ".type\t" << it->first << ", @function" << endl;
	cout << it->first << ":" << endl;
	
	// initial setup
	cout << indent << "pushl\t%ebp" << endl;
	cout << indent << "movl\t%esp, %ebp" << endl;
	curr_stack_offset = 0;		// as ebp = esp

	// create space for locals
	int space = 0;
	for(auto&& e: it->second.symbtab->Entries) {
		if(e.second.scope == "local") {
			space += e.second.size;
		}
	}
	if(space > 0) {
		cout << indent << "subl\t$" << space << ", %esp" << endl;
		curr_stack_offset -= space;
	}

	curr_fn_leave_ret_label = next_label;
	next_label += 1;

	ast[it->first]->gencode();

	cout << ".L" << curr_fn_leave_ret_label << ":" << endl;
	if(curr_fn_name == "main") {
		cout << indent << "movl\t$0, %eax" << endl;
	}
	cout << indent << "leave" << endl;
	cout << indent << "ret" << endl;

	cout << indent << ".size\t" << it->first << ", .-" << it->first << endl;
}

	fclose(stdout);
}