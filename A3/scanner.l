%{
// This part goes directly into the generated parser

#include "scanner.hh"
#include "parser.tab.hh"

// YY_DECL is bound to the type of the scanner. The default is 
// #define YY_DECL int Scanner::yylex()
// which is generated anyway.

//If you want the type of the generated scanner to be different, do this

#undef  YY_DECL
#define YY_DECL int IPL::Scanner::yylex( IPL::Parser::semantic_type * const lval, IPL::Parser::location_type *loc )

//We want these to be the first action after a token is recognized. 

#define YY_USER_ACTION loc->step(); loc->columns(yyleng);

// token_type is a short hand for IPL::Parser::token

using token_type = IPL::Parser::token;
// #define token_type IPL::Parser::token

%}


    /* %option debug */
%option nodefault
%option yyclass="Scanner"
%option noyywrap
%option c++

DIGIT [0-9]

ID [a-zA-Z_][a-zA-Z0-9_]*
    /* A single letter or _  followed by 0 or more letters or _ or digits */

WS [ \t\n]
    /* whitespace */

INT_CONSTANT ([1-9][0-9]*|0[bB][01]*|0[0-7]*|0[xX][0-9a-fA-F]*)
    /*
        10329
        0b10110101011
        06261277
        0xa0322aa1bf
        0x
        0101
        ---
        0b1213 [syntax error - binary followed by decimal]
    */

FLOAT_CONSTANT ([0-9]+[\.][0-9]*|[\.][0-9]+)([eE][+-]?[0-9]+)?
    /*
        grammar file; some checked by reference:
        1.
        23.1
        01.456
        12.e45
        12.45e12
        12.45e-12
        12.45e+12
        1.e10
        1.e+10
        1.e-10
        001.1110e10
        .1
        .1e10
        -00000.1e-10 (starting "-" not part of FLOAT_CONSTANT)
        0.e-10
        ---
        incorrect (checked by reference implementation):
        234
        .
        1e10
        .e10
        0.e
        0.e-
        ---
        NOTE:
        '-' of "-xyz.abc" is covered in unary_operator
    */
/*
Everything else
*/
%x IN_COMMENT

%%

%{
    /** Code executed at the beginning of yylex **/
    yylval = lval;
%}


[ \t] {}
[\n] {
    loc->lines();
}
    /* skip whitespace */

void {
    yylval->emplace<std::string>(YYText());
    return token_type::VOID;
}
int {
    yylval->emplace<std::string>(YYText());
    return token_type::INT;
}
float {
    yylval->emplace<std::string>(YYText());
    return token_type::FLOAT;
}
struct {
    // std::cout << "struct found in scanner\n";
    yylval->emplace<std::string>(YYText());
    return token_type::STRUCT;
}

{INT_CONSTANT} {
    yylval->emplace<std::string>(YYText());
    return token_type::INT_CONSTANT;
}
{FLOAT_CONSTANT} {
    yylval->emplace<std::string>(YYText());
    return token_type::FLOAT_CONSTANT;
}

return {
    yylval->emplace<std::string>(YYText());
    return token_type::RETURN;
}

"||" {
    yylval->emplace<std::string>(YYText());
    return token_type::OR_OP;
}
"&&" {
    yylval->emplace<std::string>(YYText());
    return token_type::AND_OP;
}
"==" {
    yylval->emplace<std::string>(YYText());
    return token_type::EQ_OP;
}
"!=" {
    yylval->emplace<std::string>(YYText());
    return token_type::NE_OP;
}
"<=" {
    yylval->emplace<std::string>(YYText());
    return token_type::LE_OP;
}
">=" {
    yylval->emplace<std::string>(YYText());
    return token_type::GE_OP;
}
"++" {
    yylval->emplace<std::string>(YYText());
    return token_type::INC_OP;
}
"->" {
    yylval->emplace<std::string>(YYText());
    return token_type::PTR_OP;
}

if {
    yylval->emplace<std::string>(YYText());
    return token_type::IF;
}
else {
    yylval->emplace<std::string>(YYText());
    return token_type::ELSE;
}

while {
    yylval->emplace<std::string>(YYText());
    return token_type::WHILE;
}
for {
    yylval->emplace<std::string>(YYText());
    return token_type::FOR;
}

{ID} {
    //std::cout << "identifier " << YYText() << " found in scanner\n";
    yylval->emplace<std::string>(YYText());
    return token_type::IDENTIFIER;
}

\"(\\[tn\\"]|[^\\"\n])*\" {
    //std::cout << "string literal " << YYText() << " found in scanner\n";
    yylval->emplace<std::string>(YYText());
    return token_type::STRING_LITERAL;
    /*
    Tests (in vscode, don't escape first and last quotes)
        "\t\n\\..ab\n\ncd\"\"..!@"
        ""
        "this is accepted"
        ---
        "\t\c"
        "this is
        not accepted"
    */
}

"+" return '+';
"-" return '-';
"*" return '*';
"/" return '/';
"=" return '=';
">" return '>';
"<" return '<';
"(" return '(';
")" return ')';
"{" return '{';
"}" return '}';
"[" return '[';
"]" return ']';
"," return ',';
"." return '.';
";" return ';';
"!" return '!';
"&" return '&';

    /* comments: flex-manual; also increment #lines here */

"/*" BEGIN(IN_COMMENT);
<IN_COMMENT>{
    "*/" {BEGIN(INITIAL);}
    [^*\n]+ {}
    "*" {}
    [\n] {
        /* loc->lines(); */
        /* should increment lines, but not doing so due to incorrect ref. impl. */
    }
}

<<EOF>> yyterminate();

. {
    yylval->emplace<std::string>(YYText());
     return token_type::OTHERS;
}


%%

