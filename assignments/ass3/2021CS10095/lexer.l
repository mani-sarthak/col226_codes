%{
#include <stdio.h>
%}

%option noyywrap

DIGIT [0-9]
SMALL [a-z_]
CAP    [A-Z]
LETTER {SMALL}|{CAP}
ID ({SMALL})({LETTER}|{DIGIT}|['])*
NATNUM [1-9]{DIGIT}*
INT {NATNUM}|0
BOOL (true|false)
STRING \"([^\"]*)\"


%%
"let"        { printf("Keyword -> LET\n"); }       
"in"         { printf("Keyword -> IN\n"); }       
"and"        { printf("Keyword -> AND\n"); }
"if"         { printf("Keyword -> IF\n"); }        
"then"       { printf("Keyword -> THEN\n"); }      
"else"       { printf("Keyword -> ELSE\n"); }      
"match"      { printf("Keyword -> MATCH\n"); }     
"with"       { printf("Keyword -> WITH\n"); }      
"function"   { printf("Keyword -> FUNCTION\n"); }   
"pair"       { printf("Keyword -> PAIR\n"); }
"fst"        { printf("Keyword -> FST\n"); }
"snd"        { printf("Keyword -> SND\n"); }
{ID}         { printf("IDENTIFIER(%s)\n", yytext); }
{INT}        { printf("INTEGER(%s)\n", yytext); }
{BOOL}       { printf("BOOL(%s)\n", yytext); }
{STRING}     { printf("STRING(%s)\n", yytext + 1); }
"+"          { printf("Arithmetic Operation -> ADD\n"); }
"-"          { printf("Arithmetic Operation -> SUBTRACT\n"); }
"*"          { printf("Arithmetic Operation -> MULTIPLY\n"); }
"/"          { printf("Arithmetic Operation -> DIVIDE\n"); }
"&&"         { printf("Binary Operation -> AND\n"); }
"||"         { printf("Binary Operation -> OR\n"); }
"!"          { printf("Binary Operation -> NOT\n"); }
"="          { printf("EQUAL(Assignment)\n"); }
"!="         { printf("comparison Operation -> NOT_EQUAL\n"); }
"<"          { printf("comparison Operation -> LESS_THAN\n"); }
">"          { printf("comparison Operation -> GREATER_THAN\n"); }
"<="         { printf("comparison Operation -> LESS_THAN_OR_EQUAL\n"); }
">="         { printf("comparison Operation -> GREATER_THAN_OR_EQUAL\n"); }
"("          { printf("Parenthesis -> OPEN_PAREN\n"); }
")"          { printf("Parenthesis -> CLOSE_PAREN\n"); }
"{"          { printf("OPEN_BRACE\n"); }
"}"          { printf("CLOSE_BRACE\n"); }
";"          { printf("SEMICOLON\n"); }
","          { printf("COMMA\n"); }
"^"          { printf("String Operation -> CONCATENATION\n"); }   
[ \t\n]+     ; // Skip whitespaces
"(*"         { while(yytext[0] != '*' || yytext[1] != ')') yylex(); } 
"(*"([^*)]|\n|\r)*"*)" {/* Skip single-line comment */ }
.            { printf("UNKNOWN(%s)\n", yytext); }
%%

int main() {
    yylex();
    return 0;
}
