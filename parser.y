/* File: parser.y
 * --------------
 * Bison input file to generate the parser for the compiler.
 *
 * pp2: your job is to write a parser that will construct the parse tree
 *      and if no parse errors were found, print it.  The parser should
 *      accept the language as described in specification, and as augmented
 *      in the pp2 handout.
 */

%{

/* Just like lex, the text within this first region delimited by %{ and %}
 * is assumed to be C/C++ code and will be copied verbatim to the y.tab.c
 * file ahead of the definitions of the yyparse() function. Add other header
 * file inclusions or C++ variable declarations/prototypes that are needed
 * by your code here.
 */
#include "scanner.h" // for yylex
#include "parser.h"
#include "errors.h"

void yyerror(const char *msg); // standard error-handling routine

%}

/* The section before the first %% is the Definitions section of the yacc
 * input file. Here is where you declare tokens and types, add precedence
 * and associativity options, and so on.
 */

/* yylval
 * ------
 * Here we define the type of the yylval global variable that is used by
 * the scanner to store attibute information about the token just scanned
 * and thus communicate that information to the parser.
 *
 * pp2: You will need to add new fields to this union as you add different
 *      attributes to your non-terminal symbols.
 */
%union {
    int integerConstant;
    bool boolConstant;
    char *stringConstant;
    double doubleConstant;
    char identifier[MaxIdentLen+1]; // +1 for terminating null
    Decl *decl;
    List<Decl*> *declList;
}


/* Tokens
 * ------
 * Here we tell yacc about all the token types that we are using.
 * Bison will assign unique numbers to these and export the #define
 * in the generated y.tab.h header file.
 */
%token   T_Void T_Bool T_Int T_Double T_String T_Class
%token   T_LessEqual T_GreaterEqual T_Equal T_NotEqual T_Dims
%token   T_And T_Or T_Null T_Extends T_This T_Interface T_Implements
%token   T_While T_For T_If T_Else T_Return T_Break
%token   T_New T_NewArray T_Print T_ReadInteger T_ReadLine

%token   <identifier> T_Identifier
%token   <stringConstant> T_StringConstant
%token   <integerConstant> T_IntConstant
%token   <doubleConstant> T_DoubleConstant
%token   <boolConstant> T_BoolConstant


/* Non-terminal types
 * ------------------
 * In order for yacc to assign/access the correct field of $$, $1, we
 * must to declare which field is appropriate for the non-terminal.
 * As an example, this first type declaration establishes that the DeclList
 * non-terminal uses the field named "declList" in the yylval union. This
 * means that when we are setting $$ for a reduction for DeclList ore reading
 * $n which corresponds to a DeclList nonterminal we are accessing the field
 * of the union named "declList" which is of type List<Decl*>.
 * pp2: You'll need to add many of these of your own.
 */
%type <declList>  DeclP
%type <decl>      Decl

/* Associate the 'else' porition of if statements with the nearest (innermost)
 * 'if' porition.
 */
%nonassoc NoElse
%nonassoc T_Else

%right '='
%left  T_Or
%left  T_And
%left  T_Equal
%left  T_NotEqual
%left  '<'
%left  T_LessEqual
%left  '>'
%left  T_GreaterEqual
%left  '-'
%left  '+'
%left  '/'
%left  '%'
%left  '*'
%right '!'
%left  '.'
%left  '['
%left  ']'
%left  T_Dims

%%
/* Rules
 * -----
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.

 */

/* Postfixes
 * ---------
 * Some of the non terminals in the grammer have one or more post fixed capital
 * letters. These are intentional and have meaning. Each letter corresponds to
 * the following:
 *     P : Plus     (i.e. 'E+', One or more E)
 *     C : Comma    (i.e. ',' , Used in Conjunction with P (PC) to denote 'E+,'
 *                   One or more E separated by commas)
 *     S : Star     (i.e. 'E*', Zero or more E)
 *     O : Optional (i.e. 'E?', Zero or One E. E is optional)
 */

Program   :    DeclP                {
                                      @1;
                                      /* pp2: The @1 is needed to convince
                                       * yacc to set up yylloc. You can remove
                                       * it once you have other uses of @n*/
                                      Program *program = new Program($1);
                                      // if no errors, advance to next phase
                                      if (ReportError::NumErrors() == 0)
                                          program->Print(0);
                                    }
          ;

DeclP     :    DeclP Decl           { ($$=$1)->Append($2); }
          |    Decl                 { ($$ = new List<Decl*>)->Append($1); }
          ;

Decl      :    VarDecl              { /* pp2: replace with correct rules */ }
          |    FnDecl
          |    ClassDecl
          |    InterfaceDecl
          ;

VarDecl   :    Var ';'
          ;

Var       :    Type T_Identifier
          ;

Type      :    T_Int
          |    T_Double
          |    T_Bool
          |    T_String
          |    T_Identifier
          |    Type T_Dims
          ;

FnDecl    :    Type   T_Identifier '(' Formals ')' StmtBlock
          |    T_Void T_Identifier '(' Formals ')' StmtBlock
          ;

VarPC     :    VarPC ',' Var
          |    Var
          ;

Formals   :    VarPC
          |
          ;

IdentifierPC : IdentifierPC ',' T_Identifier
          |    T_Identifier
          ;

ExtendsO  :    T_Extends T_Identifier
          |
          ;

ImplementsO :  T_Implements IdentifierPC
          |
          ;

FieldS    :    FieldS Field
          |
          ;

ClassDecl :    T_Class T_Identifier ExtendsO ImplementsO '{' FieldS '}'
          ;

Field     :    VarDecl
          |    FnDecl
          ;

PrototypeS :   PrototypeS Prototype
          |
          ;

InterfaceDecl : T_Interface T_Identifier '{' PrototypeS '}'
          ;

Prototype :    Type T_Identifier '(' Formals ')' ';'
          |    T_Void T_Identifier '(' Formals ')' ';'
          ;

StmtS     :    StmtS Stmt
          |
          ;

VarDeclS  :    VarDeclS VarDecl
          |
          ;

StmtBlock :    '{' VarDeclS StmtS '}'
          ;

ExprO     :    Expr
          |
          ;

Stmt      :    ExprO ';'
          |    IfStmt
          |    WhileStmt
          |    ForStmt
          |    BreakStmt
          |    ReturnStmt
          |    PrintStmt
          |    StmtBlock
          ;

ElseO     :    T_Else Stmt %prec T_Else
          |                %prec NoElse
          ;

IfStmt    :    T_If '(' Expr ')' Stmt ElseO
          ;

WhileStmt :    T_While '(' Expr ')' Stmt
          ;

ForStmt   :    T_For '(' ExprO ';' Expr ';' ExprO ')' Stmt
          ;

ReturnStmt :   T_Return ExprO ';'
          ;

BreakStmt :    T_Break ';'
          ;

ExprPC    :    ExprPC ',' Expr
          |    Expr
          ;

PrintStmt :    T_Print '(' ExprPC ')' ';'
          ;

Expr      :    LValue '=' Expr
          |    Constant
          |    LValue
          |    T_This
          |    Call
          |    '(' Expr ')'
          |    Expr '+' Expr
          |    Expr '-' Expr
          |    Expr '*' Expr
          |    Expr '/' Expr
          |    Expr '%' Expr
          |    '-' Expr
          |    Expr '<' Expr
          |    Expr T_LessEqual Expr
          |    Expr '>' Expr
          |    Expr T_GreaterEqual Expr
          |    Expr T_Equal Expr
          |    Expr T_NotEqual Expr
          |    Expr T_And Expr
          |    Expr T_Or Expr
          |    '!' Expr
          |    T_ReadInteger '()'
          |    T_ReadLine '()'
          |    T_New T_Identifier
          |    T_NewArray '(' Expr ',' Type ')'
          ;

LValue    :    T_Identifier
          |    Expr '.' T_Identifier
          |    Expr '[' Expr ']'
          ;

Call      :    T_Identifier '(' Actuals ')'
          |    Expr '.' T_Identifier '(' Actuals ')'
          ;

Actuals   :    ExprPC
          |
          ;

Constant  :    T_IntConstant
          |    T_DoubleConstant
          |    T_BoolConstant
          |    T_StringConstant
          |    T_Null
          ;

%%

/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated y.tab.c file.
 * This section is where you put definitions of helper functions.
 */

/* Function: InitParser
 * --------------------
 * This function will be called before any calls to yyparse().  It is designed
 * to give you an opportunity to do anything that must be done to initialize
 * the parser (set global variables, configure starting state, etc.). One
 * thing it already does for you is assign the value of the global variable
 * yydebug that controls whether yacc prints debugging information about
 * parser actions (shift/reduce) and contents of state stack during parser.
 * If set to false, no information is printed. Setting it to true will give
 * you a running trail that might be helpful when debugging your parser.
 * Please be sure the variable is set to false when submitting your final
 * version.
 */
void InitParser()
{
   PrintDebug("parser", "Initializing parser");
   yydebug = false;
}
