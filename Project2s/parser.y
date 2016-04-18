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
    unsigned int uintConstant;
    bool boolConstant;
    float floatConstant;
    char identifier[MaxIdentLen+1]; // +1 for terminating null
    Decl *decl;
    VarDecl *vardecl;
    List<Decl*> *declList;
    Type * type;
    TypeQualifier * typequal;
    Expr * expr;
}


/* Tokens
 * ------
 * Here we tell yacc about all the token types that we are using.
 * Bison will assign unique numbers to these and export the #define
 * in the generated y.tab.h header file.
 */
%token   T_Void T_Bool T_Int T_Float
%token   T_LessEqual T_GreaterEqual T_EQ T_NE T_LeftAngle T_RightAngle
%token   T_And T_Or
%token   T_Equal T_MulAssign T_DivAssign T_AddAssign T_SubAssign
%token   T_While T_For T_If T_Else T_Return T_Break
%token   T_Const T_Uniform T_Layout T_Continue T_Do
%token   T_Inc T_Dec T_Switch T_Case T_Default
%token   T_In T_Out T_InOut
%token   T_Mat2 T_Mat3 T_Mat4 T_Vec2 T_Vec3 T_Vec4
%token   T_Ivec2 T_Ivec3 T_Ivec4 T_Bvec2 T_Bvec3 T_Bvec4
%token   T_Uint T_Uvec2 T_Uvec3 T_Uvec4 T_Struct
%token   T_Semicolon T_Dot T_Colon T_Question T_Comma
%token   T_Dash T_Plus T_Star T_Slash
%token   T_LeftParen T_RightParen T_LeftBracket T_RightBracket T_LeftBrace T_RightBrace

%token   <identifier> T_Identifier
%token   <identifier> T_FieldSelect
%token   <integerConstant> T_IntConstant
%token   <floatConstant> T_FloatConstant
%token   <boolConstant> T_BoolConstant
%token   <uintConstant> T_UintConstant

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
%type <declList>    DeclList
%type <decl>        Decl
%type <vardecl>     VarDecl
%type <type>        TypeLiteral
%type <type>        TypeSpecifier
%type <typequal>    TypeQualifier
%type <expr>        PrimaryExpression
%type <expr>        Expression



%%
/* Rules
 * -----
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.

 */
Program   :    DeclList            {
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

DeclList  :    DeclList Decl        { ($$=$1)->Append($2); }
          |    Decl                 { ($$ = new List<Decl*>)->Append($1); }
          ;

Decl      : VarDecl T_Semicolon {$$ = $1;}
          ;
VarDecl         :   TypeSpecifier T_Identifier    {
                                Identifier *id = new
                                Identifier(@2, $2);
                                $$ = new VarDecl(id, $1);
                            }
	            |	TypeQualifier TypeSpecifier T_Identifier    {
                    Identifier *id = new Identifier(@3, $3);
                    $$ = new VarDecl(id, $2, $1);}
                |   TypeSpecifier  T_Identifier T_LeftBracket ConditionalExpression T_RightBracket 
                {
                    Identifier * id = new Identifier(@2, $2);
                    Type * type = new ArrayType(@1, $1);
                    $$ = new VarDecl(id, type);
                }
                |   TypeQualifier TypeSpecifier T_Identifier T_LeftBracket ConditionalExpression T_RightBracket
                {
                    Identifier * id = new Identifier(@3, $3);
                    Type * type = new ArrayType(@2, $2);
                    $$ = new VarDecl(id, type, $1);
                }
	            |   TypeSpecifier  T_Identifier T_Equal Expression 
	            |   TypeQualifier TypeSpecifier T_Identifier T_Equal Expression 
                ;

TypeQualifier    :   T_Const    {$$ = TypeQualifier::constTypeQualifier;}    
                 |   T_In   {$$ = TypeQualifier::inTypeQualifier;} 
                 |   T_Out  {$$ = TypeQualifier::outTypeQualifier;} 
                 |   T_Uniform  {$$ = TypeQualifier::uniformTypeQualifier;} 
                 ;

TypeSpecifier   :   TypeLiteral {$$ = $1;}
                |   TypeLiteral T_LeftBracket ConditionalExpression T_RightBracket
                {
                    $$ = $1;
                }
                ;

TypeLiteral :   T_Void  {$$ = Type::voidType;}
            |   T_Bool  {$$ = Type::boolType;}
            |   T_Int   {$$ = Type::intType;}
            |   T_Float {$$ = Type::floatType;}
            |   T_Mat2   {$$ = Type::mat2Type;}
            |   T_Mat3   {$$ = Type::mat3Type;}
            |   T_Mat4   {$$ = Type::mat4Type;}
            |   T_Vec2   {$$ = Type::vec2Type;}
            |   T_Vec3   {$$ = Type::vec3Type;}
            |   T_Vec4   {$$ = Type::vec4Type;}
            |   T_Ivec2   {$$ = Type::ivec2Type;}
            |   T_Ivec3   {$$ = Type::ivec3Type;}
            |   T_Ivec4   {$$ = Type::ivec4Type;}
            |   T_Bvec2   {$$ = Type::bvec2Type;}
            |   T_Bvec3   {$$ = Type::bvec3Type;}
            |   T_Bvec4   {$$ = Type::bvec4Type;}
            |   T_Uint   {$$ = Type::uintType;}
            |   T_Uvec2   {$$ = Type::uvec2Type;}
            |   T_Uvec3   {$$ = Type::uvec3Type;}
            |   T_Uvec4   {$$ = Type::uvec4Type;}
            ;

PrimaryExpression   :   T_Identifier
                    |   T_IntConstant   {$$ = new IntConstant(@1, $1);}
                    |   T_FloatConstant {$$ = new FloatConstant(@1, $1);}
                    |   T_BoolConstant  {$$ = new BoolConstant(@1, $1);}
                    |   T_LeftParen Expression T_RightParen {$$ = $2;}
                    ;

Expression  :   ConditionalExpression   
            {
                
            }
            |   UnaryExpression AssignmentOperator Expression
            {
                $$ = new AssignExpr($1, $2, $3);
            }
            ;

ConditionalExpression   :   OrExpression
                        |   OrExpression T_Question Expression T_Colon Expression
                        ;

OrExpression    :   AndExpression
                |   OrExpression T_Or AndExpression

AndExpression   :   EqualityExpression
                |   AndExpression T_And EqualityExpression
                ;

EqualityExpression  :   RelationalExpression
                    |   EqualityExpression T_EQ RelationalExpression
                    |   EqualityExpression T_NE RelationalExpression
                    ;

RelationalExpression    :   AdditiveExpression
                        |   RelationalExpression T_LeftAngle AdditiveExpression
                        |   RelationalExpression T_RightAngle AdditiveExpression
                        |   RelationalExpression T_LessEqual AdditiveExpression
                        |   RelationalExpression T_GreaterEqual AdditiveExpression
                        ;

AdditiveExpression  :   MultiplicativeExpr
                    |   AdditiveExpression T_Plus MultiplicativeExpr
                    |   AdditiveExpression T_Dash MultiplicativeExpr
                    ;

MultiplicativeExpr  :   UnaryExpression
                    |   MultiplicativeExpr T_Star UnaryExpression
                    |   MultiplicativeExpr T_Slash UnaryExpression
                    ;

UnaryExpression :   PostfixExpression
                |   T_Inc UnaryExpression
                |   T_Dec UnaryExpression
                |   T_Plus UnaryExpression
                |   T_Dash UnaryExpression
                ;

PostfixExpression   :   PrimaryExpression
                    |   PostfixExpression T_LeftBracket Expression T_RightBracket
                    |   FunctionCall
                    |   PostfixExpression T_Dot T_FieldSelect
                    |   PostfixExpression T_Inc
                    |   PostfixExpression T_Dec
                    ;

FunctionCall    :   FunctionCallParams T_RightParen
                |   FunctionCallNoParams T_RightParen
                ;

FunctionCallParams  :   FunctionIdentifier T_LeftParen Expression
                    |   FunctionCallParams T_Comma Expression
                    ;

FunctionIdentifier  :   TypeSpecifier
                    |   PostfixExpression
                    ;

FunctionCallNoParams    :   FunctionIdentifier T_LeftParen T_Void
                        |   FunctionIdentifier T_LeftParen
                        ;

AssignmentOperator  :   T_Equal {$$ = new Operator(@1, "=");}
                    |   T_MulAssign {$$ = new Operator(@1, "*=");}
                    |   T_DivAssign {$$ = new Operator(@1, "/=");}
                    |   T_AddAssign {$$ = new Operator(@1, "+=");}
                    |   T_SubAssign {$$ = new Operator(@1, "-=");}
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
