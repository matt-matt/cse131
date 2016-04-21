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
    FnDecl *fndecl;
    List<Decl*> *declList;
    List<Stmt*> *stmtList; 
    Type * type;
    TypeQualifier * typequal;
    Expr * expr;
    Operator *op;
    funcargs fnargs;
    funcinvoke fninvk;
    Stmt * stmt; 
    StmtBlock *stmtblock;
    Identifier * wrapper;
    forloop fl;
    List<VarDecl*> * vdclist;
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
%type <declList>    TranslationUnit
%type <decl>        ExternalDecl
%type <decl>        Decl
%type <vardecl>     VarDecl
%type <type>        TypeLiteral
%type <type>        TypeSpecifier
%type <typequal>    TypeQualifier
%type <expr>        PrimaryExpression
%type <expr>        Expression
%type <expr>        PostfixExpression
%type <expr> 	    UnaryExpression
%type <expr>        OrExpression
%type <expr>        AndExpression
%type <expr>	    EqualityExpression 
%type <expr>        RelationalExpression
%type <expr>        AdditiveExpression 
%type <expr>        MultiplicativeExpr 
%type <expr>        ConditionalExpression 
%type <expr>        Conditionopt 
%type <stmt>        JumpStatement
%type <fndecl>      FunctionPrototype
%type <op>          AssignmentOperator
%type <fnargs>      FunctionHeader
%type <fnargs>      FunctionHeaderParam
%type <vardecl>     ParamDeclaration
%type <stmtList>    StatementList 
%type <stmt>	    Statement
%type <stmt>	    SimpleStatement
%type <stmt>        SelectionStatement
%type <stmt>        IterationStatement
%type <expr>        FunctionCall
%type <fninvk>      FunctionCallParams
%type <fninvk>      FunctionCallNoParams
%type <wrapper>     FunctionIdentifier
%type <expr>        ForInitStatement
%type <fl>  	    ForRestStatement
%type <stmtblock>   StatementScope 
%type <stmtblock>   StatementNoScope
%type <stmtblock>   CompoundScope
%type <stmtblock>   CompoundNoScope
%type <vdclist>     SingleDeclList
%type <fndecl>      FunctionDefinition
%nonassoc "If" 
%nonassoc T_Else 

%%
/* Rules
 * -----
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.
 */
Program   :    TranslationUnit            {
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

TranslationUnit  :    TranslationUnit ExternalDecl        { ($$=$1)->Append($2); }
          |    ExternalDecl                 { ($$ = new List<Decl*>)->Append($1); }
          ;

Decl      : VarDecl T_Semicolon {$$ = $1;}
          | FunctionPrototype T_Semicolon {$$ = $1;}
          | TypeQualifier T_Identifier T_Semicolon
          {
              Identifier * id = new Identifier(@2, $2);
              $$ = new VarDecl(id, $1);
          }
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
                {
                    Identifier *id = new Identifier(@2, $2);
                    $$ = new VarDecl(id, $1, NULL, $4);
                }
	            |   TypeQualifier TypeSpecifier T_Identifier T_Equal Expression 
                {
                    Identifier *id = new Identifier(@3, $3);
                    $$ = new VarDecl(id, $2, $1, $5);
                }
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
                    {
                        Identifier * id = new Identifier(@1, $1);
                        $$ = new VarExpr(@1, id);
                    }
                    |   T_IntConstant   {$$ = new IntConstant(@1, $1);}
                    |   T_FloatConstant {$$ = new FloatConstant(@1, $1);}
                    |   T_BoolConstant  {$$ = new BoolConstant(@1, $1);}
                    |   T_LeftParen Expression T_RightParen {$$ = $2;}
                    ;

FunctionPrototype   :   FunctionHeader T_RightParen
                    {
                        $$ = new FnDecl($1.id, $1.type, new List<VarDecl*>());
                    }
		            |   FunctionHeaderParam T_RightParen 
                    {
                        $$ = new FnDecl($1.id, $1.type, $1.tq, $1.params);
                    }
		            ;

FunctionHeader	    :   TypeSpecifier T_Identifier T_LeftParen
                    {
                        Identifier * id = new Identifier(@2, $2);
                        $$.id = id;
                        $$.type = $1;
                        $$.tq = NULL;
                        $$.params = new List<VarDecl*>;
                    }
                    |   TypeQualifier TypeSpecifier T_Identifier T_LeftParen
                    {
                        Identifier * id = new Identifier(@3, $3);
                        $$.id = id;
                        $$.type = $2;
                        $$.tq = $1;
                        $$.params = new List<VarDecl*>;

                    }
		            ;

FunctionHeaderParam :   FunctionHeader ParamDeclaration
                    {
                        $$.id = $1.id;
                        $$.type = $1.type;
                        $$.tq = $1.tq;
                        ($$.params = $1.params)->Append($2);
                    } 
                    |   FunctionHeaderParam T_Comma ParamDeclaration
                    {
                        $$.id = $1.id;
                        $$.type = $1.type;
                        $$.tq = $1.tq;
                        ($$.params = $1.params)->Append($3);
                    }
                    ; 

ParamDeclaration    :   TypeSpecifier T_Identifier
                    {
                        Identifier *id = new Identifier(@2, $2);
                        $$ = new VarDecl(id, $1);
                    }
		            ;  

Expression  :   ConditionalExpression {$$ = $1;}
            |   UnaryExpression AssignmentOperator Expression {$$ = new AssignExpr($1, $2, $3);}
            ;

ConditionalExpression   :   OrExpression {$$ = $1;}
                        |   OrExpression T_Question Expression T_Colon Expression
                        ;

OrExpression    :   AndExpression {$$ = $1;}
                |   OrExpression T_Or AndExpression {$$ = new LogicalExpr($1, new Operator(@2, "||"), $3);}
                ;
                
AndExpression   :   EqualityExpression {$$ = $1;}
                |   AndExpression T_And EqualityExpression {$$ = new LogicalExpr($1, new Operator(@2, "&&"), $3);}
                ;

EqualityExpression  :   RelationalExpression {$$ = $1;}
                    |   EqualityExpression T_EQ RelationalExpression {$$ = new EqualityExpr($1, new Operator(@2, "=="), $3);}
                    |   EqualityExpression T_NE RelationalExpression {$$ = new EqualityExpr($1, new Operator(@2, "!="), $3);}
                    ;

RelationalExpression    :   AdditiveExpression {$$ = $1;}
                        |   RelationalExpression T_LeftAngle AdditiveExpression    {$$ = new RelationalExpr($1, new Operator(@2, "<"), $3);}
                        |   RelationalExpression T_RightAngle AdditiveExpression   {$$ = new RelationalExpr($1, new Operator(@2, ">"), $3);}
                        |   RelationalExpression T_LessEqual AdditiveExpression    {$$ = new RelationalExpr($1, new Operator(@2, "<="), $3);}
                        |   RelationalExpression T_GreaterEqual AdditiveExpression {$$ = new RelationalExpr($1, new Operator(@2, ">="), $3);}
                        ;

AdditiveExpression  :   MultiplicativeExpr {$$ = $1;}
                    |   AdditiveExpression T_Plus MultiplicativeExpr {$$ = new ArithmeticExpr($1, new Operator(@2, "+"), $3);}
                    |   AdditiveExpression T_Dash MultiplicativeExpr {$$ = new ArithmeticExpr($1, new Operator(@2, "-"), $3);}
                    ;

MultiplicativeExpr  :   UnaryExpression {$$ = $1;}
                    |   MultiplicativeExpr T_Star UnaryExpression  {$$ = new ArithmeticExpr($1, new Operator(@2, "*"), $3);}
                    |   MultiplicativeExpr T_Slash UnaryExpression {$$ = new ArithmeticExpr($1, new Operator(@2, "/"), $3);}
                    ;

UnaryExpression :   PostfixExpression {$$ = $1;}
                |   T_Inc UnaryExpression   {$$ = new ArithmeticExpr(new Operator(@1, "++"), $2);}
                |   T_Dec UnaryExpression   {$$ = new ArithmeticExpr(new Operator(@1, "--"), $2);}
                |   T_Plus UnaryExpression  {$$ = new ArithmeticExpr(new Operator(@1, "+"), $2);}
                |   T_Dash UnaryExpression  {$$ = new ArithmeticExpr(new Operator(@1, "-"), $2);}
                ;

PostfixExpression   :   PrimaryExpression {$$ = $1;}
                    |   PostfixExpression T_LeftBracket Expression T_RightBracket {$$ = new ArrayAccess(@1, $1, $3);}
                    |   FunctionCall {$$ = $1;}
                    |   PostfixExpression T_Dot T_FieldSelect {$$ = new FieldAccess($1, new Identifier(@3, $3));}
                    |   PostfixExpression T_Inc {$$ = new PostfixExpr($1, new Operator(@2, "++"));}
                    |   PostfixExpression T_Dec {$$ = new PostfixExpr($1, new Operator(@2, "--"));}
                    ;

FunctionCall    :   FunctionCallParams T_RightParen
                {
                    $$ = new Call(@1, NULL, $1.field, $1.args);
                }
                |   FunctionCallNoParams T_RightParen
                {
                    $$ = new Call(@1, NULL, $1.field, NULL);
                }
                ;

FunctionCallParams  :   FunctionIdentifier T_LeftParen Expression
                    {
                        $$.field = $1;
                        ($$.args = new List<Expr*>)->Append($3);
                    }
                    |   FunctionCallParams T_Comma Expression
                    {
                        ($$.args=$1.args)->Append($3);
                    }
                    ;

FunctionCallNoParams    :   FunctionIdentifier T_LeftParen T_Void
                        {
                            $$.field = $1;
                        }
                        |   FunctionIdentifier T_LeftParen
                        {
                            $$.field = $1;
                        }
                        ;

FunctionIdentifier  :   T_Mat2
                    {
                        $$ = new Identifier(@1, "mat2");
                    }
                    |   T_Mat3
                    {
                        $$ = new Identifier(@1, "mat3");
                    }
                    |   T_Mat4
                    {
                        $$ = new Identifier(@1, "mat4");
                    }
                    |   T_Vec2
                    {
                        $$ = new Identifier(@1, "vec2");
                    }
                    |   T_Vec3 
                    {
                        $$ = new Identifier(@1, "vec3");
                    }
                    |   T_Vec4
                    {
                        $$ = new Identifier(@1, "vec4");
                    }
                    |   T_Ivec2
                    {
                        $$ = new Identifier(@1, "ivec2");
                    }
                    |   T_Ivec3
                    {
                        $$ = new Identifier(@1, "ivec3");
                    }
                    |   T_Ivec4
                    {
                        $$ = new Identifier(@1, "ivec4");
                    }
                    |   T_Bvec2
                    {
                        $$ = new Identifier(@1, "bvec2");
                    }
                    |   T_Bvec3
                    {
                        $$ = new Identifier(@1, "bvec3");
                    }
                    |   T_Bvec4
                    {
                        $$ = new Identifier(@1, "bvec4");
                    }
                    |   T_Uvec2
                    {
                        $$ = new Identifier(@1, "uvec2");
                    }
                    |   T_Uvec3
                    {
                        $$ = new Identifier(@1, "uvec3");
                    }
                    |   T_Uvec4
                    {
                        $$ = new Identifier(@1, "uvec4");
                    }

                    |   T_Identifier
                    {
                        $$ = new Identifier(@1, $1);
                    }
                    ;

AssignmentOperator  :   T_Equal {$$ = new Operator(@1, "=");}
                    |   T_MulAssign {$$ = new Operator(@1, "*=");}
                    |   T_DivAssign {$$ = new Operator(@1, "/=");}
                    |   T_AddAssign {$$ = new Operator(@1, "+=");}
                    |   T_SubAssign {$$ = new Operator(@1, "-=");}


Statement 	    : CompoundScope {$$ = $1;}
                    | SimpleStatement {$$ = $1;}
                    ;

StatementScope	    : CompoundNoScope
                    | SimpleStatement
                    {   
                        List<Stmt*> * list = new List<Stmt*>;
                        list->Append($1);
                        $$ = new StmtBlock(new List<VarDecl*>, list);
                    }
                    ;

StatementNoScope    : CompoundNoScope
                    | SimpleStatement
                    {   
                        List<Stmt*> * list = new List<Stmt*>;
                        list->Append($1);
                        $$ = new StmtBlock(new List<VarDecl*>, list);
                    }
                    ; 

CompoundScope       : T_LeftBrace T_RightBrace {$$ = new StmtBlock(new List<VarDecl*>(), new List<Stmt*>());}
		            | T_LeftBrace StatementList T_RightBrace {$$ = new StmtBlock(new List<VarDecl*>(),  $2);}
		            | T_LeftBrace SingleDeclList StatementList T_RightBrace {$$ = new StmtBlock($2, $3);}
                    ;

SingleDeclList  :   VarDecl T_Semicolon {($$=new List<VarDecl*>)->Append($1);}
                |   SingleDeclList VarDecl T_Semicolon  {($$=$1)->Append($2);}
                ;

CompoundNoScope     : T_LeftBrace T_RightBrace {$$ = new StmtBlock(new List<VarDecl*>(), new List<Stmt*>());}
		    | T_LeftBrace StatementList T_RightBrace {$$ = new StmtBlock(new List<VarDecl*>(),  $2);}
		    ;

StatementList       : Statement {($$ = new List<Stmt*>)->Append($1); }
                    | StatementList Statement {($$=$1)->Append($2);}
                    ; 

SimpleStatement     : Expression T_Semicolon {$$ = $1;}
                    | SelectionStatement {$$ = $1;}
                    /*| SwitchStatement  {$$ = $1;}*/
                    /*| CaseLabel {$$ = $1;}*/
                    | IterationStatement  {$$ = $1;}
                    | JumpStatement  {$$ = $1;}
                    ;

Conditionopt        :   Expression   {$$ = $1;}
                    |   {$$ = new EmptyExpr();}
                    ;

SelectionStatement  : T_If T_LeftParen Expression T_RightParen StatementScope T_Else StatementScope {$$ = new IfStmt($3, $5, $7);}
                    | T_If T_LeftParen Expression T_RightParen StatementScope   {$$ = new IfStmt($3, $5, NULL);}%prec"If" 

                    ;

IterationStatement  : T_While T_LeftParen Expression T_RightParen StatementNoScope {$$ = new WhileStmt($3, $5);}
                    | T_Do StatementScope T_While T_LeftParen Expression T_RightParen T_Semicolon 
 		            {
			            $$ = new DoWhileStmt($2, $5); 
		            }
                    | T_For T_LeftParen ForInitStatement ForRestStatement T_RightParen StatementNoScope 
                    {
			            $$ = new ForStmt($3, $4.test, $4.step, $6); 
		            }
                  /*  | T_For T_LeftParen ForInitStatement T_Semicolon T_RightParen StatementNoScope 
                      {
			$$ = new ForStmt($3, new EmptyExpr(), new EmptyExpr(), $6); 
		      }
                    | T_For T_LeftParen ForInitStatement Condition T_Semicolon Expression T_RightParen StatementNoScope 
                      {
			$$ = new ForStmt($3, $4, $6, $8); 
		      }
                    | T_For T_LeftParen ForInitStatement T_Semicolon Expression T_RightParen StatementNoScope 
                      {
			$$ = new ForStmt($3, new EmptyExpr(), $5, $7); 
		      }
              */
                    ;

ForInitStatement    : T_Semicolon {$$ = new EmptyExpr();}
                    | Expression T_Semicolon {$$ = $1;}
                    ; 

ForRestStatement    : Conditionopt T_Semicolon {$$.test = $1;}
                    | Conditionopt T_Semicolon Expression
                    {
                        $$.test = $1;
                        $$.step = $3;
                    }
                    ; 

JumpStatement 	    : T_Break T_Semicolon {$$ = new BreakStmt(@1);}
                    | T_Return T_Semicolon {$$ =  new ReturnStmt(@1, NULL);}
                    | T_Return Expression T_Semicolon {$$ = new ReturnStmt(@1, $2); }
                    ;

ExternalDecl        : FunctionDefinition    {$$ = $1;}
                    | Decl  {$$ = $1;}
                    ; 

FunctionDefinition  : FunctionPrototype CompoundNoScope
                    {
                        ($$=$1)->SetFunctionBody($2);
                    }
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
