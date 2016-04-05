/* File:  scanner.l
 * ----------------
 * Lex input file to generate the scanner for the compiler.
 */

%{

/* The text within this first region delimited by %{ and %} is assumed to
 * be C/C++ code and will be copied verbatim to the lex.yy.c file ahead
 * of the definitions of the yylex() function. Add other header file inclusions
 * or C++ variable declarations/prototypes that are needed by your code here.
 */

#include <string.h>
#include "scanner.h"
#include "utility.h" // for PrintDebug()
#include "errors.h"

/* Global variable: yylval
 * -----------------------
 * This global variable is how we get attribute information about the token
 * just scanned to the client. The scanner sets the global variable
 * appropriately and since it's global the client can just read it.  In the
 * future, this variable will be declared for us in the y.tab.c file
 * produced by Yacc, but for now, we declare it manually.
 */
YYSTYPE yylval;  // manually declared for pp1, later Yacc provides

/* Global variable: yylloc
 * -----------------------
 * This global variable is how we get position information about the token
 * just scanned to the client. (Operates similarly to yylval above)
 */
struct yyltype yylloc; // manually dclared for pp1, later Yacc provides

/* Macro: YY_USER_ACTION 
 * ---------------------
 * This flex built-in macro can be defined to provide an action which is
 * always executed prior to any matched rule's action. Basically, it is
 * a way of having a piece of code common to all actions factored out to
 * this routine.  We already defined it for you and left the empty
 * function DoBeforeEachAction ready for your use as needed. It will
 * be called once for each pattern scanned from the file, before
 * executing its action.
 */
static void DoBeforeEachAction(); 
#define YY_USER_ACTION DoBeforeEachAction();

int currentLineNum, currentColNum;

%}

 /* The section before the first %% is the Definitions section of the lex
  * input file. Here is where you set options for the scanner, define lex
  * states, and can set up definitions to give names to regular expressions
  * as a simple substitution mechanism that allows for more readable
  * entries in the Rules section later. 
  */

BOOLEAN     (true|false)
IDENTIFIER  ([A-Z]|[a-z])([A-Z]|[a-z]|[0-9])*
FIELDSELECT {IDENTIFIER}.({IDENTIFIER}.)*{IDENTIFIER}
COMMENT     ["/*"]*["*/"]
OPENCOMMENT ["/*"]*
LINECOMMENT //*[\n]

DIGITSEQ   (([1-9][0-9]*)|[0])

FRACTIONAL (({DIGITSEQ}[.]{DIGITSEQ})|({DIGITSEQ}[.])|([.]{DIGITSEQ}))

EXPO	   ([e|E][+|-]{DIGITSEQ})

FLOAT	   (({FRACTIONAL}{EXPO}?[f|F]?)|({DIGITSEQ}{EXPO}[f|F]?))



%x ML_COMMENT


%%             /* BEGIN RULES SECTION */
 /* All patterns and actions should be placed between the start and stop
  * %% markers which delimit the Rules section. 
  */ 

{COMMENT}           
{OPENCOMMENT}       {}
{LINECOMMENT}       {currentLineNum++; currentColNum = 1;}

[\n]		{currentLineNum++; currentColNum = 1;}

void 		        {return T_Void;}
bool 		        {return T_Bool;}
float 		        {return T_Float;}
int 		        {return T_Int;}
"unsigned int"		{return T_Uint;}
ivec2 		        {return T_Ivec2;}
ivec3 		        {return T_Ivec3;}
ivec4 		        {return T_Ivec4;}
bvec2 		        {return T_Bvec2;}
bvec3 		        {return T_Bvec3;}
bvec4 		        {return T_Bvec4;}
uvec2 		        {return T_Uvec2;}
uvec3 		        {return T_Uvec3;}
uvec4 		        {return T_Uvec4;}

vec2 		        {return T_Vec2;}
vec3 		        {return T_Vec3;}
vec4 		        {return T_Vec4;}
mat2 		        {return T_Mat2;}
mat3 		        {return T_Mat3;}
mat4 		        {return T_Mat4;}
struct 		        {return T_Struct;}
in 		            {return T_Struct;}
out 		        {return T_Struct;}
const               {return T_Const;}
uniform             {return T_Uniform;}
while               {return T_While;}
break               {return T_Break;}
continue            {return T_Continue;}
else                {return T_Else;}
for                 {return T_For;}
if                  {return T_If;}
return              {return T_Return;}
switch              {return T_Switch;}
case                {return T_Case;}
default             {return T_Default;}
{BOOLEAN}           {yylval.boolConstant = strdup(yytext);
                     return T_BoolConstant;}

{IDENTIFIER}        {memcpy(yylval.identifier, strndup(yytext, MaxIdentLen+1), sizeof(yylval.identifier));
                     return T_Identifier;}

{FIELDSELECT}       {return T_FieldSelection;}

{FLOAT}		{yylval.floatConstant = strtod(yytext, NULL); return T_FloatConstant;}

{DIGITSEQ}[u|U]	{yylval.integerConstant = strtol(yytext, NULL, 10); return T_UintConstant;}

{DIGITSEQ}	{yylval.integerConstant = strtol(yytext, NULL, 10); return T_IntConstant;}

[+][+]		{return T_Inc;}

[-][-]		{return T_Dec;}

[<][=]		{return T_LessEqual;}

[>][=]		{return T_GreaterEqual;}

[=][=]		{return T_EQ;}

[!][=]		{return T_NE;}

[&][&]		{return T_And;}

[|][|]		{return T_Or;}

[*][=]		{return T_MulAssign;}

[/][=]		{return T_DivAssign;}

[+][=]		{return T_AddAssign;}

[-][=]		{return T_SubAssign;}

[(]		{return T_LeftParen;}

[)]		{return T_RightParen;}

[[]		{return T_LeftBracket;}

[]]		{return T_RightBracket;}

[{]		{return T_LeftBrace;}

[}]		{return T_RightBrace;}

[.]		{return T_Dot;}

[:]		{return T_Colon;}

[=]		{return T_Equal;}

[;]		{return T_Semicolon;}

[-]		{return T_Dash;}

[+]		{return T_Plus;}

[*]		{return T_Star;}

[/]		{return T_Slash;}

[<]		{return T_LeftAngle;}

[>]		{return T_RightAngle;}

[?]		{return T_Question;}

                     

%%

/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated lex.yy.c file.
 * This section is where you put definitions of helper functions.
 */


/* Function: InitScanner
 * ---------------------
 * This function will be called before any calls to yylex().  It is designed
 * to give you an opportunity to do anything that must be done to initialize
 * the scanner (set global variables, configure starting state, etc.). One
 * thing it already does for you is assign the value of the global variable
 * yy_flex_debug that controls whether flex prints debugging information
 * about each token and what rule was matched. If set to false, no information
 * is printed. Setting it to true will give you a running trail that might
 * be helpful when debugging your scanner. Please be sure the variable is
 * set to false when submitting your final version.
 */

void InitScanner()
{
    PrintDebug("lex", "Initializing scanner");
    yy_flex_debug = true;
    currentLineNum = 1;
    currentColNum = 1;
}


/* Function: DoBeforeEachAction()
 * ------------------------------
 * This function is installed as the YY_USER_ACTION. This is a place
 * to group code common to all actions.
 */
static void DoBeforeEachAction()
{
    yylloc.first_line = currentLineNum;
    yylloc.last_line = currentLineNum;

    yylloc.first_column = currentColNum;
    yylloc.last_column = currentColNum + yyleng - 1;
    currentColNum += yyleng;
}
