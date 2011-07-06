/* File:  scanner.l
 * ----------------
 * Lex input file to generate the scanner for the compiler.
 *
 * This code is currently completely stubbed out.  We will
 * release our reference scanner.l next Wednesday when the
 * first assignment is completely accounted for.  Until
 * then, you should replace this code with your own
 * scanner.
 *
 * You should be sure to implement (or, at least, stub out)
 * the GetLineNumbered function in your scanner so that the
 * error-reporting routines will operate correctly.  The
 * current behavior of this function just returns NULL, which
 * suppresses the pretty error messages that would otherwise
 * be generated.  Our reference solution uses a technique
 * similar to the HQ9+ interpreter we saw in class to keep
 * track of each line.
 */

%{

#include <string.h>
#include "scanner.h"
#include "utility.h" // for PrintDebug()
#include "errors.h"
#include "parser.h" // for token codes, yylval
#include <vector>
using namespace std;

#define TAB_SIZE 8

static void DoBeforeEachAction(); 
#define YY_USER_ACTION DoBeforeEachAction();

%}

%%             /* BEGIN RULES SECTION */

%%             /* BEGIN DEFINITIONS SECTION */

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
    yy_flex_debug = false;
}


/* Function: DoBeforeEachAction()
 * ------------------------------
 * This function is installed as the YY_USER_ACTION. This is a place
 * to group code common to all actions.
 * On each match, we fill in the fields to record its location and
 * update our column counter.
 */
static void DoBeforeEachAction()
{
}

/* Function: GetLineNumbered()
 * ---------------------------
 * Returns string with contents of line numbered n or NULL if the
 * contents of that line are not available.  Our scanner copies
 * each line scanned and appends each to a list so we can later
 * retrieve them to report the context for errors.  If you use
 * your own scanner, you must implement this function yourself.
 */
const char *GetLineNumbered(int num) {
  return NULL;
}


