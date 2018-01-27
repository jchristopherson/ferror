// ferror_test_c.c

#include <stdio.h>
#include "ferror.h"

// Testing code for the ferror library via it's C interface.
int main() {
    // Local Variables
    errorhandler err;

    // Initialize the errorhandler object
    alloc_errorhandler(&err);

    // Try and pass NULL to the report_error routine and ensure it doesn't 
    // bomb out.
    report_error(NULL, "function name", "error message", 1);

    // If we're still running, the test has passed
    printf("FERROR C INTERFACE TEST STATUS: PASS\n");

    // End
    free_errorhandler(&err);
    return 0;
}