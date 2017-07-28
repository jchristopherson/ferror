// ferror_test_c.c

#include <stdio.h>
#include "ferror.h"

// Testing code for the ferror library via it's C interface.
int main() {
    errorhandler err;

    // Initialize the errorhandler object
    alloc_errorhandler(&err);

    // Try and pass NULL to the register_error routine and ensure it doesn't 
    // bomb out.
    register_error(NULL, "function name", "error message", 1);

    // If we're still running, the test has passed
    printf("FERROR C INTERFACE TEST STATUS: PASS\n");

    // End
    return 0;
}