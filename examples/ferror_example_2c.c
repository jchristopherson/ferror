// ferror_example_2c.c

#include <stdio.h>
#include "ferror.h"

void causes_error(error_handler *err);


int main(void) {
    // Variables
    error_handler err_mgr;
    char fname[256], msg[256];
    int flag, fnamelength = 256, msglength = 256;

    // Initialization
    alloc_error_handler(&err_mgr);

    // Ensure the error reporting doesn't terminate the application
    set_exit_on_error(&err_mgr, false);

    // Don't print the error message to the command line
    set_suppress_printing(&err_mgr, true);

    // Call the routine that causes the error
    causes_error(&err_mgr);

    // Retrieve the error information
    get_error_fcn_name(&err_mgr, fname, &fnamelength);
    get_error_message(&err_mgr, msg, &msglength);
    flag = get_error_flag(&err_mgr);

    // Print the error information
    printf("An error occurred in the following subroutine: %s\nThe error message is: %s\nThe error code is: %i\n",
        fname, msg, flag);

    // End
    free_error_handler(&err_mgr);
    return 0;
}

void causes_error(error_handler *err) {
    report_error(err,                       // The error_handler object
        "causes_error",                     // The function name
        "This is a test error message.",    // The error message
        200);                               // The error flag
}
