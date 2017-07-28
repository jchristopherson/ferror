#include <stdio.h>
#include "ferror.h"

int main() {
    errorhandler err;
    int errorFlag, sz;
    char buffer[256];

    // Initialize the errorhandler object
    alloc_errorhandler(&err);

    // Get the name of the error log file
    get_log_filename(&err, buffer, &sz);
    printf("Number of characters: %i\nError Log File: %s\n", sz, buffer);

    // Don't let the program terminate upon error.
    set_exit_on_error(&err, false);

    // Warn the user
    errorFlag = 1;
    report_warning(&err, "function name", "warning message", errorFlag);

    // Return the warning code
    printf("Retrieved Warning Code: %i\n", get_warning_flag(&err));

    // Inform the user of an error condition.
    errorFlag = 2;
    report_error(&err, "function name", "error message", errorFlag);

    // Return the error code
    printf("Retrieved Error Code: %i\n", get_error_flag(&err));
}
