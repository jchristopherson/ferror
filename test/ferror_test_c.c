// ferror_test_c.c

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include "ferror.h"

bool test_log_file_get_set(void);
bool test_error_reporting(void);

// Testing code for the ferror library via it's C interface.
int main() {
    // // Local Variables
    // errorhandler err;

    // // Initialize the errorhandler object
    // alloc_errorhandler(&err);

    // // Try and pass NULL to the report_error routine and ensure it doesn't 
    // // bomb out.
    // report_error(NULL, "function name", "error message", 1);

    // // If we're still running, the test has passed
    // printf("FERROR C INTERFACE TEST STATUS: PASS\n");

    // // End
    // free_errorhandler(&err);
    // return 0;

    // Local Variables
    bool test_result, overall;

    // Initialization
    overall = true;

    // Process
    test_result = test_log_file_get_set();
    if (!test_result) overall = false;

    test_result = test_error_reporting();
    if (!test_result) overall = false;

    // End
    if (overall) {
        printf("FERROR C INTERFACE TEST STATUS: PASS\n");
        return 0;
    }
    else {
        printf("FERROR C INTERFACE TEST STATUS: FAIL\n");
        return 1;
    }
}

/* ************************************************************************** */
bool test_log_file_get_set(void) {
    // Local Variables
    bool rst = true;
    errorhandler obj;
    const char fname[] = "test_filename.txt";
    char buffer[256];
    int flag, nbuffer = 256;

    // Initialization
    alloc_errorhandler(&obj);

    // See if the get and set functions work appropriately
    set_log_filename(&obj, fname);
    get_log_filename(&obj, buffer, &nbuffer);
    flag = strncmp(fname, buffer, (size_t)nbuffer);
    if (flag != 0) {
        rst = false;
        printf("Expected a filename of: %s, but found a filename of %s.\n",
            fname, buffer);
    }

    // End
    free_errorhandler(&obj);
    return rst;
}

/* ************************************************************************** */
bool test_error_reporting(void) {
    // Local Variables
    bool test, rst = true;
    errorhandler obj;
    const int code = 100;
    const char msg[] = "Test error message.  Do not be alarmed.";
    const char fcn[] = "Test_Fcn";
    int flag, nbuffer = 256, nfbuffer = 256;
    char buffer[256], fbuffer[256];

    // Initialization
    alloc_errorhandler(&obj);

    // Ensure the error reporting doesn't terminate the application
    set_exit_on_error(&obj, false);

    // Don't print the error message to the command line
    set_suppress_printing(&obj, true);

    // Report the error
    report_error(&obj, fcn, msg, code);

    // Ensure an error was logged
    test = has_error_occurred(&obj);
    if (!test) {
        rst = false;
        printf("Expected an error, but found none.");
    }

    // Check the error flag
    flag = get_error_flag(&obj);
    if (flag != code) {
        rst = false;
        printf(
            "Expected an error code of %i, but received an error code of %i.\n",
            code, flag);
    }

    // Check the error message
    get_error_message(&obj, buffer, &nbuffer);
    flag = strncmp(msg, buffer, nbuffer);
    if (flag != 0) {
        rst = false;
        printf("Expected an error message of: %s, but found a message of: %s.\n",
            msg, buffer);
    }

    // Check the function name
    get_error_fcn_name(&obj, fbuffer, &nfbuffer);
    flag = strncmp(fcn, fbuffer, nfbuffer);
    if (flag != 0) {
        rst = false;
        printf("Expected a function name of: %s, but found a name of: %s.\n",
            fcn, fbuffer);
    }

    // End
    free_errorhandler(&obj);
    return rst;
}

/* ************************************************************************** */

/* ************************************************************************** */

/* ************************************************************************** */

/* ************************************************************************** */

/* ************************************************************************** */
