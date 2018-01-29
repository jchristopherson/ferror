// ferror_test_c.c

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include "ferror.h"

bool test_log_file_get_set(void);
bool test_error_reporting(void);
bool test_warning_reporting(void);
bool test_error_reset(void);
bool test_warning_reset(void);

// Testing code for the ferror library via it's C interface.
int main() {
    // Local Variables
    bool test_result, overall;

    // Initialization
    overall = true;

    // Process
    test_result = test_log_file_get_set();
    if (!test_result) overall = false;

    test_result = test_error_reporting();
    if (!test_result) overall = false;

    test_result = test_warning_reporting();
    if (!test_result) overall = false;

    test_result = test_error_reset();
    if (!test_result) overall = false;

    test_result = test_warning_reset();
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
bool test_warning_reporting(void) {
    // Local Variables
    bool test, rst = true;
    errorhandler obj;
    const int code = 100;
    const char msg[] = "Test warning message.  Do not be alarmed.";
    const char fcn[] = "Test_Fcn";
    int flag, nbuffer = 256, nfbuffer = 256;
    char buffer[256], fbuffer[256];

    // Initialization
    alloc_errorhandler(&obj);

    // Don't print the warning message to the command line
    set_suppress_printing(&obj, true);

    // Report the warning
    report_warning(&obj, fcn, msg, code);

    // Ensure an warning was logged
    test = has_warning_occurred(&obj);
    if (!test) {
        rst = false;
        printf("Expected a warning, but found none.");
    }

    // Check the warning flag
    flag = get_warning_flag(&obj);
    if (flag != code) {
        rst = false;
        printf(
            "Expected a warning code of %i, but received a warning code of %i.\n",
            code, flag);
    }

    // Check the warning message
    get_warning_message(&obj, buffer, &nbuffer);
    flag = strncmp(msg, buffer, nbuffer);
    if (flag != 0) {
        rst = false;
        printf("Expected an warning message of: %s, but found a message of: %s.\n",
            msg, buffer);
    }

    // Check the function name
    get_warning_fcn_name(&obj, fbuffer, &nfbuffer);
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
bool test_error_reset(void) {
    // Local Variables
    bool rst = true;
    errorhandler obj;

    // Initialization
    alloc_errorhandler(&obj);

    // Ensure the error reporting doesn't kill the application
    set_exit_on_error(&obj, false);

    // Don't print anything either
    set_suppress_printing(&obj, true);

    // Set an error condition
    report_error(&obj, "fcn1", "Error Message", 1);

    // Reset the error
    reset_error_status(&obj);

    // Ensure the error was reset
    if (has_error_occurred(&obj)) {
        rst = false;
        printf("Expected the error message to be reset.\n");
    }


    // End
    free_errorhandler(&obj);
    return rst;
}

/* ************************************************************************** */
bool test_warning_reset(void) {
    // Local Variables
    bool rst = true;
    errorhandler obj;

    // Initialization
    alloc_errorhandler(&obj);

    // Don't print anything
    set_suppress_printing(&obj, true);

    // Set a warning condition
    report_warning(&obj, "fcn1", "Warning Message", 1);

    // Reset the error
    reset_warning_status(&obj);

    // Ensure the warning was reset
    if (has_warning_occurred(&obj)) {
        rst = false;
        printf("Expected the warning message to be reset.\n");
    }


    // End
    free_errorhandler(&obj);
    return rst;
}

/* ************************************************************************** */
