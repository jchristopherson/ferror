/** @file ferror.h */
#ifndef FERROR_H_DEFINED
#define FERROR_H_DEFINED

/**
 *
 * @mainpage
 *
 * @section intro_sec Introduction
 * FERROR is a library to assist with error handling in Fortran projects.  The 
 * error handling capabilities also have been extended to be called from C
 * thereby providing both an error handling mechanism for C projects as well as
 * allowing C interop with Fortran projects that use this library to handle
 * errors.
 *
 * @par Example
 * The following piece of code offers a simple introduction to the use of this
 * C API of this library.
 * @code{.c}
 * #include <stdio.h>
 * #include "ferror.h"
 *
 * void causes_error(error_handler *err);
 *
 * int main(void) {
 *     // Variables
 *     error_handler err_mgr;
 *     char fname[256], msg[256];
 *     int flag, fnamelength = 256, msglength = 256;
 *
 *     // Initialization
 *     alloc_error_handler(&err_mgr);
 *
 *     // Ensure the error reporting doesn't terminate the application - optional
 *     set_exit_on_error(&err_mgr, false);
 *
 *     // Don't print the error message to the command line - optional
 *     set_suppress_printing(&err_mgr, true);
 *
 *     // Call the routine that causes the error
 *     causes_error(&err_mgr);
 *
 *     // Retrieve the error information
 *     get_error_fcn_name(&err_mgr, fname, &fnamelength);
 *     get_error_message(&err_mgr, msg, &msglength);
 *     flag = get_error_flag(&err_mgr);
 *
 *     // Print the error information
 *     printf("An error occurred in the following subroutine: %s\nThe error message is: %s\nThe error code is: %i\n",
 *         fname, msg, flag);
 *
 *     // End
 *     free_error_handler(&err_mgr);
 *     return 0;
 * }
 *
 * void causes_error(error_handler *err) {
 *     report_error(err,                       // The error_handler object
 *         "causes_error",                     // The function name
 *         "This is a test error message.",    // The error message
 *         200);                               // The error flag
 * }
 * @endcode
 *
 * @par
 * The above program produces the following output.
 * @code{.txt}
 * An error occurred in the following subroutine: causes_error
 * The error message is: This is a test error message.
 * The error code is: 200
 * @endcode
 *
 * @par
 * The above program also creates a log file.  The log file is titled 
 * error_log.txt by default, but can be named whatever by the user.  The 
 * contents of the file written from the above program are as follows.  
 * @code{.txt}
 * ***** ERROR *****
 * 1/2/2018; 16:49:40
 * Function: causes_error
 * Error Flag: 200
 * Message:
 * This is a test error message.
 * @endcode
 *
 * @par
 * If additional errors are encountered, the information is simply appended to
 * the end of the file.
*/

#include <stdbool.h>

/** @brief A C compatible type encapsulating an errors object. */
typedef struct {
    /** @brief A pointer to the errors object. */
    void *ptr;
    /** @brief The size of the errors object, in bytes. */
    int object_size;
} error_handler;

/** @brief Describes a function to call when an error is encountered.
 * 
 * @param args A pointer to any object to pass to the callback routine.
 */
typedef void (*error_callback)(void *args);

#ifdef __cplusplus
extern "C" {
#endif

/** @brief Initializes a new error handler object.
 *
 * @param obj The error_handler object to allocate.
 */
void alloc_error_handler(error_handler *obj);

/** @brief Frees resources held by the error_handler object.
 *
 * @param obj The error_handler object.
 */
void free_error_handler(error_handler *obj);

/** @brief Gets the name of the error log file.
 *
 * @param err The error_handler object.
 * @param fname A character buffer where the filename will be written.
 *  It is recommended that this be in the neighborhood of 256 elements.
 * @param nfname On input, the actual size of the buffer.  Be sure
 *  to leave room for the null terminator character.  On output, the actual
 *  numbers of characters written to @p fname (not including the null
 *  character).
 */
void get_log_filename(const error_handler *err, char *fname, int *nfname);

/** @brief Sets the error log filename.
 *
 * @param err The error_handler object.
 * @param fname A null-terminated string containing the filename.
 */
void set_log_filename(error_handler *err, const char *fname);

/** @brief Reports an error condition to the user.
 *
 * @param err The error_handler object.
 * @param fcn The name of the function or subroutine in which the error
 *  was encountered.
 * @param msg The error message.
 * @param flag The error flag.
 */
void report_error(error_handler *err, const char *fcn, const char *msg,
                  int flag);

/** @brief Reports a warning condition to the user.
 *
 * @param err The error_handler object.
 * @param fcn The name of the function or subroutine in which the
 *  warning was encountered.
 * @param msg The warning message.
 * @param flag The warning flag.
 */
void report_warning(error_handler *err, const char *fcn, const char *msg,
                    int flag);

/** @brief Writes an error log file.
 *
 * @param err The error_handler object.
 * @param fcn The name of the function or subroutine in which the error
 *  was encountered.
 * @param msg The error message.
 * @param flag The error flag.
 */
void log_error(const error_handler *err, const char *fcn, const char *msg,
               int flag);

/** @brief Tests to see if an error has been encountered.
 *
 * @param err The error_handler object.
 * @return Returns true if an error has been encountered; else, false.
 */
bool has_error_occurred(const error_handler *err);

/** @brief Resets the error status flag to false, and the current error flag
 * to zero.
 *
 * @param err The error_handler object.
 */
void reset_error_status(error_handler *err);

/** @brief Tests to see if a warning has been encountered.
 *
 * @param err The error_handler object.
 * @return Returns true if a warning has been encountered; else, false.
 */
bool has_warning_occurred(const error_handler *err);

/** @brief Resets the warning status flag to false, and the current warning
 * flag to zero.
 *
 * @param err The error_handler object.
 */
void reset_warning_status(error_handler *err);

/** @brief Gets the current error flag.
 *
 * @param err The error_handler object.
 * @return The current error flag.
 */
int get_error_flag(const error_handler *err);

/** @brief Gets the current warning flag.
 *
 * @param err The error_handler object.
 * @return The current warning flag.
 */
int get_warning_flag(const error_handler *err);

/** @brief Gets a logical value determining if the application should be
 * terminated when an error is encountered.
 *
 * @param err The error_handler object.
 * @return Returns true if the application should be terminated; else,
 *  false.
 */
bool get_exit_on_error(const error_handler *err);

/** @brief Sets a logical value determining if the application should be
 * terminated when an error is encountered.
 *
 * @param err A pointer to the error handler object.
 * @param[x] in Set to true if the application should be terminated when an
 *  error is reported; else, false.
 */
void set_exit_on_error(error_handler *err, bool x);

/** @brief Gets a logical value determining if printing of error and warning
 * messages should be suppressed.
 *
 * @param err The error_handler object.
 * @return True if message printing should be suppressed; else, false to 
 *  allow printing.
 */
bool get_suppress_printing(const error_handler *err);

/** @brief Sets a logical value determining if printing of error and warning
 * messages should be suppressed.
 *
 * @param err The error_handler object.
 * @param x Set to true if message printing should be suppressed; else,
 *  false to allow printing.
 */
void set_suppress_printing(error_handler *err, bool x);

/** @brief Gets the current error message.
 *
 * @param err The error_handler object.
 * @param msg A character buffer where the message will be written.
 * @param nmsg On input, the actual size of the buffer.  On output,
 *  the actual number of characters written to @p msg (not including the 
 *  null character).
 */
void get_error_message(const error_handler *err, char *msg, int *nmsg);

/** @brief Gets the current warning message.
 *
 * @param err The error_handler object.
 * @param msg A character buffer where the message will be written.
 * @param nmsg On input, the actual size of the buffer.  On output,
 *  the actual number of characters written to @p msg (not including the 
 *  null character).
 */
void get_warning_message(const error_handler *err, char *msg, int *nmsg);

/** @brief Gets the name of the function or subroutine that issued the last
 * error message.
 *
 * @param err The error_handler object.
 * @param fname A character buffer where the name will be written.
 * @param nfname On input, the actual size of the buffer.  On 
 *  output, the actual number of characters written to @p fname (not
 *  including the null character).
 */
void get_error_fcn_name(const error_handler *err, char *fname, int *nfname);

/** @brief Gets the name of the function or subroutine that issued the last
 * warning message.
 *
 * @param err The error_handler object.
 * @param fname A character buffer where the name will be written.
 * @param nfname On input, the actual size of the buffer.  On 
 *  output, the actual number of characters written to @p fname (not
 *  including the null character).
 */
void get_warning_fcn_name(const error_handler *err, char *fname, int *nfname);

/** @brief Reports an error condition to the user, and executes a callback
 * routine.
 *
 * @param err A pointer to the error handler object.
 * @param fname The name of the function or subroutine in which the error
 *  was encountered.
 * @param msg The error message.
 * @param flag The error flag.
 * @param cback A pointer to the callback function.
 * @param args A pointer to an object to pass to the callback function.
 */
void report_error_with_callback(error_handler *err, const char *fname, 
                                const char *msg, int flag, error_callback cback, 
                                void *args);

#ifdef __cplusplus
}
#endif
#endif // END FERROR_H_DEFINED