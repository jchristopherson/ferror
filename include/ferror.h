/** @file ferror.h */
#ifndef FERROR_H_DEFINED
#define FERROR_H_DEFINED

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