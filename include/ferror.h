// ferror.h
#ifndef FERROR_H_DEFINED
#define FERROR_H_DEFINED

#include <stdbool.h>

// Convenience type to avoid confusion with void* for error handler object.
typedef struct {
    int n;
    void *ptr;
} errorhandler;

#ifdef __cplusplus
extern "C" {
#endif

/** @brief Initializes a new error handler object.
 *
 * @param obj The errorhandler object to allocate.
 */
void alloc_errorhandler(errorhandler *obj);

/** @brief Gets the name of the error log file.
 *
 * @param err The errorhandler object.
 * @param fname A character buffer where the filename will be written.
 *  It is recommended that this be in the neighborhood of 256 elements.
 * @param nfname On input, the actual size of the buffer.  Be sure
 *  to leave room for the null terminator character.  On output, the actual
 *  numbers of characters written to @p fname (not including the null
 *  character).
 */
void get_error_log_fname(const errorhandler *err, char *fname, int *nfname);

/** @brief Sets the error log filename.
 *
 * @param err The errorhandler object.
 * @param fname A null-terminated string containing the filename.
 */
void set_error_log_fname(errorhandler *err, const char *fname);

/** @brief Reports an error condition to the user.
 *
 * @param err The errorhandler object.
 * @param fcn The name of the function or subroutine in which the error
 *  was encountered.
 * @param msg The error message.
 * @param flag The error flag.
 */
void register_error(errorhandler *err, const char *fcn, const char *msg,
                    int flag);

/** @brief Reports a warning condition to the user.
 *
 * @param err The errorhandler object.
 * @param fcn The name of the function or subroutine in which the
 *  warning was encountered.
 * @param msg The warning message.
 * @param flag The warning flag.
 */
void register_warning(errorhandler *err, const char *fcn, const char *msg,
                      int flag);

/** @brief Writes an error log file.
 *
 * @param err The errorhandler object.
 * @param fcn The name of the function or subroutine in which the error
 *  was encountered.
 * @param msg The error message.
 * @param flag The error flag.
 */
void write_error_log(const errorhandler *err, const char *fcn, const char *msg,
                     int flag);

/** @brief Tests to see if an error has been encountered.
 *
 * @param err The errorhandler object.
 * @return Returns true if an error has been encountered; else, false.
 */
bool error_occurred(const errorhandler *err);

/** @brief Resets the error status flag to false, and the current error flag
 * to zero.
 *
 * @param err The errorhandler object.
 */
void reset_error(errorhandler *err);

/** @brief Tests to see if a warning has been encountered.
 *
 * @param err The errorhandler object.
 * @return Returns true if a warning has been encountered; else, false.
 */
bool warning_occurred(const errorhandler *err);

/** @brief Resets the warning status flag to false, and the current warning
 * flag to zero.
 *
 * @param err The errorhandler object.
 */
void reset_warning(errorhandler *err);

/** @brief Gets the current error flag.
 *
 * @param err The errorhandler object.
 * @return The current error flag.
 */
int get_error_code(const errorhandler *err);

/** @brief Gets the current warning flag.
 *
 * @param err The errorhandler object.
 * @return The current warning flag.
 */
int get_warning_code(const errorhandler *err);

/** @brief Gets a logical value determining if the application should be
 * terminated when an error is encountered.
 *
 * @param err The errorhandler object.
 * @return Returns true if the application should be terminated; else,
 *  false.
 */
bool get_exit_behavior(const errorhandler *err);

/** @brief Sets a logical value determining if the application should be
 * terminated when an error is encountered.
 *
 * @param err A pointer to the error handler object.
 * @param[x] in Set to true if the application should be terminated when an
 *  error is reported; else, false.
 */
void set_exit_behavior(errorhandler *err, bool x);

/** @brief Gets a logical value determining if printing of error and warning
 * messages should be suppressed.
 *
 * @param err The errorhandler object.
 * @return True if message printing should be suppressed; else, false to 
 *  allow printing.
 */
bool get_suppress_printing(const errorhandler *err);

/** @brief Sets a logical value determining if printing of error and warning
 * messages should be suppressed.
 *
 * @param err The errorhandler object.
 * @param x Set to true if message printing should be suppressed; else,
 *  false to allow printing.
 */
void set_suppress_printing(errorhandler *err, bool x);

#ifdef __cplusplus
}
#endif
#endif // END FERROR_H_DEFINED
