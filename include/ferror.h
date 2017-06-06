// ferror.h
#ifndef FERROR_H_DEFINED
#define FERROR_H_DEFINED

#include <stdbool.h>

// Convenience type to avoid confusion with void* for error handler object.
typedef void* errorhandler;

#ifdef __cplusplus
extern "C" {
#endif

errorhandler alloc_error_handler();
void free_error_handler(errorhandler x);

void get_error_log_fname(const errorhandler err, char *fname, int *nfname);
void set_error_log_fname(errorhandler err, const char *fname);
void register_error(errorhandler err, const char *fcn, const char *msg, 
                    int flag);
void register_warning(errorhandler err, const char *fcn, const char *msg,
                      int flag);
void write_error_log(const errorhandler err, const char *fcn, const char *msg,
                     int flag);
bool error_occurred(const errorhandler err);
void reset_error(errorhandler err);
bool warning_occurred(const errorhandler err);
void reset_warning(errorhandler err);
int get_error_code(const errorhandler err);
int get_warning_code(const errorhandler err);
bool get_exit_behavior(const errorhandler err);
void set_exit_behavior(errorhandler err, bool x);

#ifdef __cplusplus
}
#endif
#endif // END FERROR_H_DEFINED
