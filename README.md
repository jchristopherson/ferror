# ferror
A library to assist with error handling in Fortran projects.

## Usage

```fortran

    use ferror, only : errors
    implicit none
    
    type(errors) :: err

    ! A flag to identify the error
    integer :: errorFlag = 1
    
    ! Inform the user of an error condition.  Notice, the default behavior
    ! creates/appends this information to a log file, prints the information,
    ! and then terminates the application.
    call err%report_error("function name", "Error message here", errorFlag)

    ! Inform the user of a warning.  The default behavior simply prints a 
    ! warning message and returns to the calling code.
    call err%report_warning("function name", "Warning message here", errorFlag)

```

## C Usage
```c
#include <stdio.h>
#include "ferror.h"

int main() {
    errorhandler err;
    int errorFlag, sz;
    char buffer[256];

    // Initialize the errorhandler object
    alloc_errorhandler(&err);

    // Get the name of the error log file
    get_error_log_fname(&err, buffer, &sz);
    printf("Number of characters: %i\nError Log File: %s\n", sz, buffer);

    // Don't let the program terminate upon error.
    set_exit_behavior(&err, false);

    // Warn the user
    errorFlag = 1;
    register_warning(&err, "function name", "warning message", errorFlag);

    // Return the warning code
    printf("Retrieved Warning Code: %i\n", get_warning_code(&err));

    // Inform the user of an error condition.
    errorFlag = 2;
    register_error(&err, "function name", "error message here", errorFlag);

    // Return the error code
    printf("Retrieved Error Code: %i\n", get_error_code(&err));
}

```
## Documentation
Documentation can be found [here](doc/refman.pdf)

## Contributing
This project is a work in progress and anyone is free to contribute.

For bug reports or feature requests please open an issue on github.