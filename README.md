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

#include "ferror.h"

int main() {
    errorhandler err;
    int errorFlag;

    // Initialize the errorhandler object
    err = alloc_error_handler();

    // Inform the user of an error condition.  Notice, the default behavior
    // creates/appends this information to a log file, prints the information,
    // and then terminates the application.
    errorFlag = 1;
    register_error(err, "function name", "Error message here", errorFlag);

    // Inform the user of a warning.  The default behavior simply prints a 
    // warning message and returns to the calling code.
    register_warning(err, "function name", "Warning message here", errorFlag);

    // Clean up after ourselves
    free_error_handler(err);
}

```
## Documentation
Documentation can be found [here](doc/refman.pdf)

## Contributing
This project is a work in progress and anyone is free to contribute.

For bug reports or feature requests please open an issue on github.