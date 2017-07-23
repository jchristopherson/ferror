# ferror
A library to assist with error handling in Fortran projects.

## Usage

```fortran
program  example
    use ferror, only : errors
    implicit none

    ! Local Variables
    type(errors) :: err
    integer :: errorFlag
    character(len = :), allocatable :: fname

    ! Get the name of the error log file used by default
    fname = err%get_log_filename()
    print '(A)', "Error Log File: " // fname

    ! Don't let the program terminate upon error
    call err%set_exit_on_error(.false.)

    ! Warn the user
    errorFlag = 1
    call err%report_warning("function name", "warning message", errorFlag)

    ! Get the warning code
    print '(AI0)', "Retrieved Warning Code: ", err%get_warning_flag()

    ! Inform the user of an error condition
    errorFlag = 2
    call err%report_error("function name", "error message", errorFlag)

    ! Get the error code
    print '(AI0)', "Retrieved Error Code: ", err%get_error_flag()
end program 
```
The above program produces the following output.
```text
Error Log File: error_log.txt
 
***** WARNING *****
Error Log File: error_log.txt

***** WARNING *****
Function: function name
Warning Flag: 1
Message:
warning message

Retrieved Warning Code: 1

***** ERROR *****
Function: function name
Error Flag: 2
Message:
error message

Retrieved Error Code: 2
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
The above program produces the following output.
```text
Number of characters: 14
Error Log File: error_log.txt

***** WARNING *****
Function: function name
Warning Flag: 1
Message:
warning message

Retrieved Warning Code: 1

***** ERROR *****
Function: function name
Error Flag: 2
Message:
error message here

Retrieved Error Code: 2
```

## Documentation
Documentation can be found [here](doc/refman.pdf)

## Contributing
This project is a work in progress and anyone is free to contribute.

For bug reports or feature requests please open an issue on github.