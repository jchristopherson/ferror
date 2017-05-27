# ferror
A library to assist with error handling in Fortran projects.

## Usage

```fortran

    use, intrinsic :: ISO_C_binding, only: &
        wp => C_DOUBLE
        
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

## Contributing
This project is a work in progress and anyone is free to contribute.

For bug reports or feature requests please open an issue on github.