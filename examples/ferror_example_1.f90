! ferror_example_1.f90

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