! ferror_example_2.f90

program example
    use ferror
    use, intrinsic :: iso_fortran_env, only : int32
    implicit none

    ! Variables
    type(errors) :: err_mgr

    ! Ensure the error reporting doesn't terminate the application.  The default
    ! behavior terminates the application.
    call err_mgr%set_exit_on_error(.false.)

    ! Don't print the error message to the command line.  The default behavior
    ! prints the error information to the command line.
    call err_mgr%set_suppress_printing(.true.)

    ! Call the routine that causes the error
    call causes_error(err_mgr)

    ! Print the error information
    print '(A)', "An error occurred in the following subroutine: " // &
        err_mgr%get_error_fcn_name()
    print '(A)', "The error message is: " // err_mgr%get_error_message()
    print '(AI0)', "The error code is: ", err_mgr%get_error_flag()
contains

! The purpose of this subroutine is to simply trigger an error condition.
subroutine causes_error(err)
    ! Arguments
    class(errors), intent(inout) :: err

    ! Define an error flag
    integer(int32), parameter :: error_flag = 200

    ! Trigger the error condition
    call err%report_error(&
        "causes_error", &                   ! The subroutine or function name
        "This is a test error message.", &  ! The error message.
        error_flag)                         ! The error flag
end subroutine

end program