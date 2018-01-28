! ferror_test.f90

program main
    use, intrinsic :: iso_fortran_env, only : int32
    use ferror
    implicit none

    ! Local Variables
    logical :: test_result, overall
    overall = .true.

    ! Tests
    test_result = test_log_file_get_set()
    if (.not.test_result) overall = .false.

    test_result = test_error_reporting()
    if (.not.test_result) overall = .false.

    test_result = test_warning_reporting()
    if (.not.test_result) overall = .false.

    test_result = test_error_reset()
    if (.not.test_result) overall = .false.

    test_result = test_warning_reset()
    if (.not.test_result) overall = .false.

    if (overall) then
        print '(A)', "FERROR TEST STATUS: PASS"
        call exit(0)
    else
        print '(A)', "FERROR TEST STATUS: FAIL"
        call exit(1)
    end if

contains
! ------------------------------------------------------------------------------
    function test_log_file_get_set() result(rst)
        ! Local Variables
        logical :: rst
        type(errors) :: obj
        character(len = *), parameter :: fname = "test_filename.txt"
        character(len = :), allocatable :: check
        
        ! Initialization
        rst = .true.

        ! See if the get and set functions work appropriately
        call obj%set_log_filename(fname)
        check = obj%get_log_filename()
        if (check /= fname) then
            rst = .false.
            print '(A)', "Expected a filename of: " // fname // &
                ", but found a filename of: " // check // "."
        end if
    end function

! ------------------------------------------------------------------------------
    function test_error_reporting() result(rst)
        ! Local Variables
        logical :: rst
        type(errors) :: obj
        integer(int32), parameter :: code = 100
        character(len = *), parameter :: msg = "Test error message.  Do not be alarmed"
        character(len = *), parameter :: fcn = "Test_Fcn"
        integer(int32) :: flag
        character(len = :), allocatable :: check
        logical :: test

        ! Initialization
        rst = .true.

        ! Ensure the error reporting doesn't terminate the application
        call obj%set_exit_on_error(.false.)

        ! Don't print the error message to the command line
        call obj%set_suppress_printing(.true.)

        ! Report the error
        call obj%report_error(fcn, msg, code)

        ! Ensure an error was logged
        test = obj%has_error_occurred()
        if (.not.test) then
            rst = .false.
            print '(A)', "Expected an error, but found none."
        end if

        ! Check the error flag
        if (obj%get_error_flag() /= code) then
            rst = .false.
            print '(AI0AI0)', "Expected an error code of ", code, &
                ", but received an error code of ", flag, "."
        end if

        ! Check the error message
        check = obj%get_error_message()
        if (check /= msg) then
            rst = .false.
            print '(A)', "Expected an error message of: " // msg // &
                ", but found a message of: " // check // "."
        end if

        ! Check the function name
        check = obj%get_error_fcn_name()
        if (check /= fcn) then
            rst = .false.
            print '(A)', "Expected a function name of: " // fcn // &
                ", but found a name of: " // check // "."
        end if
    end function

! ------------------------------------------------------------------------------
    function test_warning_reporting() result(rst)
        ! Local Variables
        logical :: rst
        type(errors) :: obj
        integer(int32), parameter :: code = 100
        character(len = *), parameter :: msg = "Test warning message.  Do not be alarmed"
        character(len = *), parameter :: fcn = "Test_Fcn"
        integer(int32) :: flag
        character(len = :), allocatable :: check
        logical :: test

        ! Initialization
        rst = .true.

        ! Don't print the warning message to the command line
        call obj%set_suppress_printing(.true.)

        ! Report the warning
        call obj%report_warning(fcn, msg, code)

        ! Ensure a warning was logged
        test = obj%has_warning_occurred()
        if (.not.test) then
            rst = .false.
            print '(A)', "Expected a warning, but found none."
        end if

        ! Check the warning flag
        if (obj%get_warning_flag() /= code) then
            rst = .false.
            print '(AI0AI0)', "Expected an warning code of ", code, &
                ", but received an warning code of ", flag, "."
        end if

        ! Check the warning message
        check = obj%get_warning_message()
        if (check /= msg) then
            rst = .false.
            print '(A)', "Expected a warning message of: " // msg // &
                ", but found a message of: " // check // "."
        end if

        ! Check the function name
        check = obj%get_warning_fcn_name()
        if (check /= fcn) then
            rst = .false.
            print '(A)', "Expected a function name of: " // fcn // &
                ", but found a name of: " // check // "."
        end if
    end function

! ------------------------------------------------------------------------------
    function test_error_reset() result(rst)
        ! Local Variables
        logical :: rst
        type(errors) :: obj

        ! Initialization
        rst = .true.

        ! Ensure the error reporting doesn't terminate the application
        call obj%set_exit_on_error(.false.)

        ! Don't print the error message to the command line
        call obj%set_suppress_printing(.true.)

        ! Set an error condition
        call obj%report_error("fcn1", "Error Message", 1)

        ! Reset the error
        call obj%reset_error_status()

        ! Ensure the error was reset
        if (obj%has_error_occurred()) then
            rst = .false.
            print '(A)', "Expected the error message to be reset."
        end if
    end function

! ------------------------------------------------------------------------------
    function test_warning_reset() result(rst)
        ! Local Variables
        logical :: rst
        type(errors) :: obj

        ! Initialization
        rst = .true.

        ! Don't print the warning message to the command line
        call obj%set_suppress_printing(.true.)

        ! Set a warning condition
        call obj%report_warning("fcn1", "Warning Message", 1)

        ! Reset the warning
        call obj%reset_warning_status()

        ! Ensure the warning was reset
        if (obj%has_warning_occurred()) then
            rst = .false.
            print '(A)', "Expected the warning message to be reset."
        end if
    end function

! ------------------------------------------------------------------------------
end program main