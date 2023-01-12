submodule (ferror) ferror_implementation
contains
! ------------------------------------------------------------------------------
pure module function er_get_log_filename(this) result(str)
    class(errors), intent(in) :: this
    character(len = :), allocatable :: str
    str = trim(this%m_fname)
end function

! --------------------
module subroutine er_set_log_filename(this, str)
    class(errors), intent(inout) :: this
    character(len = *), intent(in) :: str
    integer(int32) :: n
    n = min(len(str), 256)
    this%m_fname = ""
    this%m_fname(1:n) = str(1:n)
end subroutine

! ------------------------------------------------------------------------------
module subroutine er_report_error(this, fcn, msg, flag, obj)
    ! Arguments
    class(errors), intent(inout) :: this
    character(len = *), intent(in) :: fcn, msg
    integer(int32), intent(in) :: flag
    class(*), intent(inout), optional :: obj

    ! Local Variables
    integer(int32) :: n, dummy

    ! Write the error message to the command line
    if (.not.this%m_suppressPrinting) then
        print *, ""
        print '(A)', "***** ERROR *****"
        print '(A)', "Function: " // fcn
        print 100, "Error Flag: ", flag
        print '(A)', "Message:"
        print '(A)', msg
        print *, ""
    100 format(A, I0)            
    end if

    ! Update the error found status
    this%m_foundError = .true.
    this%m_errorFlag = flag

    ! Store the message
    n = len(msg)
    if (allocated(this%m_errorMessage)) deallocate(this%m_errorMessage)
    allocate(character(len = n) :: this%m_errorMessage)
    this%m_errorMessage = msg(1:n)

    ! Store the function name
    n = len(fcn)
    if (allocated(this%m_eFunName)) deallocate(this%m_eFunName)
    allocate(character(len = n) :: this%m_eFunName)
    this%m_eFunName = fcn(1:n)

    ! Write the error message to a log file
    call this%log_error(fcn, msg, flag)

    ! Call the clean-up routine, if available
    if (associated(this%m_errCleanUp)) then
        if (present(obj)) then
            call this%m_errCleanUp(obj)
        else
            dummy = 0
            call this%m_errCleanUp(dummy)
        end if
    end if

    ! Exit the program
    if (this%m_exitOnError) call exit(flag)
end subroutine

! ------------------------------------------------------------------------------
module subroutine er_report_warning(this, fcn, msg, flag)
    ! Arguments
    class(errors), intent(inout) :: this
    character(len = *), intent(in) :: fcn, msg
    integer(int32), intent(in) :: flag

    ! Local Variables
    integer(int32) :: n

    ! Write the warning message to the command line
    if (.not.this%m_suppressPrinting) then
        print *, ""
        print '(A)', "***** WARNING *****"
        print '(A)', "Function: " // fcn
        print 100, "Warning Flag: ", flag
        print '(A)', "Message:"
        print '(A)', msg
        print *, ""
    100 format(A, I0)            
    end if

    ! Update the warning found status
    this%m_foundWarning = .true.
    this%m_warningFlag = flag

    ! Store the message
    n = len(msg)
    if (allocated(this%m_warningMessage)) deallocate(this%m_warningMessage)
    allocate(character(len = n) :: this%m_warningMessage)
    this%m_warningMessage = msg(1:n)

    ! Store the function name
    n = len(fcn)
    if (allocated(this%m_wFunName)) deallocate(this%m_wFunName)
    allocate(character(len = n) :: this%m_wFunName)
    this%m_wFunName = fcn(1:n)
end subroutine

! ------------------------------------------------------------------------------
module subroutine er_log_error(this, fcn, msg, flag)
    ! Arguments
    class(errors), intent(in) :: this
    character(len = *), intent(in) :: fcn, msg
    integer(int32), intent(in) :: flag

    ! Local Variables
    integer(int32) :: fid, time(3), date(3), t1, t2, t3, d1, d2, d3

    ! Open the file
    open(newunit = fid, file = this%m_fname, access = "append")

    ! Determine the time
#ifdef IFORT
    call itime(t1, t2, t3)
    call idate(d1, d2, d3)
    time = [t1, t2, t3]
    date = [d1, d2, d3]
#else
    call itime(time)
    call idate(date)
#endif

    ! Write the error information
    write(fid, '(A)') ""
    write(fid, '(A)') "***** ERROR *****"
    write(fid, 100) date(1), "/", date(2), "/", date(3), &
        "; ", time(1), ":", time(2), ":", time(3)
    write(fid, '(A)') "Function: " // fcn
    write(fid, 101) "Error Flag: ", flag
    write(fid, '(A)') "Message:"
    write(fid, '(A)') msg
    write(fid, '(A)') ""

    ! Close the file
    close(fid)

    ! Format Statements
    100 format(I0, A, I0, A, I0, A, I0, A, I0, A, I0)        
    101 format(A, I0)        
end subroutine

! ------------------------------------------------------------------------------
pure module function er_has_error_occurred(this) result(x)
    class(errors), intent(in) :: this
    logical :: x
    x = this%m_foundError
end function

! ------------------------------------------------------------------------------
module subroutine er_reset_error_status(this)
    class(errors), intent(inout) :: this
    this%m_foundError = .false.
    this%m_errorFlag = 0
    if (allocated(this%m_errorMessage)) deallocate(this%m_errorMessage)
    if (allocated(this%m_eFunName)) deallocate(this%m_eFunName)
end subroutine

! ------------------------------------------------------------------------------
pure module function er_has_warning_occurred(this) result(x)
    class(errors), intent(in) :: this
    logical :: x
    x = this%m_foundWarning
end function

! ------------------------------------------------------------------------------
module subroutine er_reset_warning_status(this)
    class(errors), intent(inout) :: this
    this%m_foundWarning = .false.
    this%m_warningFlag = 0
    if (allocated(this%m_warningMessage)) deallocate(this%m_warningMessage)
    if (allocated(this%m_wFunName)) deallocate(this%m_wFunName)
end subroutine

! ------------------------------------------------------------------------------
pure module function er_get_error_flag(this) result(x)
    class(errors), intent(in) :: this
    integer(int32) :: x
    x = this%m_errorFlag
end function

! ------------------------------------------------------------------------------
pure module function er_get_warning_flag(this) result(x)
    class(errors), intent(in) :: this
    integer(int32) :: x
    x = this%m_warningFlag
end function

! ------------------------------------------------------------------------------
pure module function er_get_exit_on_error(this) result(x)
    class(errors), intent(in) :: this
    logical :: x
    x = this%m_exitOnError
end function

! ------------------------------------------------------------------------------
module subroutine er_set_exit_on_error(this, x)
    class(errors), intent(inout) :: this
    logical, intent(in) :: x
    this%m_exitOnError = x
end subroutine

! ------------------------------------------------------------------------------
pure module function er_get_suppress_printing(this) result(x)
    class(errors), intent(in) :: this
    logical :: x
    x = this%m_suppressPrinting
end function

! --------------------
module subroutine er_set_suppress_printing(this, x)
    class(errors), intent(inout) :: this
    logical, intent(in) :: x
    this%m_suppressPrinting = x
end subroutine

! ------------------------------------------------------------------------------
module function er_get_err_msg(this) result(msg)
    class(errors), intent(in) :: this
    character(len = :), allocatable :: msg
    integer(int32) :: n
    if (allocated(this%m_errorMessage)) then
        n = len(this%m_errorMessage)
        allocate(character(len = n) :: msg)
        msg = this%m_errorMessage(1:n)
    end if
end function

! ------------------------------------------------------------------------------
module function er_get_warning_msg(this) result(msg)
    class(errors), intent(in) :: this
    character(len = :), allocatable :: msg
    integer(int32) :: n
    if (allocated(this%m_warningMessage)) then
        n = len(this%m_warningMessage)
        allocate(character(len = n) :: msg)
        msg = this%m_warningMessage(1:n)
    end if
end function

! ------------------------------------------------------------------------------
module function er_get_err_fcn(this) result(fcn)
    class(errors), intent(in) :: this
    character(len = :), allocatable :: fcn
    integer(int32) :: n
    if (allocated(this%m_eFunName)) then
        n = len(this%m_eFunName)
        allocate(character(len = n) :: fcn)
        fcn = this%m_eFunName
    end if
end function

! ------------------------------------------------------------------------------
module function er_get_warning_fcn(this) result(fcn)
    class(errors), intent(in) :: this
    character(len = :), allocatable :: fcn
    integer(int32) :: n
    if (allocated(this%m_wFunName)) then
        n = len(this%m_wFunName)
        allocate(character(len = n) :: fcn)
        fcn = this%m_wFunName
    end if
end function

! ------------------------------------------------------------------------------
! module function er_get_err_fcn_ptr(this) result(ptr)
!     class(errors), intent(in) :: this
!     procedure(error_callback), pointer :: ptr
!     ptr => this%m_errCleanUp
! end function
module subroutine er_get_err_fcn_ptr(this, ptr)
    class(errors), intent(in) :: this
    procedure(error_callback), intent(out), pointer :: ptr
    ptr => this%m_errCleanUp
end subroutine

! ------------------------------------------------------------------------------
module subroutine er_set_err_fcn_ptr(this, ptr)
    class(errors), intent(inout) :: this
    procedure(error_callback), intent(in), pointer :: ptr
    this%m_errCleanUp => ptr
end subroutine

! ------------------------------------------------------------------------------
end submodule
