submodule (ferror_c_binding) ferror_c_implementation
! ------------------------------------------------------------------------------
    !> @brief A type that allows C interop with the Fortran error callback
    !! routine.
    type :: callback_manager
        !> A pointer to the C callback routine.
        procedure(c_error_callback), pointer, nopass :: fcn
        !> A pointer to the C-supplied arguments.
        type(c_ptr) :: args
    end type

    
contains
! ------------------------------------------------------------------------------
module subroutine alloc_error_handler(obj) bind(C, name = "alloc_error_handler")
    ! Arguments
    type(error_handler), intent(inout) :: obj

    ! Local Variables
    type(errors), pointer :: ptr

    ! Process
    allocate(ptr)
    obj%object_size = sizeof(ptr)
    obj%ptr = c_loc(ptr)
end subroutine

! ------------------------------------------------------------------------------
module subroutine free_error_handler(obj) bind(C, name = "free_error_handler")
    ! Arguments
    type(error_handler), intent(inout), target :: obj

    ! Local Variables
    type(c_ptr) :: testptr
    type(error_handler), pointer :: err

    ! Process
    testptr = c_loc(obj)
    if (.not.c_associated(testptr)) return
    if (.not.c_associated(obj%ptr)) return
    call c_f_pointer(obj%ptr, err)
    if (associated(err)) deallocate(err)
    obj%object_size = 0
    obj%ptr = c_null_ptr
end subroutine

! ------------------------------------------------------------------------------
module subroutine get_error_handler(obj, eobj)
    ! Arguments
    type(error_handler), intent(in), target :: obj
    type(errors), intent(out), pointer :: eobj

    ! Process
    type(c_ptr) :: testptr
    testptr = c_loc(obj)
    nullify(eobj)
    if (.not.c_associated(testptr)) return
    if (.not.c_associated(obj%ptr)) return
    if (obj%object_size == 0) return
    call c_f_pointer(obj%ptr, eobj)
end subroutine

! ******************************************************************************
! ERROR HANDLER ACCESS ROUTINES
! ------------------------------------------------------------------------------
module subroutine get_log_filename(err, fname, nfname) &
        bind(C, name = "get_log_filename")
    ! Arguments
    type(error_handler), intent(in) :: err
    character(kind = c_char), intent(out) :: fname(*)
    integer(c_int), intent(inout) :: nfname

    ! Local Variables
    type(errors), pointer :: ferr
    character(len = :), allocatable :: fstr

    ! Get the errors object
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) then
        nfname = 0
        return
    end if

    ! Process
    fstr = ferr%get_log_filename()
    call fstr_2_cstr(fstr, fname, nfname)
end subroutine

! ------------------------------------------------------------------------------
module subroutine set_log_filename(err, fname) &
        bind(C, name = "set_log_filename")
    ! Arguments
    type(error_handler), intent(inout) :: err
    character(kind = c_char), intent(in) :: fname(*)

    ! Local Variables
    type(errors), pointer :: ferr
    character(len = :), allocatable :: fstr

    ! Get the errors object
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) return

    ! Process
    fstr = cstr_2_fstr(fname)
    call ferr%set_log_filename(fstr)
end subroutine

! ------------------------------------------------------------------------------
module subroutine report_error(err, fcn, msg, flag) &
        bind(C, name = "report_error")
    ! Arguments
    type(error_handler), intent(inout) :: err
    character(kind = c_char), intent(in) :: fcn, msg
    integer(c_int), intent(in), value :: flag

    ! Local Variables
    type(errors), pointer :: ferr

    ! Get the errors object
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) return

    ! Report the error
    call ferr%report_error(cstr_2_fstr(fcn), cstr_2_fstr(msg), flag)
end subroutine

! ------------------------------------------------------------------------------
module subroutine report_warning(err, fcn, msg, flag) &
        bind(C, name = "report_warning")
    ! Arguments
    type(error_handler), intent(inout) :: err
    character(kind = c_char), intent(in) :: fcn, msg
    integer(c_int), intent(in), value :: flag

    ! Local Variables
    type(errors), pointer :: ferr

    ! Get the errors object
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) return

    ! Report the warning
    call ferr%report_warning(cstr_2_fstr(fcn), cstr_2_fstr(msg), flag)
end subroutine

! ------------------------------------------------------------------------------
module subroutine log_error(err, fcn, msg, flag) &
        bind(C, name = "log_error")
    ! Arguments
    type(error_handler), intent(in) :: err
    character(kind = c_char), intent(in) :: fcn, msg
    integer(c_int), intent(in), value :: flag

    ! Local Variables
    type(errors), pointer :: ferr

    ! Get the errors object
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) return

    ! Log the error
    call ferr%log_error(cstr_2_fstr(fcn), cstr_2_fstr(msg), flag)
end subroutine

! ------------------------------------------------------------------------------
module function has_error_occurred(err) result(x) &
        bind(C, name = "has_error_occurred")
    ! Arguments
    type(error_handler), intent(in) :: err
    logical(c_bool) :: x

    ! Local Variables
    type(errors), pointer :: ferr

    ! Get the errors object
    x = .false.
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) return

    ! Process
    x = ferr%has_error_occurred()
end function

! ------------------------------------------------------------------------------
module subroutine reset_error_status(err) bind(C, name = "reset_error_status")
    ! Arguments
    type(error_handler), intent(inout) :: err

    ! Local Variables
    type(errors), pointer :: ferr

    ! Get the errors object
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) return

    ! Process
    call ferr%reset_error_status()
end subroutine

! ------------------------------------------------------------------------------
module function has_warning_occurred(err) result(x) &
        bind(C, name = "has_warning_occurred")
    ! Arguments
    type(error_handler), intent(in) :: err
    logical(c_bool) :: x

    ! Local Variables
    type(errors), pointer :: ferr

    ! Get the errors object
    x = .false.
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) return

    ! Process
    x = ferr%has_warning_occurred()
end function

! ------------------------------------------------------------------------------
module subroutine reset_warning_status(err) bind(C, name = "reset_warning_status")
    ! Arguments
    type(error_handler), intent(inout) :: err

    ! Local Variables
    type(errors), pointer :: ferr

    ! Get the errors object
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) return

    ! Process
    call ferr%reset_warning_status()
end subroutine

! ------------------------------------------------------------------------------
module function get_error_flag(err) result(x) bind(C, name = "get_error_flag")
    ! Arguments
    type(error_handler), intent(in) :: err
    integer(c_int) :: x

    ! Local Variables
    type(errors), pointer :: ferr

    ! Get the errors object
    x = 0
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) return

    ! Process
    x = ferr%get_error_flag()
end function

! ------------------------------------------------------------------------------
module function get_warning_flag(err) result(x) &
        bind(C, name = "get_warning_flag")
    ! Arguments
    type(error_handler), intent(in) :: err
    integer(c_int) :: x

    ! Local Variables
    type(errors), pointer :: ferr

    ! Get the errors object
    x = 0
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) return

    ! Process
    x = ferr%get_warning_flag()
end function

! ------------------------------------------------------------------------------
module function get_exit_on_error(err) result(x) &
        bind(C, name = "get_exit_on_error")
    ! Arguments
    type(error_handler), intent(in) :: err
    logical(c_bool) :: x

    ! Local Variables
    type(errors), pointer :: ferr

    ! Get the errors object
    x = .true.
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) return

    ! Process
    x = ferr%get_exit_on_error()
end function

! ------------------------------------------------------------------------------
module subroutine set_exit_on_error(err, x) bind(C, name = "set_exit_on_error")
    ! Arguments
    type(error_handler), intent(inout) :: err
    logical(c_bool), intent(in), value :: x

    ! Local Variables
    type(errors), pointer :: ferr

    ! Get the errors object
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) return

    ! Process
    call ferr%set_exit_on_error(logical(x))
end subroutine

! ------------------------------------------------------------------------------
module function get_suppress_printing(err) result(x) &
        bind(C, name = "get_suppress_printing")
    ! Arguments
    type(error_handler), intent(in) :: err
    logical(c_bool) :: x

    ! Local Variables
    type(errors), pointer :: ferr
    
    ! Get the errors object
    x = .false.
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) return

    ! Process
    x = ferr%get_suppress_printing()
end function

! ------------------------------------------------------------------------------
module subroutine set_suppress_printing(err, x) &
        bind(C, name = "set_suppress_printing")
    ! Arguments
    type(error_handler), intent(inout) :: err
    logical(c_bool), intent(in), value :: x

    ! Local Variables
    type(errors), pointer :: ferr

    ! Get the errors object
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) return

    ! Process
    call ferr%set_suppress_printing(logical(x))
end subroutine

! ------------------------------------------------------------------------------
module subroutine get_error_message(err, msg, nmsg) &
        bind(C, name = "get_error_message")
    ! Arguments
    type(error_handler), intent(in) :: err
    character(kind = c_char), intent(out) :: msg(*)
    integer(c_int), intent(inout) :: nmsg

    ! Local Variables
    type(errors), pointer :: ferr
    character(len = :), allocatable :: fstr

    ! Get the errors object
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) then
        nmsg = 0
        return
    end if

    ! Process
    fstr = ferr%get_error_message()
    if (.not.allocated(fstr)) then
        nmsg = 0
        return
    end if
    call fstr_2_cstr(fstr, msg, nmsg)
end subroutine

! ------------------------------------------------------------------------------
module subroutine get_warning_message(err, msg, nmsg) &
        bind(C, name = "get_warning_message")
    ! Arguments
    type(error_handler), intent(in) :: err
    character(kind = c_char), intent(out) :: msg(*)
    integer(c_int), intent(inout) :: nmsg

    ! Local Variables
    type(errors), pointer :: ferr
    character(len = :), allocatable :: fstr

    ! Get the errors object
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) then
        nmsg = 0
        return
    end if

    ! Process
    fstr = ferr%get_warning_message()
    if (.not.allocated(fstr)) then
        nmsg = 0
        return
    end if
    call fstr_2_cstr(fstr, msg, nmsg)
end subroutine

! ------------------------------------------------------------------------------
module subroutine get_error_fcn_name(err, fname, nfname) &
        bind(C, name = "get_error_fcn_name")
    ! Arguments
    type(error_handler), intent(in) :: err
    character(kind = c_char), intent(out) :: fname(*)
    integer(c_int), intent(inout) :: nfname

    ! Local Variables
    type(errors), pointer :: ferr
    character(len = :), allocatable :: fstr

    ! Get the errors object
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) then
        nfname = 0
        return
    end if

    ! Process
    fstr = ferr%get_error_fcn_name()
    if (.not.allocated(fstr)) then
        nfname = 0
        return
    end if
    call fstr_2_cstr(fstr, fname, nfname)
end subroutine

! ------------------------------------------------------------------------------
module subroutine get_warning_fcn_name(err, fname, nfname) &
        bind(C, name = "get_warning_fcn_name")
    ! Arguments
    type(error_handler), intent(in) :: err
    character(kind = c_char), intent(out) :: fname(*)
    integer(c_int), intent(inout) :: nfname

    ! Local Variables
    type(errors), pointer :: ferr
    character(len = :), allocatable :: fstr

    ! Get the errors object
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) then
        nfname = 0
        return
    end if

    ! Process
    fstr = ferr%get_warning_fcn_name()
    if (.not.allocated(fstr)) then
        nfname = 0
        return
    end if
    call fstr_2_cstr(fstr, fname, nfname)
end subroutine

! ------------------------------------------------------------------------------
module subroutine report_error_with_callback(err, fcn, msg, flag, cback, args) &
        bind(C, name = "report_error_with_callback")
    ! Arguments
    type(error_handler), intent(inout) :: err
    character(kind = c_char), intent(in) :: fcn, msg
    integer(c_int), intent(in), value :: flag
    procedure(error_callback), pointer :: ffcn
    type(c_funptr), intent(in), value :: cback
    type(c_ptr), intent(in), value :: args

    ! Local Variables
    type(errors), pointer :: ferr
    type(callback_manager) :: mgr
    procedure(c_error_callback), pointer :: fcback

    ! Get the errors object
    call get_error_handler(err, ferr)
    if (.not.associated(ferr)) return

    ! Define the callback
    call c_f_procpointer(cback, fcback)
    mgr%fcn => fcback
    mgr%args = args
    ffcn => err_callback
    call ferr%set_clean_up_routine(ffcn)

    ! Report the error
    call ferr%report_error(cstr_2_fstr(fcn), cstr_2_fstr(msg), flag, mgr)
end subroutine

! ******************************************************************************
! HELPER ROUTINES
! ------------------------------------------------------------------------------
function cstr_2_fstr(cstr) result(fstr)
    ! Arguments
    character(kind = c_char), intent(in) :: cstr(*)
    character(len = :), allocatable :: fstr

    ! Parameters
    integer, parameter :: maxchar = 5000 ! Maximum allowed string length

    ! Local Variables
    integer :: i, n

    ! Determine the length of the C string
    n = 0
    do i = 1, maxchar
        if (cstr(i) == c_null_char) exit
        n = n + 1
    end do

    ! Process
     if (n == 0) return
     allocate(character(len = n) :: fstr)
     do i = 1, n
        fstr(i:i) = cstr(i)
     end do
end function

! ------------------------------------------------------------------------------
subroutine fstr_2_cstr(fstr, cstr, csize)
    ! Arguments
    character(len = *), intent(in) :: fstr
    character(kind = c_char), intent(out) :: cstr(*)
    integer, intent(inout) :: csize

    ! Local Variables
    integer :: i, n

    ! Process
    n = min(len(fstr), csize - 1) ! -1 allows room for the null char
    do i = 1, n
        cstr(i) = fstr(i:i)
    end do
    cstr(n+1) = c_null_char
    csize = n
end subroutine

! ------------------------------------------------------------------------------
subroutine err_callback(this, obj)
    class(errors), intent(in) :: this
    class(*), intent(inout) :: obj
    select type (obj)
    class is (callback_manager)
        call obj%fcn(obj%args)
    end select
end subroutine

! ------------------------------------------------------------------------------
end submodule