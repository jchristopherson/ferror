! ferror_c_binding.f90

!> @brief \b ferror
!!
!! @par Purpose
!! Provides C bindings to the ferror library.
module ferror_c_binding
    use, intrinsic :: iso_c_binding
    use ferror
    implicit none
    private
    public :: errorhandler
    public :: get_errorhandler
    public :: alloc_errorhandler
    public :: free_errorhandler
    public :: get_log_filename
    public :: set_log_filename
    public :: report_error
    public :: report_warning
    public :: log_error
    public :: has_error_occurred
    public :: reset_error_status
    public :: has_warning_occurred
    public :: reset_warning_status
    public :: get_error_flag
    public :: get_warning_flag
    public :: get_exit_on_error
    public :: set_exit_on_error
    public :: get_suppress_printing
    public :: set_suppress_printing
    public :: get_error_message
    public :: get_warning_message
    public :: get_error_fcn_name
    public :: get_warning_fcn_name
    public :: report_error_with_callback

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief A C compatible type encapsulating an errors object.
    type, bind(C) :: errorhandler
        !> @brief A pointer to the errors object.
        type(c_ptr) :: ptr
        !> @brief The size of the errors object, in bytes.
        integer(c_int) :: n
    end type

! ------------------------------------------------------------------------------
    interface
        subroutine c_error_callback(ptr)
            use, intrinsic :: iso_c_binding
            type(c_ptr), intent(in), value :: ptr
        end subroutine
    end interface

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
! ******************************************************************************
! TYPE CONSTRUCTION/DESTRUCTION ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Initializes a new error handler object.
    !!
    !! @param[in] obj The errorhandler object to allocate.
    subroutine alloc_errorhandler(obj) bind(C, name = "alloc_errorhandler")
        ! Arguments
        type(errorhandler), intent(inout) :: obj

        ! Local Variables
        type(errors), pointer :: ptr

        ! Process
        allocate(ptr)
        obj%n = sizeof(ptr)
        obj%ptr = c_loc(ptr)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Frees resources held by the errorhandler object.
    !!
    !! @param[in,out] obj The errorhandler object.
    subroutine free_errorhandler(obj) bind(C, name = "free_errorhandler")
        ! Arguments
        type(errorhandler), intent(inout), target :: obj

        ! Local Variables
        type(c_ptr) :: testptr
        type(errorhandler), pointer :: err

        ! Process
        testptr = c_loc(obj)
        if (.not.c_associated(testptr)) return
        if (.not.c_associated(obj%ptr)) return
        call c_f_pointer(obj%ptr, err)
        if (associated(err)) deallocate(err)
        obj%n = 0
        obj%ptr = c_null_ptr
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Retrieves the errors object from the C compatible data structure.
    !!
    !! @param[in] obj The C compatible errorhandler data structure.
    !! @param[out] eobj The resulting errors object.
    subroutine get_errorhandler(obj, eobj)
        ! Arguments
        type(errorhandler), intent(in), target :: obj
        type(errors), intent(out), pointer :: eobj

        ! Process
        type(c_ptr) :: testptr
        testptr = c_loc(obj)
        nullify(eobj)
        if (.not.c_associated(testptr)) return
        if (.not.c_associated(obj%ptr)) return
        if (obj%n == 0) return
        call c_f_pointer(obj%ptr, eobj)
    end subroutine

! ******************************************************************************
! ERROR HANDLER ACCESS ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Gets the name of the error log file.
    !!
    !! @param[in] err The errorhandler object.
    !! @param[out] fname A character buffer where the filename will be written.
    !!  It is recommended that this be in the neighborhood of 256 elements.
    !! @param[in,out] nfname On input, the actual size of the buffer.  Be sure
    !!  to leave room for the null terminator character.  On output, the actual
    !!  number of characters written to @p fname (not including the null
    !!  character).
    subroutine get_log_filename(err, fname, nfname) &
            bind(C, name = "get_log_filename")
        ! Arguments
        type(errorhandler), intent(in) :: err
        character(kind = c_char), intent(out) :: fname(*)
        integer(c_int), intent(inout) :: nfname

        ! Local Variables
        type(errors), pointer :: ferr
        character(len = :), allocatable :: fstr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.associated(ferr)) then
            nfname = 0
            return
        end if

        ! Process
        fstr = ferr%get_log_filename()
        call fstr_2_cstr(fstr, fname, nfname)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Sets the error log filename.
    !!
    !! @param[in,out] err The errorhandler object.
    !! @param[in] fname A null-terminated string containing the filename.
    subroutine set_log_filename(err, fname) &
            bind(C, name = "set_log_filename")
        ! Arguments
        type(errorhandler), intent(inout) :: err
        character(kind = c_char), intent(in) :: fname(*)

        ! Local Variables
        type(errors), pointer :: ferr
        character(len = :), allocatable :: fstr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.associated(ferr)) return

        ! Process
        fstr = cstr_2_fstr(fname)
        call ferr%set_log_filename(fstr)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Reports an error condition to the user.
    !!
    !! @param[in,out] err A pointer to the error handler object.
    !! @param[in] fcn The name of the function or subroutine in which the error
    !!  was encountered.
    !! @param[in] msg The error message.
    !! @param[in] flag The error flag.
    subroutine report_error(err, fcn, msg, flag) &
            bind(C, name = "report_error")
        ! Arguments
        type(errorhandler), intent(inout) :: err
        character(kind = c_char), intent(in) :: fcn, msg
        integer(c_int), intent(in), value :: flag

        ! Local Variables
        type(errors), pointer :: ferr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.associated(ferr)) return

        ! Report the error
        call ferr%report_error(cstr_2_fstr(fcn), cstr_2_fstr(msg), flag)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Reports a warning condition to the user.
    !!
    !! @param[in,out] err The errorhandler object.
    !! @param[in] fcn The name of the function or subroutine in which the
    !!  warning was encountered.
    !! @param[in] msg The warning message.
    !! @param[in] flag The warning flag.
    subroutine report_warning(err, fcn, msg, flag) &
            bind(C, name = "report_warning")
        ! Arguments
        type(errorhandler), intent(inout) :: err
        character(kind = c_char), intent(in) :: fcn, msg
        integer(c_int), intent(in), value :: flag

        ! Local Variables
        type(errors), pointer :: ferr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.associated(ferr)) return

        ! Report the warning
        call ferr%report_warning(cstr_2_fstr(fcn), cstr_2_fstr(msg), flag)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes an error log file.
    !!
    !! @param[in] err The errorhandler object.
    !! @param[in] fcn The name of the function or subroutine in which the error
    !!  was encountered.
    !! @param[in] msg The error message.
    !! @param[in] flag The error flag.
    subroutine log_error(err, fcn, msg, flag) &
            bind(C, name = "log_error")
        ! Arguments
        type(errorhandler), intent(in) :: err
        character(kind = c_char), intent(in) :: fcn, msg
        integer(c_int), intent(in), value :: flag

        ! Local Variables
        type(errors), pointer :: ferr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.associated(ferr)) return

        ! Log the error
        call ferr%log_error(cstr_2_fstr(fcn), cstr_2_fstr(msg), flag)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Tests to see if an error has been encountered.
    !!
    !! @param[in] err A pointer to the error handler object.
    !! @return Returns true if an error has been encountered; else, false.
    function has_error_occurred(err) result(x) &
            bind(C, name = "has_error_occurred")
        ! Arguments
        type(errorhandler), intent(in) :: err
        logical(c_bool) :: x

        ! Local Variables
        type(errors), pointer :: ferr

        ! Get the errors object
        x = .false.
        call get_errorhandler(err, ferr)
        if (.not.associated(ferr)) return

        ! Process
        x = ferr%has_error_occurred()
    end function

! ------------------------------------------------------------------------------
    !> @brief Resets the error status flag to false, and the current error flag
    !! to zero.
    !!
    !! @param[in,out] err The errorhandler object.
    subroutine reset_error_status(err) bind(C, name = "reset_error_status")
        ! Arguments
        type(errorhandler), intent(inout) :: err

        ! Local Variables
        type(errors), pointer :: ferr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.associated(ferr)) return

        ! Process
        call ferr%reset_error_status()
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Tests to see if a warning has been encountered.
    !!
    !! @param[in] err The errorhandler object.
    !! @return Returns true if a warning has been encountered; else, false.
    function has_warning_occurred(err) result(x) &
            bind(C, name = "has_warning_occurred")
        ! Arguments
        type(errorhandler), intent(in) :: err
        logical(c_bool) :: x

        ! Local Variables
        type(errors), pointer :: ferr

        ! Get the errors object
        x = .false.
        call get_errorhandler(err, ferr)
        if (.not.associated(ferr)) return

        ! Process
        x = ferr%has_warning_occurred()
    end function

! ------------------------------------------------------------------------------
    !> @brief Resets the warning status flag to false, and the current warning
    !! flag to zero.
    !!
    !! @param[in,out] err The errorhandler object.
    subroutine reset_warning_status(err) bind(C, name = "reset_warning_status")
        ! Arguments
        type(errorhandler), intent(inout) :: err

        ! Local Variables
        type(errors), pointer :: ferr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.associated(ferr)) return

        ! Process
        call ferr%reset_warning_status()
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the current error flag.
    !!
    !! @param[in] err The errorhandler object.
    !! @return The current error flag.
    function get_error_flag(err) result(x) bind(C, name = "get_error_flag")
        ! Arguments
        type(errorhandler), intent(in) :: err
        integer(c_int) :: x

        ! Local Variables
        type(errors), pointer :: ferr

        ! Get the errors object
        x = 0
        call get_errorhandler(err, ferr)
        if (.not.associated(ferr)) return

        ! Process
        x = ferr%get_error_flag()
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the current warning flag.
    !!
    !! @param[in] err The errorhandler object.
    !! @return The current warning flag.
    function get_warning_flag(err) result(x) &
            bind(C, name = "get_warning_flag")
        ! Arguments
        type(errorhandler), intent(in) :: err
        integer(c_int) :: x

        ! Local Variables
        type(errors), pointer :: ferr

        ! Get the errors object
        x = 0
        call get_errorhandler(err, ferr)
        if (.not.associated(ferr)) return

        ! Process
        x = ferr%get_warning_flag()
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets a logical value determining if the application should be
    !! terminated when an error is encountered.
    !!
    !! @param[in] err The errorhandler object.
    !! @return Returns true if the application should be terminated; else,
    !!  false.
    function get_exit_on_error(err) result(x) &
            bind(C, name = "get_exit_on_error")
        ! Arguments
        type(errorhandler), intent(in) :: err
        logical(c_bool) :: x

        ! Local Variables
        type(errors), pointer :: ferr

        ! Get the errors object
        x = .true.
        call get_errorhandler(err, ferr)
        if (.not.associated(ferr)) return

        ! Process
        x = ferr%get_exit_on_error()
    end function

! ------------------------------------------------------------------------------
    !> @brief Sets a logical value determining if the application should be
    !! terminated when an error is encountered.
    !!
    !! @param[in,out] err The errorhandler object.
    !! @param[in] x Set to true if the application should be terminated when an
    !!  error is reported; else, false.
    subroutine set_exit_on_error(err, x) bind(C, name = "set_exit_on_error")
        ! Arguments
        type(errorhandler), intent(inout) :: err
        logical(c_bool), intent(in), value :: x

        ! Local Variables
        type(errors), pointer :: ferr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.associated(ferr)) return

        ! Process
        call ferr%set_exit_on_error(logical(x))
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a logical value determining if printing of error and warning
    !! messages should be suppressed.
    !!
    !! @param[in] err The errorhandler object.
    !! @return True if message printing should be suppressed; else, false to 
    !!  allow printing.
    function get_suppress_printing(err) result(x) &
            bind(C, name = "get_suppress_printing")
        ! Arguments
        type(errorhandler), intent(in) :: err
        logical(c_bool) :: x

        ! Local Variables
        type(errors), pointer :: ferr
        
        ! Get the errors object
        x = .false.
        call get_errorhandler(err, ferr)
        if (.not.associated(ferr)) return

        ! Process
        x = ferr%get_suppress_printing()
    end function

! ------------------------------------------------------------------------------
    !> @brief Sets a logical value determining if printing of error and warning
    !! messages should be suppressed.
    !!
    !! @param[in,out] err The errorhandler object.
    !! @param[in] x Set to true if message printing should be suppressed; else,
    !!  false to allow printing.
    subroutine set_suppress_printing(err, x) &
            bind(C, name = "set_suppress_printing")
        ! Arguments
        type(errorhandler), intent(inout) :: err
        logical(c_bool), intent(in), value :: x

        ! Local Variables
        type(errors), pointer :: ferr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.associated(ferr)) return

        ! Process
        call ferr%set_suppress_printing(logical(x))
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the current error message.
    !!
    !! @param[in] err The errorhandler object.
    !! @param[out] mst A character buffer where the message will be written.
    !! @param[in,out] nmsg On input, the actual size of the buffer.  On output,
    !!  the actual number of characters written to @p msg (not including the 
    !!  null character).
    subroutine get_error_message(err, msg, nmsg) &
            bind(C, name = "get_error_message")
        ! Arguments
        type(errorhandler), intent(in) :: err
        character(kind = c_char), intent(out) :: msg(*)
        integer(c_int), intent(inout) :: nmsg

        ! Local Variables
        type(errors), pointer :: ferr
        character(len = :), allocatable :: fstr

        ! Get the errors object
        call get_errorhandler(err, ferr)
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
    !> @brief Gets the current warning message.
    !!
    !! @param[in] err The errorhandler object.
    !! @param[out] mst A character buffer where the message will be written.
    !! @param[in,out] nmsg On input, the actual size of the buffer.  On output,
    !!  the actual number of characters written to @p msg (not including the 
    !!  null character).
    subroutine get_warning_message(err, msg, nmsg) &
            bind(C, name = "get_warning_message")
        ! Arguments
        type(errorhandler), intent(in) :: err
        character(kind = c_char), intent(out) :: msg(*)
        integer(c_int), intent(inout) :: nmsg

        ! Local Variables
        type(errors), pointer :: ferr
        character(len = :), allocatable :: fstr

        ! Get the errors object
        call get_errorhandler(err, ferr)
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
    !> @brief Gets the name of the function or subroutine that issued the last
    !! error message.
    !!
    !! @param[in] err The errorhandler object.
    !! @param[out] fname A character buffer where the name will be written.
    !! @param[in,out] nfname On input, the actual size of the buffer.  On 
    !!  output, the actual number of characters written to @p fname (not
    !!  including the null character).
    subroutine get_error_fcn_name(err, fname, nfname) &
            bind(C, name = "get_error_fcn_name")
        ! Arguments
        type(errorhandler), intent(in) :: err
        character(kind = c_char), intent(out) :: fname(*)
        integer(c_int), intent(inout) :: nfname

        ! Local Variables
        type(errors), pointer :: ferr
        character(len = :), allocatable :: fstr

        ! Get the errors object
        call get_errorhandler(err, ferr)
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
    !> @brief Gets the name of the function or subroutine that issued the last
    !! warning message.
    !!
    !! @param[in] err The errorhandler object.
    !! @param[out] fname A character buffer where the name will be written.
    !! @param[in,out] nfname On input, the actual size of the buffer.  On 
    !!  output, the actual number of characters written to @p fname (not
    !!  including the null character).
    subroutine get_warning_fcn_name(err, fname, nfname) &
            bind(C, name = "get_warning_fcn_name")
        ! Arguments
        type(errorhandler), intent(in) :: err
        character(kind = c_char), intent(out) :: fname(*)
        integer(c_int), intent(inout) :: nfname

        ! Local Variables
        type(errors), pointer :: ferr
        character(len = :), allocatable :: fstr

        ! Get the errors object
        call get_errorhandler(err, ferr)
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
    !> @brief Reports an error condition to the user, and executes a callback
    !! routine.
    !!
    !! @param[in,out] err A pointer to the error handler object.
    !! @param[in] fcn The name of the function or subroutine in which the error
    !!  was encountered.
    !! @param[in] msg The error message.
    !! @param[in] flag The error flag.
    !! @param[in] cback A pointer to the callback function.
    !! @param[in] args A pointer to an object to pass to the callback function.
    subroutine report_error_with_callback(err, fcn, msg, flag, cback, args) &
            bind(C, name = "report_error_with_callback")
        ! Arguments
        type(errorhandler), intent(inout) :: err
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
        call get_errorhandler(err, ferr)
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
    !> @brief Copies a C string (null terminated) to a Fortran string.
    !!
    !! @param[in] cstr The null-terminated C string.
    !! @return The Fortran copy.
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
    !> @brief Copies a Fortran string into a C string.
    !!
    !! @param[in] fstr The Fortran string to copy.
    !! @param[out] cstr The null-terminated C string.
    !! @param[in,out] csize On input, the size of the character buffer @p cstr.
    !!  On output, the actual number of characters (not including the null
    !!  character) writen to @p cstr.
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
    !
    subroutine err_callback(this, obj)
        class(errors), intent(in) :: this
        class(*), intent(inout) :: obj
        select type (obj)
        class is (callback_manager)
            call obj%fcn(obj%args)
        end select
    end subroutine

! ------------------------------------------------------------------------------
end module
