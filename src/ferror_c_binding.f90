! ferror_c_binding.f90

!> @brief \b ferror
!!
!! @par Purpose
!! Provides C bindings to the ferror library.
module ferror_c_binding
    use, intrinsic :: iso_c_binding
    use ferror
    implicit none

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief A C compatible type encapsulating an errors object.
    type, bind(C) :: errorhandler
        !> @brief The size of the errors object, in bytes.
        integer(c_int) :: n
        !> @brief A pointer to the errors object.
        type(c_ptr) :: ptr
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
        integer(c_short), allocatable, target, dimension(:) :: temp
        type(errors) :: eobj

        ! Process
        temp = transfer(eobj, temp)
        obj%n = size(temp)
        obj%ptr = c_loc(temp(1))
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Retrieves the errors object from the C compatible data structure.
    !!
    !! @param[in] obj The C compatible errorhandler data structure.
    !! @param[out] eobj The resulting errors object.
    subroutine get_errorhandler(obj, eobj)
        ! Arguments
        type(errorhandler), intent(in), target :: obj
        class(errors), intent(out), allocatable :: eobj

        ! Local Variables
        integer(c_short), pointer, dimension(:) :: temp
        type(errors) :: item
        type(c_ptr) :: testptr

        ! Process
        testptr = c_loc(obj) ! Ensures that obj wasn't passed as NULL from C
        if (.not.c_associated(testptr)) return
        if (.not.c_associated(obj%ptr)) return
        call c_f_pointer(obj%ptr, temp, shape = [obj%n])
        item = transfer(temp, item)
        allocate(eobj, source = item)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Updates the errorhandler object.
    !!
    !! @param[in] eobj The errors object.
    !! @param[in,out] cobj The errorhandler object to update.
    subroutine update_errorhandler(eobj, cobj)
        ! Arguments
        class(errors), intent(in) :: eobj
        type(errorhandler), intent(inout) :: cobj

        ! Local Variables
        integer(c_short), allocatable, target, dimension(:) :: temp

        ! Process
        temp = transfer(eobj, temp)
        cobj%n = size(temp)
        cobj%ptr = c_loc(temp(1))
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
    !!  numbers of characters written to @p fname (not including the null
    !!  character).
    subroutine get_log_filename(err, fname, nfname) &
            bind(C, name = "get_log_filename")
        ! Arguments
        type(errorhandler), intent(in) :: err
        character(kind = c_char), intent(out) :: fname(*)
        integer(c_int), intent(inout) :: nfname

        ! Local Variables
        class(errors), allocatable :: ferr
        character(len = :), allocatable :: fstr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.allocated(ferr)) then
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
        class(errors), allocatable :: ferr
        character(len = :), allocatable :: fstr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.allocated(ferr)) return

        ! Process
        fstr = cstr_2_fstr(fname)
        call ferr%set_log_filename(fstr)
        call update_errorhandler(ferr, err)
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
        class(errors), allocatable :: ferr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.allocated(ferr)) return

        ! Report the error
        call ferr%report_error(cstr_2_fstr(fcn), cstr_2_fstr(msg), flag)
        call update_errorhandler(ferr, err)
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
        class(errors), allocatable :: ferr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.allocated(ferr)) return

        ! Report the warning
        call ferr%report_warning(cstr_2_fstr(fcn), cstr_2_fstr(msg), flag)
        call update_errorhandler(ferr, err)
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
        class(errors), allocatable :: ferr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.allocated(ferr)) return

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
        class(errors), allocatable :: ferr

        ! Get the errors object
        x = .false.
        call get_errorhandler(err, ferr)
        if (.not.allocated(ferr)) return

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
        class(errors), allocatable :: ferr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.allocated(ferr)) return

        ! Process
        call ferr%reset_error_status()
        call update_errorhandler(ferr, err)
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
        class(errors), allocatable :: ferr

        ! Get the errors object
        x = .false.
        call get_errorhandler(err, ferr)
        if (.not.allocated(ferr)) return

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
        class(errors), allocatable :: ferr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.allocated(ferr)) return

        ! Process
        call ferr%reset_warning_status()
        call update_errorhandler(ferr, err)
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
        class(errors), allocatable :: ferr

        ! Get the errors object
        x = 0
        call get_errorhandler(err, ferr)
        if (.not.allocated(ferr)) return

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
        class(errors), allocatable :: ferr

        ! Get the errors object
        x = 0
        call get_errorhandler(err, ferr)
        if (.not.allocated(ferr)) return

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
        class(errors), allocatable :: ferr

        ! Get the errors object
        x = .true.
        call get_errorhandler(err, ferr)
        if (.not.allocated(ferr)) return

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
        class(errors), allocatable :: ferr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.allocated(ferr)) return

        ! Process
        call ferr%set_exit_on_error(logical(x))
        call update_errorhandler(ferr, err)
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
        class(errors), allocatable :: ferr
        
        ! Get the errors object
        x = .false.
        call get_errorhandler(err, ferr)
        if (.not.allocated(ferr)) return

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
        class(errors), allocatable :: ferr

        ! Get the errors object
        call get_errorhandler(err, ferr)
        if (.not.allocated(ferr)) return

        ! Process
        call ferr%set_suppress_printing(logical(x))
        call update_errorhandler(ferr, err)
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
        integer :: i

        ! Process
        do i = 1, min(len(fstr), csize - 1) ! -1 allows room for the null char
            cstr(i) = fstr(i:i)
        end do
        cstr(i+1) = c_null_char
        csize = i
    end subroutine

! ------------------------------------------------------------------------------
end module
