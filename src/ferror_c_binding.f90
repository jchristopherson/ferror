! ferror_c_binding.f90

module ferror_c_binding
    use, intrinsic :: iso_c_binding
    use ferror
    implicit none

! ******************************************************************************
! INTERFACES
! ------------------------------------------------------------------------------
    ! C standard library routines - must compile with -lc.
    interface
        function strlen(str) result(n) bind(C, name = "strlen")
            import
            character(kind = c_char), intent(in) :: str(*)
            integer(c_int) :: n
        end function
    end interface

contains
! ******************************************************************************
! TYPE CONSTRUCTION/DESTRUCTION ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Initializes a pointer to a new error handler object.
    !!
    !! @return The pointer to the newly created error handler object.
    function alloc_error_handler() result(ptr) &
            bind(C, name = "alloc_error_handler")
        ! Arguments
        type(c_ptr) :: ptr

        ! Create a new errors object, and then associate the C pointer
        type(errors), pointer :: fptr
        allocate(fptr)
        ptr = c_loc(fptr)
    end function

! ------------------------------------------------------------------------------
    !> @brief Cleans up an error handler object.
    !!
    !! @param[in,out] ptr The pointer to the error handler object.
    subroutine free_error_handler(ptr) bind(C, name = "free_error_handler")
        ! Arguments
        type(c_ptr), intent(inout) :: ptr

        ! Local Variables
        type(errors), pointer :: fptr

        ! Ensure the pointer isn't null
        if (.not.c_associated(ptr)) return

        ! Free memory
        call c_f_pointer(ptr, fptr)
        deallocate(fptr)
        ptr = c_null_ptr
    end subroutine

! ******************************************************************************
! ERROR HANDLER ACCESS ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Gets the name of the error log file.
    !!
    !! @param[in] err A pointer to the error handler object.
    !! @param[out] fname A character buffer where the filename will be written.
    !!  It is recommended that this be in the neighborhood of 256 elements.
    !! @param[in,out] nfname On input, the actual size of the buffer.  Be sure
    !!  to leave room for the null terminator character.  On output, the actual
    !!  numbers of characters written to @p fname (not including the null
    !!  character).
    subroutine get_error_log_fname(err, fname, nfname) &
            bind(C, name = "get_error_log_fname")
        ! Arguments
        type(c_ptr), intent(in), value :: err
        character(kind = c_char), intent(out) :: fname(*)
        integer(c_int), intent(inout) :: nfname

        ! Local Variables
        type(errors), pointer :: ferr
        character(len = :), allocatable :: fstr

        ! Ensure the pointer from C is not null
        if (.not.c_associated(err)) then
            nfname = 0
            return
        end if

        ! Process
        call c_f_pointer(err, ferr)
        fstr = ferr%get_log_filename()
        call fstr_2_cstr(fstr, fname, nfname)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Sets the error log filename.
    !!
    !! @param[in,out] err A pointer to the error handler object.
    !! @param[in] fname A null-terminated string containing the filename.
    subroutine set_error_log_fname(err, fname) &
            bind(C, name = "set_error_log_fname")
        ! Arguments
        type(c_ptr), intent(in), value :: err
        character(kind = c_char), intent(in) :: fname(*)

        ! Local Variables
        type(errors), pointer :: ferr
        character(len = :), allocatable :: fstr

        ! Ensure the pointer from C is not null
        if (.not.c_associated(err)) return

        ! Process
        call c_f_pointer(err, ferr)
        fstr = cstr_2_fstr(fname)
        call ferr%set_log_filename(fstr)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Reports an error condition to the user.
    !!
    !! @param[in] err A pointer to the error handler object.
    !! @param[in] fcn The name of the function or subroutine in which the error
    !!  was encountered.
    !! @param[in] msg The error message.
    !! @param[in] flag The error flag.
    subroutine register_error(err, fcn, msg, flag) &
            bind(C, name = "register_error")
        ! Arguments
        type(c_ptr), intent(in), value :: err
        character(kind = c_char), intent(in) :: fcn, msg
        integer(c_int), intent(in), value :: flag

        ! Process
        type(errors), pointer :: ferr
        if (.not.c_associated(err)) return
        call c_f_pointer(err, ferr)
        call ferr%report_error(cstr_2_fstr(fcn), cstr_2_fstr(msg), flag)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Reports a warning condition to the user.
    !!
    !! @param[in] err A pointer to the error handler object.
    !! @param[in] fcn The name of the function or subroutine in which the 
    !!  warning was encountered.
    !! @param[in] msg The warning message.
    !! @param[in] flag The warning flag.
    subroutine register_warning(err, fcn, msg, flag) &
            bind(C, name = "register_warning")
        ! Arguments
        type(c_ptr), intent(in), value :: err
        character(kind = c_char), intent(in) :: fcn, msg
        integer(c_int), intent(in), value :: flag

        ! Process
        type(errors), pointer :: ferr
        if (.not.c_associated(err)) return
        call c_f_pointer(err, ferr)
        call ferr%report_warning(cstr_2_fstr(fcn), cstr_2_fstr(msg), flag)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes an error log file.
    !!
    !! @param[in] err A pointer to the error handler object.
    !! @param[in] fcn The name of the function or subroutine in which the error
    !!  was encountered.
    !! @param[in] msg The error message.
    !! @param[in] flag The error flag.
    subroutine write_error_log(err, fcn, msg, flag) &
            bind(C, name = "write_error_log")
        ! Arguments
        type(c_ptr), intent(in), value :: err
        character(kind = c_char), intent(in) :: fcn, msg
        integer(c_int), intent(in), value :: flag

        ! Process
        type(errors), pointer :: ferr
        if (.not.c_associated(err)) return
        call c_f_pointer(err, ferr)
        call ferr%log_error(cstr_2_fstr(fcn), cstr_2_fstr(msg), flag)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Tests to see if an error has been encountered.
    !!
    !! @param[in] err A pointer to the error handler object.
    !! @return Returns true if an error has been encountered; else, false.
    function error_occurred(err) result(x) bind(C, name = "error_occurred")
        ! Arguments
        type(c_ptr), intent(in), value :: err
        logical(c_bool) :: x

        ! Process
        type(errors), pointer :: ferr
        x = .false.
        if (.not.c_associated(err)) return
        call c_f_pointer(err, ferr)
        x = ferr%has_error_occurred()
    end function

! ------------------------------------------------------------------------------
    !> @brief Resets the error status flag to false, and the current error flag
    !! to zero.
    !!
    !! @param[in] err The error handler object.
    subroutine reset_error(err) bind(C, name = "reset_error")
        ! Arguments
        type(c_ptr), intent(in), value :: err

        ! Process
        type(errors), pointer :: ferr
        if (.not.c_associated(err)) return
        call c_f_pointer(err, ferr)
        call ferr%reset_error_status()
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Tests to see if a warning has been encountered.
    !!
    !! @param[in] err A pointer to the error handler object.
    !! @return Returns true if a warning has been encountered; else, false.
    function warning_occurred(err) result(x) &
            bind(C, name = "warning_occurred")
        ! Arguments
        type(c_ptr), intent(in), value :: err
        logical(c_bool) :: x

        ! Process
        type(errors), pointer :: ferr
        x = .false.
        if (.not.c_associated(err)) return
        call c_f_pointer(err, ferr)
        x = ferr%has_warning_occurred()
    end function

! ------------------------------------------------------------------------------
    !> @brief Resets the warning status flag to false, and the current warning
    !! flag to zero.
    !!
    !! @param[in] err A pointer to the error handler object.
    subroutine reset_warning(err) bind(C, name = "reset_warning")
        ! Arguments
        type(c_ptr), intent(in), value :: err

        ! Process
        type(errors), pointer :: ferr
        if (.not.c_associated(err)) return
        call c_f_pointer(err, ferr)
        call ferr%reset_warning_status()
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the current error flag.
    !!
    !! @param[in] err A pointer to the error handler object.
    !! @return The current error flag.
    function get_error_code(err) result(x) bind(C, name = "get_error_code")
        ! Arguments
        type(c_ptr), intent(in), value :: err
        integer(c_int) :: x

        ! Process
        type(errors), pointer :: ferr
        x = 0
        if (.not.c_associated(err)) return
        call c_f_pointer(err, ferr)
        x = ferr%get_error_flag()
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the current warning flag.
    !!
    !! @param[in] err A pointer to the error handler object.
    !! @return The current warning flag.
    function get_warning_code(err) result(x) &
            bind(C, name = "get_warning_code")
        ! Arguments
        type(c_ptr), intent(in), value :: err
        integer(c_int) :: x

        ! Process
        type(errors), pointer :: ferr
        x = 0
        if (.not.c_associated(err)) return
        call c_f_pointer(err, ferr)
        x = ferr%get_warning_flag()
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets a logical value determining if the application should be
    !! terminated when an error is encountered.
    !!
    !! @param[in] err A pointer to the error handler object.
    !! @return Returns true if the application should be terminated; else, 
    !!  false.
    function get_exit_behavior(err) result(x) &
            bind(C, name = "get_exit_behavior")
        ! Arguments
        type(c_ptr), intent(in), value :: err
        logical(c_bool) :: x

        ! Process
        type(errors), pointer :: ferr
        if (.not.c_associated(err)) then
            x = .true.
            return
        end if
        call c_f_pointer(err, ferr)
        x = ferr%get_exit_on_error()
    end function

! ------------------------------------------------------------------------------
    !> @brief Sets a logical value determining if the application should be
    !! terminated when an error is encountered.
    !!
    !! @param[in] err A pointer to the error handler object.
    !! @param[x] in Set to true if the application should be terminated when an
    !!  error is reported; else, false.
    subroutine set_exit_behavior(err, x) bind(C, name = "set_exit_behavior")
        ! Arguments
        type(c_ptr), intent(in), value :: err
        logical(c_bool), intent(in) :: x

        ! Process
        type(errors), pointer :: ferr
        if (.not.c_associated(err)) return
        call c_f_pointer(err, ferr)
        call ferr%set_exit_on_error(logical(x))
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

        ! Local Variables
        integer :: i, n

        ! Process
         n = strlen(cstr)
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

end module
