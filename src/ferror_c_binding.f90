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

! ------------------------------------------------------------------------------

! ******************************************************************************
! PRIVATE HELPER ROUTINES
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
