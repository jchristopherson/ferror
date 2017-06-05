! ferror_c_binding.f90

module ferror_c_binding
    use, intrinsic :: iso_c_binding
    use ferror
    implicit none

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
    !>
    subroutine get_error_log_fname(err, n, fname)
        !>
        type(c_ptr), intent(in), value :: err
        character(kind = c_char), intent(out) :: fname(*)

        ! REF: https://gcc.gnu.org/onlinedocs/gfortran/Interoperable-Subroutines-and-Functions.html
    end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module
