! ferror_c_binding.f90

!> @brief \b ferror
!!
!! @par Purpose
!! Provides C bindings to the ferror library.
module ferror_c_binding
    use :: iso_c_binding
    use ferror
    implicit none
    private
    public :: error_handler
    public :: get_error_handler
    public :: alloc_error_handler
    public :: free_error_handler
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
    type, bind(C) :: error_handler
        type(c_ptr) :: ptr
        !! A pointer to the errors object.
        integer(c_int) :: object_size
        !! The size of the errors object, in bytes.
    end type

! ------------------------------------------------------------------------------
    interface
        subroutine c_error_callback(ptr)
            use, intrinsic :: iso_c_binding
            type(c_ptr), intent(in), value :: ptr
        end subroutine
    end interface

! ------------------------------------------------------------------------------
    interface
        !> @brief Initializes a new error handler object.
        !!
        !! @param[in] obj The error_handler object to allocate.
        module subroutine alloc_error_handler(obj) &
            bind(C, name = "alloc_error_handler")
            type(error_handler), intent(inout) :: obj
        end subroutine

        !> @brief Frees resources held by the error_handler object.
        !!
        !! @param[in,out] obj The error_handler object.
        module subroutine free_error_handler(obj) &
            bind(C, name = "free_error_handler")
            type(error_handler), intent(inout), target :: obj
        end subroutine

        !> @brief Gets the @ref errors object stored within the 
        !! @ref error_handler object.
        module subroutine get_error_handler(obj, eobj)
            type(error_handler), intent(in), target :: obj
            type(errors), intent(out), pointer :: eobj
        end subroutine

        !> @brief Gets the name of the error log file.
        !!
        !! @param[in] err The error_handler object.
        !! @param[out] fname A character buffer where the filename will be 
        !!  written.  It is recommended that this be in the neighborhood of 
        !!  256 elements.
        !! @param[in,out] nfname On input, the actual size of the buffer.  Be 
        !!  sure to leave room for the null terminator character.  On output, 
        !!  the actual number of characters written to @p fname (not including 
        !!  the null character).
        module subroutine get_log_filename(err, fname, nfname) &
            bind(C, name = "get_log_filename")
            type(error_handler), intent(in) :: err
            character(kind = c_char), intent(out) :: fname(*)
            integer(c_int), intent(inout) :: nfname
        end subroutine

        !> @brief Sets the error log filename.
        !!
        !! @param[in,out] err The error_handler object.
        !! @param[in] fname A null-terminated string containing the filename.
        module subroutine set_log_filename(err, fname) &
            bind(C, name = "set_log_filename")
            type(error_handler), intent(inout) :: err
            character(kind = c_char), intent(in) :: fname(*)
        end subroutine

        !> @brief Reports an error condition to the user.
        !!
        !! @param[in,out] err A pointer to the error handler object.
        !! @param[in] fcn The name of the function or subroutine in which the 
        !!  error was encountered.
        !! @param[in] msg The error message.
        !! @param[in] flag The error flag.
        module subroutine report_error(err, fcn, msg, flag) &
            bind(C, name = "report_error")
            type(error_handler), intent(inout) :: err
            character(kind = c_char), intent(in) :: fcn, msg
            integer(c_int), intent(in), value :: flag
        end subroutine

        !> @brief Reports a warning condition to the user.
        !!
        !! @param[in,out] err The error_handler object.
        !! @param[in] fcn The name of the function or subroutine in which the
        !!  warning was encountered.
        !! @param[in] msg The warning message.
        !! @param[in] flag The warning flag.
        module subroutine report_warning(err, fcn, msg, flag) &
            bind(C, name = "report_warning")
            type(error_handler), intent(inout) :: err
            character(kind = c_char), intent(in) :: fcn, msg
            integer(c_int), intent(in), value :: flag
        end subroutine

        !> @brief Writes an error log file.
        !!
        !! @param[in] err The error_handler object.
        !! @param[in] fcn The name of the function or subroutine in which the 
        !!  error was encountered.
        !! @param[in] msg The error message.
        !! @param[in] flag The error flag.
        module subroutine log_error(err, fcn, msg, flag) &
            bind(C, name = "log_error")
            type(error_handler), intent(in) :: err
            character(kind = c_char), intent(in) :: fcn, msg
            integer(c_int), intent(in), value :: flag
        end subroutine

        !> @brief Tests to see if an error has been encountered.
        !!
        !! @param[in] err A pointer to the error handler object.
        !! @return Returns true if an error has been encountered; else, false.
        module function has_error_occurred(err) result(x) &
            bind(C, name = "has_error_occurred")
            type(error_handler), intent(in) :: err
            logical(c_bool) :: x
        end function

        !> @brief Resets the error status flag to false, and the current error flag
        !! to zero.
        !!
        !! @param[in,out] err The error_handler object.
        module subroutine reset_error_status(err) &
            bind(C, name = "reset_error_status")
            type(error_handler), intent(inout) :: err
        end subroutine

        !> @brief Tests to see if a warning has been encountered.
        !!
        !! @param[in] err The error_handler object.
        !! @return Returns true if a warning has been encountered; else, false.
        module function has_warning_occurred(err) result(x) &
            bind(C, name = "has_warning_occurred")
            type(error_handler), intent(in) :: err
            logical(c_bool) :: x
        end function

        !> @brief Resets the warning status flag to false, and the current 
        !! warning flag to zero.
        !!
        !! @param[in,out] err The error_handler object.
        module subroutine reset_warning_status(err) &
            bind(C, name = "reset_warning_status")
            type(error_handler), intent(inout) :: err
        end subroutine

        !> @brief Gets the current error flag.
        !!
        !! @param[in] err The error_handler object.
        !! @return The current error flag.
        module function get_error_flag(err) result(x) &
            bind(C, name = "get_error_flag")
            type(error_handler), intent(in) :: err
            integer(c_int) :: x
        end function

        !> @brief Gets the current warning flag.
        !!
        !! @param[in] err The error_handler object.
        !! @return The current warning flag.
        module function get_warning_flag(err) result(x) &
            bind(C, name = "get_warning_flag")
            type(error_handler), intent(in) :: err
            integer(c_int) :: x
        end function

        !> @brief Gets a logical value determining if the application should be
        !! terminated when an error is encountered.
        !!
        !! @param[in] err The error_handler object.
        !! @return Returns true if the application should be terminated; else,
        !!  false.
        module function get_exit_on_error(err) result(x) &
            bind(C, name = "get_exit_on_error")
            type(error_handler), intent(in) :: err
            logical(c_bool) :: x
        end function

        !> @brief Sets a logical value determining if the application should be
        !! terminated when an error is encountered.
        !!
        !! @param[in,out] err The error_handler object.
        !! @param[in] x Set to true if the application should be terminated when 
        !!  an error is reported; else, false.
        module subroutine set_exit_on_error(err, x) &
            bind(C, name = "set_exit_on_error")
            type(error_handler), intent(inout) :: err
            logical(c_bool), intent(in), value :: x
        end subroutine

        !> @brief Gets a logical value determining if printing of error and 
        !! warning messages should be suppressed.
        !!
        !! @param[in] err The error_handler object.
        !! @return True if message printing should be suppressed; else, false to 
        !!  allow printing.
        module function get_suppress_printing(err) result(x) &
            bind(C, name = "get_suppress_printing")
            type(error_handler), intent(in) :: err
            logical(c_bool) :: x
        end function

        !> @brief Sets a logical value determining if printing of error and 
        !! warning messages should be suppressed.
        !!
        !! @param[in,out] err The error_handler object.
        !! @param[in] x Set to true if message printing should be suppressed; 
        !!  else, false to allow printing.
        module subroutine set_suppress_printing(err, x) &
            bind(C, name = "set_suppress_printing")
            type(error_handler), intent(inout) :: err
            logical(c_bool), intent(in), value :: x
        end subroutine

        !> @brief Gets the current error message.
        !!
        !! @param[in] err The error_handler object.
        !! @param[out] mst A character buffer where the message will be written.
        !! @param[in,out] nmsg On input, the actual size of the buffer.  On 
        !!  output, the actual number of characters written to @p msg (not 
        !!  including the null character).
        module subroutine get_error_message(err, msg, nmsg) &
            bind(C, name = "get_error_message")
            type(error_handler), intent(in) :: err
            character(kind = c_char), intent(out) :: msg(*)
            integer(c_int), intent(inout) :: nmsg
        end subroutine

        !> @brief Gets the current warning message.
        !!
        !! @param[in] err The error_handler object.
        !! @param[out] mst A character buffer where the message will be written.
        !! @param[in,out] nmsg On input, the actual size of the buffer.  On 
        !!  output, the actual number of characters written to @p msg (not 
        !!  including the null character).
        module subroutine get_warning_message(err, msg, nmsg) &
            bind(C, name = "get_warning_message")
            type(error_handler), intent(in) :: err
            character(kind = c_char), intent(out) :: msg(*)
            integer(c_int), intent(inout) :: nmsg
        end subroutine

        !> @brief Gets the name of the function or subroutine that issued the 
        !! last error message.
        !!
        !! @param[in] err The error_handler object.
        !! @param[out] fname A character buffer where the name will be written.
        !! @param[in,out] nfname On input, the actual size of the buffer.  On 
        !!  output, the actual number of characters written to @p fname (not
        !!  including the null character).
        module subroutine get_error_fcn_name(err, fname, nfname) &
            bind(C, name = "get_error_fcn_name")
            type(error_handler), intent(in) :: err
            character(kind = c_char), intent(out) :: fname(*)
            integer(c_int), intent(inout) :: nfname
        end subroutine

        !> @brief Gets the name of the function or subroutine that issued the 
        !! last warning message.
        !!
        !! @param[in] err The error_handler object.
        !! @param[out] fname A character buffer where the name will be written.
        !! @param[in,out] nfname On input, the actual size of the buffer.  On 
        !!  output, the actual number of characters written to @p fname (not
        !!  including the null character).
        module subroutine get_warning_fcn_name(err, fname, nfname) &
            bind(C, name = "get_warning_fcn_name")
            type(error_handler), intent(in) :: err
            character(kind = c_char), intent(out) :: fname(*)
            integer(c_int), intent(inout) :: nfname
        end subroutine

        !> @brief Reports an error condition to the user, and executes a 
        !! callback routine.
        !!
        !! @param[in,out] err A pointer to the error handler object.
        !! @param[in] fcn The name of the function or subroutine in which the 
        !!  error was encountered.
        !! @param[in] msg The error message.
        !! @param[in] flag The error flag.
        !! @param[in] cback A pointer to the callback function.
        !! @param[in] args A pointer to an object to pass to the callback 
        !!  function.
        module subroutine report_error_with_callback(err, fcn, msg, flag, &
            cback, args) bind(C, name = "report_error_with_callback")
            type(error_handler), intent(inout) :: err
            character(kind = c_char), intent(in) :: fcn, msg
            integer(c_int), intent(in), value :: flag
            procedure(error_callback), pointer :: ffcn
            type(c_funptr), intent(in), value :: cback
            type(c_ptr), intent(in), value :: args
        end subroutine
    end interface

! ------------------------------------------------------------------------------
end module
