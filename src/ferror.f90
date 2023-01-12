! ferror.f90

!> @mainpage
!!
!! @section intro_sec Introduction
!! FERROR is a library to assist with error handling in Fortran projects.  The 
!! error handling capabilities also have been extended to be called from C
!! thereby providing both an error handling mechanism for C projects as well as
!! allowing C interop with Fortran projects that use this library to handle
!! errors.
!!
!! @par Example
!! The following piece of code offers a simple introduction to the use of this
!! library.
!! @code{.f90}
!! program example
!!     use ferror
!!     use, intrinsic :: iso_fortran_env, only : int32
!!     implicit none
!! 
!!     ! Variables
!!     type(errors) :: err_mgr
!! 
!!     ! Ensure the error reporting doesn't terminate the application.  The default
!!     ! behavior terminates the application - optional.
!!     call err_mgr%set_exit_on_error(.false.)
!! 
!!     ! Don't print the error message to the command line.  The default behavior
!!     ! prints the error information to the command line - optional.
!!     call err_mgr%set_suppress_printing(.true.)
!! 
!!     ! Call the routine that causes the error
!!     call causes_error(err_mgr)
!! 
!!     ! Print the error information
!!     print '(A)', "An error occurred in the following subroutine: " // &
!!         err_mgr%get_error_fcn_name()
!!     print '(A)', "The error message is: " // err_mgr%get_error_message()
!!     print '(AI0)', "The error code is: ", err_mgr%get_error_flag()
!! contains
!! 
!! ! The purpose of this subroutine is to simply trigger an error condition.
!! subroutine causes_error(err)
!!     ! Arguments
!!     class(errors), intent(inout) :: err
!! 
!!     ! Define an error flag
!!     integer(int32), parameter :: error_flag = 200
!! 
!!     ! Trigger the error condition
!!     call err%report_error(&
!!         "causes_error", &                   ! The subroutine or function name
!!         "This is a test error message.", &  ! The error message.
!!         error_flag)                         ! The error flag
!! end subroutine
!! 
!! end program
!! @endcode
!!
!! @par
!! The above program produces the following output.
!! @code{.txt}
!! An error occurred in the following subroutine: causes_error
!! The error message is: This is a test error message.
!! The error code is: 200
!! @endcode
!!
!! @par
!! The above program also creates a log file.  The log file is titled 
!! error_log.txt by default, but can be named whatever by the user.  The 
!! contents of the file written from the above program are as follows.  
!! @code{.txt}
!! ***** ERROR *****
!! 1/2/2018; 16:49:40
!! Function: causes_error
!! Error Flag: 200
!! Message:
!! This is a test error message.
!! @endcode
!!
!! @par
!! If additional errors are encountered, the information is simply appended to
!! the end of the file.

! ------------------------------------------------------------------------------

!> @brief \b ferror
!!
!! @par Purpose
!! Provides a series of error codes and error handling mechanisms.
module ferror
    use, intrinsic :: iso_fortran_env, only : int32
    implicit none
    private
    public :: errors
    public :: error_callback

!> @brief Defines a type for managing errors and warnings.
    type :: errors
        character(len = 256), private :: m_fname = "error_log.txt"
        logical, private :: m_foundError = .false.
        logical, private :: m_foundWarning = .false.
        integer(int32), private :: m_errorFlag = 0
        integer(int32), private :: m_warningFlag = 0
        logical, private :: m_exitOnError = .true.
        logical, private :: m_suppressPrinting = .false.
        character(len = :), private, allocatable :: m_errorMessage
        character(len = :), private, allocatable :: m_warningMessage
        character(len = :), private, allocatable :: m_eFunName
        character(len = :), private, allocatable :: m_wFunName
        procedure(error_callback), private, pointer, pass :: m_errCleanUp => null()
    contains
        !> Gets the name of the error log file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function get_log_filename(class(errors) this, )
        !! @endcode
        !!
        !! @param[in] this The @ref errors object.
        !! @return The filename.
        procedure, public :: get_log_filename => er_get_log_filename

        !> Sets the name of the error log file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_log_filename(class(errors) this, character(len = *) str)
        !! @endcode
        !!
        !! @param[in,out] this The @ref errors object.
        !! @param[in] str The filename.
        procedure, public :: set_log_filename => er_set_log_filename

        !> Reports an error condition to the user.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine report_error(class(errors) this, character(len = *) msg, integer(int32) flag, optional class(*) obj)
        !! @endcode
        !! 
        !!  @param[in,out] this The @ref errors object.
        !!  @param[in] fcn The name of the function or subroutine in which the error
        !!   was encountered.
        !!  @param[in] msg The error message.
        !!  @param[in] flag The error flag.
        !!  @param[in,out] obj An optional unlimited polymorphic object that can be
        !!   passed to provide information to the clean-up routine.
        !! 
        !!  @par Remarks
        !!  The default behavior prints an error message, appends the supplied 
        !!  information to a log file, and terminates the program.
        procedure, public :: report_error => er_report_error

        !> Reports a warning message to the user.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine report_warning(class(errors) this, character(len = *) msg, integer(int32) flag)
        !! @endcode
        !!
        !!  @param[in,out] this The @ref errors object.
        !!  @param[in] fcn The name of the function or subroutine from which the
        !!   warning was issued.
        !!  @param[in] msg The warning message.
        !!  @param[in] flag The warning flag.
        !! 
        !!  @par Remarks
        !!  The default behavior prints the warning message, and returns control
        !!  back to the calling code.
        procedure, public :: report_warning => er_report_warning
        
        !> brief Writes an error log file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine log_error(class(errors) this, character(len = *) fcn, character(len = *) msg, integer(int32) flag)
        !! @endcode
        !! 
        !!  @param[in] this The @ref errors object.
        !!  @param[in] fcn The name of the function or subroutine in which the error
        !!   was encountered.
        !!  @param[in] msg The error message.
        !!  @param[in] flag The error flag.
        procedure, public :: log_error => er_log_error
        
        !> Tests to see if an error has been encountered.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical function has_error_occurred(class(errors) this)
        !! @endcode
        !! 
        !!  @param[in] this The @ref errors object.
        !!  @return Returns true if an error has been encountered; else, false.
        procedure, public :: has_error_occurred => er_has_error_occurred
        
        !> Resets the error status flag to false, and the current error flag
        !!  to zero.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine reset_error_status(class(errors) this)
        !! @endcode
        !! 
        !!  @param[in,out] this The @ref errors object.
        procedure, public :: reset_error_status => er_reset_error_status
        
        !> Tests to see if a warning has been encountered.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical function has_warning_occurred(class(errors) this)
        !! @endcode
        !! 
        !!  @param[in] this The @ref errors object.
        !!  @return Returns true if a warning has been encountered; else, false.
        procedure, public :: has_warning_occurred => er_has_warning_occurred
        
        !> Resets the warning status flag to false, and the current warning
        !!  flag to zero.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine reset_warning_status(class(errors) this)
        !! @endcode
        !! 
        !!  @param[in,out] this The @ref errors object.
        procedure, public :: reset_warning_status => er_reset_warning_status

        !> Gets the current error flag.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function get_error_flag(class(errors) this)
        !! @endcode
        !! 
        !!  @param[in] this The @ref errors object.
        !!  @return The current error flag.
        procedure, public :: get_error_flag => er_get_error_flag
        
        !> Gets the current warning flag.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function get_warning_flag(class(errors) this)
        !! @endcode
        !! 
        !!  @param[in] this The @ref errors object.
        !!  @return The current warning flag.
        procedure, public :: get_warning_flag => er_get_warning_flag
        
        !> Gets a logical value determining if the application should be
        !! terminated when an error is encountered.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical function get_exit_on_error(class(errors) this)
        !! @endcode
        !! 
        !!  @param[in] this The @ref errors object.
        !!  @return Returns true if the application should be terminated; else, 
        !!   false.
        procedure, public :: get_exit_on_error => er_get_exit_on_error
        
        !> Sets a logical value determining if the application should be
        !!  terminated when an error is encountered.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_exit_on_error(class(errors) this, logical x)
        !! @endcode
        !! 
        !!  @param[in,out] this The @ref errors object.
        !!  @param[in] x Set to true if the application should be terminated when an
        !!   error is reported; else, false.
        procedure, public :: set_exit_on_error => er_set_exit_on_error
        
        !> Gets a logical value determining if printing of error and warning
        !! messages should be suppressed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical function get_suppress_printing(class(errors) err)
        !! @endcode
        !! 
        !!  @param[in] this The @ref errors object.
        !!  @return True if message printing should be suppressed; else, false to 
        !!   allow printing.
        procedure, public :: get_suppress_printing => er_get_suppress_printing
        
        !> Sets a logical value determining if printing of error and warning
        !!  messages should be suppressed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_suppress_printing(class(errors) this, logical x)
        !! @endcode
        !! 
        !!  @param[in,out] this The @ref errors object.
        !!  @param[in] x Set to true if message printing should be suppressed; else,
        !!   false to allow printing.
        procedure, public :: set_suppress_printing => er_set_suppress_printing
        
        !> Gets the current error message.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function get_error_message(class(errors) this)
        !! @endcode
        !! 
        !!  @param[in] this The @ref errors object.
        !!  @return The error message.
        procedure, public :: get_error_message => er_get_err_msg
        
        !> Gets the current warning message.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function get_warning_message(class(errors) this)
        !! @endcode
        !! 
        !!  @param[in] this The @ref errors object.
        !!  @return The warning message.
        procedure, public :: get_warning_message => er_get_warning_msg
        
        !> Gets the name of the routine that initiated the error.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function get_error_fcn_name(class(errors) this)
        !! @endcode
        !! 
        !!  @param[in] this The @ref errors object.
        !!  @return The routine or function name.
        procedure, public :: get_error_fcn_name => er_get_err_fcn
        
        !> Gets the name of the routine that initiated the warning.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function get_warning_fcn_name(class(errors) this)
        !! @endcode
        !! 
        !!  @param[in] this The @ref errors object.
        !!  @return The routine or function name.
        procedure, public :: get_warning_fcn_name => er_get_warning_fcn
        
        !> Gets the routine to call when an error has been logged.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine get_clean_up_routine(class(errors) this, procedure(error_callback) pointer ptr)
        !! @endcode
        !! 
        !!  @param[in] this The @ref errors object.
        !!  @param[out] ptr A pointer to the @ref error_callback routine.
        procedure, public :: get_clean_up_routine => er_get_err_fcn_ptr
        
        !> Sets the routine to call when an error has been logged.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_clean_up_routine(class(errors) this, procedure(error_callback) pointer ptr)
        !! @endcode
        !! 
        !!  @param[in,out] this The @ref errors object.
        !!  @param[in] ptr A pointer to the @ref error_callback routine.
        procedure, public :: set_clean_up_routine => er_set_err_fcn_ptr
        
    end type

    interface
        !> Defines the signature of a routine that can be used to clean up
        !! after an error condition is encountered.
        subroutine error_callback(err, obj)
            import errors
            !> The errors-based object managing the error handling.
            class(errors), intent(in) :: err
            !> An unlimited polymorphic object that can be passed to provide
            !! information to the clean-up routine.
            class(*), intent(inout) :: obj
        end subroutine
    end interface

    ! ferror_implementation.f90
    interface
        pure module function er_get_log_filename(this) result(str)
            class(errors), intent(in) :: this
            character(len = :), allocatable :: str
        end function

        module subroutine er_set_log_filename(this, str)
            class(errors), intent(inout) :: this
            character(len = *), intent(in) :: str
            integer(int32) :: n
        end subroutine

        module subroutine er_report_error(this, fcn, msg, flag, obj)
            class(errors), intent(inout) :: this
            character(len = *), intent(in) :: fcn, msg
            integer(int32), intent(in) :: flag
            class(*), intent(inout), optional :: obj
        end subroutine

        module subroutine er_report_warning(this, fcn, msg, flag)
            class(errors), intent(inout) :: this
            character(len = *), intent(in) :: fcn, msg
            integer(int32), intent(in) :: flag
        end subroutine

        module subroutine er_log_error(this, fcn, msg, flag)
            class(errors), intent(in) :: this
            character(len = *), intent(in) :: fcn, msg
            integer(int32), intent(in) :: flag
        end subroutine

        pure module function er_has_error_occurred(this) result(x)
            class(errors), intent(in) :: this
            logical :: x
        end function

        module subroutine er_reset_error_status(this)
            class(errors), intent(inout) :: this
        end subroutine

        pure module function er_has_warning_occurred(this) result(x)
            class(errors), intent(in) :: this
            logical :: x
        end function

        module subroutine er_reset_warning_status(this)
            class(errors), intent(inout) :: this
        end subroutine

        pure module function er_get_error_flag(this) result(x)
            class(errors), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function er_get_warning_flag(this) result(x)
            class(errors), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function er_get_exit_on_error(this) result(x)
            class(errors), intent(in) :: this
            logical :: x
        end function

        module subroutine er_set_exit_on_error(this, x)
            class(errors), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function er_get_suppress_printing(this) result(x)
            class(errors), intent(in) :: this
            logical :: x
        end function

        module subroutine er_set_suppress_printing(this, x)
            class(errors), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module function er_get_err_msg(this) result(msg)
            class(errors), intent(in) :: this
            character(len = :), allocatable :: msg
            integer(int32) :: n
        end function

        module function er_get_warning_msg(this) result(msg)
            class(errors), intent(in) :: this
            character(len = :), allocatable :: msg
            integer(int32) :: n
        end function

        module function er_get_err_fcn(this) result(fcn)
            class(errors), intent(in) :: this
            character(len = :), allocatable :: fcn
            integer(int32) :: n
        end function

        module function er_get_warning_fcn(this) result(fcn)
            class(errors), intent(in) :: this
            character(len = :), allocatable :: fcn
            integer(int32) :: n
        end function

        module subroutine er_get_err_fcn_ptr(this, ptr)
            class(errors), intent(in) :: this
            procedure(error_callback), intent(out), pointer :: ptr
        end subroutine

        module subroutine er_set_err_fcn_ptr(this, ptr)
            class(errors), intent(inout) :: this
            procedure(error_callback), intent(in), pointer :: ptr
        end subroutine
    end interface

! ------------------------------------------------------------------------------
end module
