! ferror.f90

!> @mainpage
!!
!! @section intro_sec Introduction
!! FERROR is a library to assist with error handling in Fortran projects.
!!
!! @author Jason Christopherson
!! @version 1.2.4

!> @brief \b ferror
!!
!! @par Purpose
!! Provides a series of error codes and error handling mechanisms.
module ferror
    implicit none
    private

! ------------------------------------------------------------------------------
    !> @brief Defines a type for managing errors and warnings.
    type, public :: errors
        private

        !> A maximum of 256 character error log filename
        character(len = 256) :: m_fname = "error_log.txt"
        !> Found an error
        logical :: m_foundError = .false.
        !> Found a warning
        logical :: m_foundWarning = .false.
        !> The error flag
        integer :: m_errorFlag = 0
        !> The warning flag
        integer :: m_warningFlag = 0
        !> Terminate the application on error
        logical :: m_exitOnError = .true.
        !> Suppress printing of error and warning messages
        logical :: m_suppressPrinting = .false.
    contains
        !> @brief Gets the name of the error log file.
        procedure, public :: get_log_filename => er_get_log_filename
        !> @brief Sets the name of the error log file.
        procedure, public :: set_log_filename => er_set_log_filename
        !> eports an error condition to the user.
        procedure, public :: report_error => er_report_error
        !> @brief Reports a warning message to the user.
        procedure, public :: report_warning => er_report_warning
        !> @brief Writes an error log file.
        procedure, public :: log_error => er_log_error
        !> @brief Tests to see if an error has been encountered.
        procedure, public :: has_error_occurred => er_has_error_occurred
        !> @brief Resets the error status flag to false.
        procedure, public :: reset_error_status => er_reset_error_status
        !> @brief Tests to see if a warning has been encountered.
        procedure, public :: has_warning_occurred => er_has_warning_occurred
        !> @brief Resets the warning status flag to false.
        procedure, public :: reset_warning_status => er_reset_warning_status
        !> @brief Gets the current error flag.
        procedure, public :: get_error_flag => er_get_error_flag
        !> @brief Gets the current warning flag.
        procedure, public :: get_warning_flag => er_get_warning_flag
        !> @brief Gets a logical value determining if the application should be
        !! terminated when an error is encountered.
        procedure, public :: get_exit_on_error => er_get_exit_on_error
        !> @brief Sets a logical value determining if the application should be
        !! terminated when an error is encountered.
        procedure, public :: set_exit_on_error => er_set_exit_on_error
        !> @brief Gets a logical value determining if printing of error and 
        !! warning messages should be suppressed.
        procedure, public :: get_suppress_printing => er_get_suppress_printing
        !> @brief Sets a logical value determining if printing of error and 
        !! warning messages should be suppressed.
        procedure, public :: set_suppress_printing => er_set_suppress_printing
    end type

contains
! ------------------------------------------------------------------------------
    !> @brief Gets the name of the error log file.
    !!
    !! @param[in] this The errors object.
    !! @return The filename.
    pure function er_get_log_filename(this) result(str)
        class(errors), intent(in) :: this
        character(len = :), allocatable :: str
        str = trim(this%m_fname)
    end function

! --------------------
    !> @brief Sets the name of the error log file.
    !!
    !! @param[in,out] this The errors object.
    !! @param[in] str The filename.
    subroutine er_set_log_filename(this, str)
        class(errors), intent(inout) :: this
        character(len = :), allocatable :: str
        integer :: n
        n = min(len(str), 256)
        this%m_fname = ""
        this%m_fname(1:n) = str(1:n)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Reports an error condition to the user.
    !!
    !! @param[in,out] this The errors object.
    !! @param[in] fcn The name of the function or subroutine in which the error
    !!  was encountered.
    !! @param[in] msg The error message.
    !! @param[in] flag The error flag.
    !!
    !! @par Remarks
    !! The default behavior prints an error message, appends the supplied 
    !! information to a log file, and terminates the program.
    subroutine er_report_error(this, fcn, msg, flag)
        ! Arguments
        class(errors), intent(inout) :: this
        character(len = *), intent(in) :: fcn, msg
        integer, intent(in) :: flag

        ! Write the error message to the command line
        if (.not.this%m_suppressPrinting) then
            print *, ""
            print '(A)', "***** ERROR *****"
            print '(A)', "Function: " // fcn
            print '(AI0)', "Error Flag: ", flag
            print '(A)', "Message:"
            print '(A)', msg
            print *, ""
        end if

        ! Update the error found status
        this%m_foundError = .true.
        this%m_errorFlag = flag

        ! Write the error message to a log file
        call this%log_error(fcn, msg, flag)

        ! Exit the program
        if (this%m_exitOnError) call exit(flag)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Reports a warning message to the user.
    !!
    !! @param[in,out] this The errors object.
    !! @param[in] fcn The name of the function or subroutine from which the
    !!  warning was issued.
    !! @param[in] msg The warning message.
    !! @param[in] flag The warning flag.
    !!
    !! @par Remarks
    !! The default behavior prints the warning message, and returns control
    !! back to the calling code.
    subroutine er_report_warning(this, fcn, msg, flag)
        ! Arguments
        class(errors), intent(inout) :: this
        character(len = *), intent(in) :: fcn, msg
        integer, intent(in) :: flag

        ! Write the warning message to the command line
        if (.not.this%m_suppressPrinting) then
            print *, ""
            print '(A)', "***** WARNING *****"
            print '(A)', "Function: " // fcn
            print '(AI0)', "Warning Flag: ", flag
            print '(A)', "Message:"
            print '(A)', msg
            print *, ""
        end if

        ! Update the warning found status
        this%m_foundWarning = .true.
        this%m_warningFlag = flag
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes an error log file.
    !!
    !! @param[in] this The errors object.
    !! @param[in] fcn The name of the function or subroutine in which the error
    !!  was encountered.
    !! @param[in] msg The error message.
    !! @param[in] flag The error flag.
    subroutine er_log_error(this, fcn, msg, flag)
        ! Arguments
        class(errors), intent(in) :: this
        character(len = *), intent(in) :: fcn, msg
        integer, intent(in) :: flag

        ! Local Variables
        integer :: fid, time(3), date(3)

        ! Open the file
        open(newunit = fid, file = this%m_fname, access = "append")

        ! Determine the time
        call itime(time)
        call idate(date)

        ! Write the error information
        write(fid, '(A)') ""
        write(fid, '(A)') "***** ERROR *****"
        write(fid, '(I0AI0AI0AI0AI0AI0)') date(1), "/", date(2), "/", date(3), &
            "; ", time(1), ":", time(2), ":", time(3)
        write(fid, '(A)') "Function: " // fcn
        write(fid, '(AI0)') "Error Flag: ", flag
        write(fid, '(A)') "Message:"
        write(fid, '(A)') msg
        write(fid, '(A)') ""

        ! Close the file
        close(fid)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Tests to see if an error has been encountered.
    !!
    !! @param[in] this The errors object.
    !! @return Returns true if an error has been encountered; else, false.
    pure function er_has_error_occurred(this) result(x)
        class(errors), intent(in) :: this
        logical :: x
        x = this%m_foundError
    end function
    
! ------------------------------------------------------------------------------
    !> @brief Resets the error status flag to false, and the current error flag
    !! to zero.
    !!
    !! @param[in,out] this The errors object.
    subroutine er_reset_error_status(this)
        class(errors), intent(inout) :: this
        this%m_foundError = .false.
        this%m_errorFlag = 0
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Tests to see if a warning has been encountered.
    !!
    !! @param[in] this The errors object.
    !! @return Returns true if a warning has been encountered; else, false.
    pure function er_has_warning_occurred(this) result(x)
        class(errors), intent(in) :: this
        logical :: x
        x = this%m_foundWarning
    end function
    
! ------------------------------------------------------------------------------
    !> @brief Resets the warning status flag to false, and the current warning
    !! flag to zero.
    !!
    !! @param[in,out] this The errors object.
    subroutine er_reset_warning_status(this)
        class(errors), intent(inout) :: this
        this%m_foundWarning = .false.
        this%m_warningFlag = 0
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the current error flag.
    !!
    !! @param[in] this The errors object.
    !! @return The current error flag.
    pure function er_get_error_flag(this) result(x)
        class(errors), intent(in) :: this
        integer :: x
        x = this%m_errorFlag
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the current warning flag.
    !!
    !! @param[in] this The errors object.
    !! @return The current warning flag.
    pure function er_get_warning_flag(this) result(x)
        class(errors), intent(in) :: this
        integer :: x
        x = this%m_warningFlag
    end function
    
! ------------------------------------------------------------------------------
    !> @brief Gets a logical value determining if the application should be
    !! terminated when an error is encountered.
    !!
    !! @param[in] this The errors object.
    !! @return Returns true if the application should be terminated; else, 
    !!  false.
    pure function er_get_exit_on_error(this) result(x)
        class(errors), intent(in) :: this
        logical :: x
        x = this%m_exitOnError
    end function

! ------------------------------------------------------------------------------
    !> @brief Sets a logical value determining if the application should be
    !! terminated when an error is encountered.
    !!
    !! @param[in,out] this The errors object.
    !! @param[in] x Set to true if the application should be terminated when an
    !!  error is reported; else, false.
    subroutine er_set_exit_on_error(this, x)
        class(errors), intent(inout) :: this
        logical, intent(in) :: x
        this%m_exitOnError = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a logical value determining if printing of error and warning
    !! messages should be suppressed.
    !!
    !! @param[in] this The errors object.
    !! @return True if message printing should be suppressed; else, false to 
    !!  allow printing.
    pure function er_get_suppress_printing(this) result(x)
        class(errors), intent(in) :: this
        logical :: x
        x = this%m_suppressPrinting
    end function

! --------------------
    !> @brief Sets a logical value determining if printing of error and warning
    !! messages should be suppressed.
    !!
    !! @param[in,out] this The errors object.
    !! @param[in] x Set to true if message printing should be suppressed; else,
    !!  false to allow printing.
    subroutine er_set_suppress_printing(this, x)
        class(errors), intent(inout) :: this
        logical, intent(in) :: x
        this%m_suppressPrinting = x
    end subroutine

! ------------------------------------------------------------------------------
end module
