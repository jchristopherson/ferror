! ferror.f90

!> @brief \b ferror
!!
!! @par Purpose
!! Provides a series of error codes and error handling mechanisms.
module ferror
    use, intrinsic :: iso_fortran_env
    implicit none
    private

! ------------------------------------------------------------------------------
    !> @brief Defines a type for managing errors and warnings.
    type, public :: errors
        private

        !> A maximum of 256 character error log filename
        character(len = 256) :: m_fname = "error_log.txt"
    contains
        !> @brief Gets the name of the error log file.
        procedure, public :: get_log_filename
        !> @brief Sets the name of the error log file.
        procedure, public :: set_log_filename
        !> eports an error condition to the user.
        procedure, public :: report_error
        !> @brief Reports a warning message to the user.
        procedure, public :: report_warning
        !> @brief Writes an error log file.
        procedure, public :: log_error
    end type

contains
! ------------------------------------------------------------------------------
    !> @brief Gets the name of the error log file.
    !!
    !! @param[in] this The errors object.
    !! @return The filename.
    pure function get_log_filename(this) result(str)
        class(errors), intent(in) :: this
        character(len = :), allocatable :: str
        str = trim(this%m_fname)
    end function

! --------------------
    !> @brief Sets the name of the error log file.
    !!
    !! @param[in,out] this The errors object.
    !! @param[in] str The filename.
    subroutine set_log_filename(this, str)
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
    !! @param[in] this The errors object.
    !! @param[in] fcn The name of the function or subroutine in which the error
    !!  was encountered.
    !! @param[in] msg The error message.
    !! @param[in] flag The error flag.
    subroutine report_error(this, fcn, msg, flag)
        ! Arguments
        class(errors), intent(in) :: this
        character(len = *), intent(in) :: fcn, msg
        integer, intent(in) :: flag

        ! Write the error message to the command line
        print *, ""
        print '(A)', "***** ERROR *****"
        print '(A)', "Function: " // fcn
        print '(AI0)', "Error Flag: ", flag
        print '(A)', "Message:"
        print '(A)', msg
        print *, ""

        ! Write the error message to a log file
        call this%log_error(fcn, msg, flag)

        ! Exit the program
        call exit(flag)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Reports a warning message to the user.
    !!
    !! @param[in] this The errors object.
    !! @param[in] fcn The name of the function or subroutine from which the
    !!  warning was issued.
    !! @param[in] msg The warning message.
    subroutine report_warning(this, fcn, msg)
        ! Arguments
        class(errors), intent(in) :: this
        character(len = *), intent(in) :: fcn, msg

        ! Write the warning message to the command line
        print *, ""
        print '(A)', "***** WARNING *****"
        print '(A)', "Function: " // fcn
        print '(A)', "Message:"
        print '(A)', msg
        print *, ""
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes an error log file.
    !!
    !! @param[in] this The errors object.
    !! @param[in] fcn The name of the function or subroutine in which the error
    !!  was encountered.
    !! @param[in] msg The error message.
    !! @param[in] flag The error flag.
    subroutine log_error(this, fcn, msg, flag)
        ! Arguments
        class(errors), intent(in) :: this
        character(len = *), intent(in) :: fcn, msg
        integer, intent(in) :: flag

        ! Local Variables
        integer :: fid

        ! Open the file
        open(newunit = fid, file = this%m_fname)

        ! Write the error information
        write(fid, '(A)') ""
        write(fid, '(A)') "***** ERROR *****"
        write(fid, '(A)') "Function: " // fcn
        write(fid, '(AI0)') "Error Flag: ", flag
        write(fid, '(A)') "Message:"
        write(fid, '(A)') msg
        write(fid, '(A)') ""

        ! Close the file
        close(fid)
    end subroutine

! ------------------------------------------------------------------------------
end module
