! ferror_test.f90

program main
    use ferror
    implicit none

    ! Local Variables
    logical :: test_result, overall
    overall = .true.

    ! Tests
    test_result = test_log_file_get_set()
    if (.not.test_result) overall = .false.

    if (overall) then
        print '(A)', "FERROR TEST STATUS: PASS"
    else
        print '(A)', "FERROR TEST STATUS: FAIL"
    end if

contains
! ------------------------------------------------------------------------------
    function test_log_file_get_set() result(rst)
        ! Local Variables
        logical :: rst
        type(errors) :: obj
        character(len = *), parameter :: fname = "test_filename.txt"
        character(len = :), allocatable :: check
        
        ! Initialization
        rst = .true.

        ! See if the get and set functions work appropriately
        call obj%set_log_filename(fname)
        check = obj%get_log_filename()
        if (check /= fname) then
            rst = .false.
            print '(A)', "Expected a filename of: " // fname // &
                ", but found a filename of: " // check // "."
        end if
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end program main