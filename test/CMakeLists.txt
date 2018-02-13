# ------------------------------------------------------------------------------
# FORTRAN API TESTS
# ------------------------------------------------------------------------------
# Locate the source files
set(f_test_source ferror_test.f90)

# Build the executable
add_executable(ferror_test ${f_test_source})
target_link_libraries(ferror_test ferror)

# Force the tests to run as part of the build process
add_custom_command(OUTPUT ferror_api_test DEPENDS ${f_test_source}
  COMMAND ferror_test)
add_custom_target(run_ferror_tests ALL DEPENDS ferror_api_test)

# ------------------------------------------------------------------------------
# C API TESTS
# ------------------------------------------------------------------------------
# Locate the source files
set(c_test_source ferror_test_c.c)

# Build the executable
add_executable(ferror_test_c ${c_test_source})
target_link_libraries(ferror_test_c ferror)

# Force the tests to run as part of the build process
add_custom_command(OUTPUT c_api_test DEPENDS ${c_test_source}
  COMMAND ferror_test_c)
add_custom_target(run_c_api_test ALL DEPENDS c_api_test)