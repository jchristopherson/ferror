# Define the include directories
include_directories(${PROJECT_SOURCE_DIR}/include)

# Locate the source files
set(c_test_source ferror_test_c.c)

# Build the executable
add_executable(ferror_test_c ${c_test_source})
target_link_libraries(ferror_test_c ferror)

# Force the tests to run as part of the build process
add_custom_command(OUTPUT c_api_test DEPENDS ${c_test_source}
  COMMAND ferror_test_c)
add_custom_target(run_c_api_test ALL DEPENDS c_api_test)