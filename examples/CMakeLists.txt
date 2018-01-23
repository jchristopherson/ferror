# Example 1
set(example_1_sources ferror_example_1.f90)
add_executable(ferror_example_1 ${example_1_sources})
target_link_libraries(ferror_example_1 ferror)

# Example 1 - C API
include_directories(${PROJECT_SOURCE_DIR}/include)
set(example_1_sources_c examples/ferror_example_1c.c)
add_executable(ferror_example_1c ${example_1_sources_c})
target_link_libraries(ferror_example_1c ferror)