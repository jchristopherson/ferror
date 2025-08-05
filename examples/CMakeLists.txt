# Example 1
add_executable(ferror_example_1 ferror_example_1.f90)
target_link_libraries(ferror_example_1 ferror)
target_include_directories(ferror_example_1 PUBLIC ${PROJECT_BINARY_DIR}/include)

# Example 2
add_executable(ferror_example_2 ferror_example_2.f90)
target_link_libraries(ferror_example_2 ferror)
target_include_directories(ferror_example_2 PUBLIC ${PROJECT_BINARY_DIR}/include)

# Example 1 - C API
include_directories(${PROJECT_SOURCE_DIR}/include)
add_executable(ferror_example_1c ferror_example_1c.c)
target_link_libraries(ferror_example_1c ferror)

# Example 2 - C API
add_executable(ferror_example_2c ferror_example_2c.c)
target_link_libraries(ferror_example_2c ferror)

# Shared Library?
if (${BUILD_SHARED_LIBS})
    add_custom_command(
        TARGET ferror_test
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
        $<TARGET_FILE:ferror>
        ${CMAKE_BINARY_DIR}/test
    )
endif()