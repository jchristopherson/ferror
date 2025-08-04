add_executable(ferror_test ferror_test.f90)
target_link_libraries(ferror_test ferror)
add_test(
    NAME ferror_test
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:ferror_test>
)

if (${BUILD_FERROR_C_API})
    add_executable(ferror_test_c C/ferror_test_c.c)
    target_link_libraries(ferror_test_c ferror)
    target_include_directories(ferror_test_c PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/include)
    add_test(
        NAME ferror_test_c
        WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
        COMMAND $<TARGET_FILE:ferror_test_c>
    )
endif()

if (${BUILD_SHARED_LIBS})
    add_custom_command(
        TARGET ferror_test
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
        $<TARGET_FILE:ferror>
        ${CMAKE_BINARY_DIR}/test
    )
endif()
