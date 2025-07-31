include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

add_executable(ferror_test ferror_test.f90)
link_library(ferror_test ${PROJECT_NAME} ${PROJECT_INCLUDE_DIR})
add_test(
    NAME ferror_test
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:ferror_test>
)

if (${BUILD_FERROR_C_API})
    include_directories(
        ${CMAKE_CURRENT_SOURCE_DIR}
        $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
    )
    add_executable(ferror_test_c C/ferror_test_c.c)
    link_library(ferror_test_c ${PROJECT_NAME} ${PROJECT_INCLUDE_DIR})
    add_test(
        NAME ferror_test_c
        WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
        COMMAND $<TARGET_FILE:ferror_test_c>
    )
endif()

set(FERROR_SHARED_LIB_NAME ${CMAKE_SHARED_LIBRARY_PREFIX})
string(APPEND FERROR_SHARED_LIB_NAME ${PROJECT_NAME})
string(APPEND FERROR_SHARED_LIB_NAME ${CMAKE_SHARED_LIBRARY_SUFFIX})
add_custom_command(
    TARGET ferror_test
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy
    "${CMAKE_BINARY_DIR}/${FERROR_SHARED_LIB_NAME}"
    "${CMAKE_BINARY_DIR}/test"
)