cmake_minimum_required(VERSION 2.8)

project(flogger LANGUAGES Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}")

if (EXISTS ${PROJECT_SOURCE_DIR}/lib/container/CMakeLists.txt)
  add_subdirectory(lib/container)
endif ()
if (EXISTS ${PROJECT_SOURCE_DIR}/lib/string/CMakeLists.txt)
  add_subdirectory(lib/string)
endif ()

set(source_files
  src/face_mod.F90
  src/log_mod.F90
  src/flogger.F90
)

add_library(flogger ${source_files})
target_link_libraries(flogger fortran_container fortran_string)

if (RUN_TEST)
  add_executable(test_flogger src/test_flogger.F90)
  target_link_libraries(test_flogger flogger)
endif ()
