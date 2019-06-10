cmake_minimum_required(VERSION 2.8)

project(flogger LANGUAGES Fortran)

if (EXISTS ${PROJECT_SOURCE_DIR}/lib/container/CMakeLists.txt)
  add_subdirectory(lib/container)
endif ()

set(source_files
  src/face_mod.F90
  src/string_mod.F90
  src/log_mod.F90
  src/flogger.F90
)

add_library(flogger ${source_files})
target_link_libraries(flogger fortran_container)

if (RUN_TEST)
  add_executable(test_flogger src/test_flogger.F90)
  target_link_libraries(test_flogger flogger)
endif ()
