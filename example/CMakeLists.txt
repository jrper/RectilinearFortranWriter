cmake_minimum_required(VERSION 2.8)

enable_language(C CXX Fortran) 
project(example)

 
find_package(VTK REQUIRED)
include(${VTK_USE_FILE})
find_package(MPI REQUIRED)
link_directories(${CMAKE_SOURCE_DIR}/../writer)
include_directories(${CMAKE_SOURCE_DIR}/../writer  ${MPI_Fortran_INCLUDE_PATH})

add_executable(example example.F90)

target_link_libraries(example ${VTK_LIBRARIES} writer ${MPI_Fortran_LIBRARIES})
set_target_properties(example PROPERTIES LINKER_LANGUAGE CXX)
