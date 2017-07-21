cmake_minimum_required(VERSION 2.8)

enable_language(C CXX Fortran) 
project(writer)

 
find_package(VTK REQUIRED)
include(${VTK_USE_FILE})

find_package(MPI REQUIRED)
include_directories(${MPI_CXX_INCLUDE_PATH})

add_library(writer STATIC writer.cxx writer.F90)

target_link_libraries(writer ${VTK_LIBRARIES} ${MPI_CXX_LIBRARIES})
