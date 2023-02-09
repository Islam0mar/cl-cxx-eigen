cmake_minimum_required(VERSION 2.8.12)
project(ClCxx-Eigen)

set(PROJECT_PATH "${CMAKE_CURRENT_SOURCE_DIR}")

# Compiler Flags
# ==============
include(CheckCXXCompilerFlag)
# -march=native for GCC, Clang and ICC in any version that does support it.

if((NOT CRYPTOPP_CROSS_COMPILE) AND (CMAKE_CXX_COMPILER_ID MATCHES
                                     "Clang|GNU|Intel"))
  check_cxx_compiler_flag("-march=native" COMPILER_OPT_ARCH_NATIVE_SUPPORTED)
  if(COMPILER_OPT_ARCH_NATIVE_SUPPORTED AND NOT CMAKE_CXX_FLAGS MATCHES
                                            "-march=")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=native")
  endif()
endif()

# Liker flags
# =================
if(APPLE)
  set(D_LINER_FLAGS "-undefined error" "-flat_namespace")
else()
  set(D_LINER_FLAGS "-Wl,--no-undefined" "-Wl,--no-allow-shlib-undefined")
endif()

# Dependencies
# ============
find_library(
  ClCxx 1.0.0
  NAMES ClCxx libClCxx.so
  PATHS "~/.local/lib"
  NO_DEFAULT_PATH)
find_package(Eigen3 REQUIRED)
find_package(OpenMP REQUIRED)

# Options
set(CMAKE_VERBOSE_MAKEFILE true)
set(CMAKE_EXPORT_COMPILE_COMMANDS 1)

set(EXECUTABLE_OUTPUT_PATH "${PROJECT_PATH}/bin")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_PATH}/lib")
set(CMAKE_BINARY_DIR "${PROJECT_PATH}/bin")

# Source files
# ============

set(MY_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include)
set(MY_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)

file(GLOB HEADER_GLOB ${MY_INCLUDE_DIR}/*.hpp)

file(GLOB SOURCES_GLOB ${MY_SOURCE_DIR}/*.cpp)

set(MY_HEADERS ${HEADER_GLOB})

set(MY_SOURCES ${SOURCES_GLOB})

# Versioning
# ==========

file(STRINGS "${MY_INCLUDE_DIR}/clcxx_eigen_config.hpp"
     clcxx_eigen_version_defines
     REGEX "#define CLCXX_EIGEN_VERSION_(MAJOR|MINOR|PATCH)")
foreach(ver ${clcxx_eigen_version_defines})
  if(ver MATCHES "#define CLCXX_EIGEN_VERSION_(MAJOR|MINOR|PATCH) +([^ ]+)$")
    set(CLCXX_EIGEN_VERSION_${CMAKE_MATCH_1}
        "${CMAKE_MATCH_2}"
        CACHE INTERNAL "")
  endif()
endforeach()
set(${PROJECT_NAME}_VERSION
    ${CLCXX_EIGEN_VERSION_MAJOR}.${CLCXX_EIGEN_VERSION_MINOR}.${CLCXX_EIGEN_VERSION_PATCH}
)
message(STATUS "${PROJECT_NAME} version: v${${PROJECT_NAME}_VERSION}")

# Output
# ======
add_library(${PROJECT_NAME} SHARED ${MY_SOURCES})

target_include_directories(${PROJECT_NAME} PUBLIC include "~/.local/include/"
                                                  ${EIGEN3_INCLUDE_DIR})

target_compile_options(
  ${PROJECT_NAME} PUBLIC "$<$<COMPILE_LANGUAGE:CXX>:-std=c++17>" "-Wall"
                         "-Wextra" "-O3")

target_link_libraries(${PROJECT_NAME} ${ClCxx} ${D_LINER_FLAGS}
                      OpenMP::OpenMP_CXX)

set_property(TARGET ${PROJECT_NAME} PROPERTY VERSION ${${PROJECT_NAME}_VERSION})
set_property(TARGET ${PROJECT_NAME} PROPERTY SOVERSION ${CLCXX_EIGEN_MAJOR})
