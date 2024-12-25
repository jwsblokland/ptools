# Ptools library
set(PTOOLS_SRCDIR ${CMAKE_CURRENT_SOURCE_DIR})
set(PTOOLS_BINDIR ${CMAKE_CURRENT_BINARY_DIR})

set(PTOOLS_CONFIG_IN  ${CMAKE_CURRENT_SOURCE_DIR}/mptools_version.f90.in)
set(PTOOLS_CONFIG_OUT ${CMAKE_CURRENT_BINARY_DIR}/mptools_version.f90)
configure_file(${PTOOLS_CONFIG_IN} ${PTOOLS_CONFIG_OUT} @ONLY)

set(PTOOLSLIB_SRC      ${PTOOLS_SRCDIR}/mptools_parameters.f90
                       ${PTOOLS_SRCDIR}/mptools_system.f90
	               ${PTOOLS_BINDIR}/mptools_version.f90)
set(PTOOLSLIB_OMP_SRC  ${PTOOLS_SRCDIR}/mptools_omp.f90)
set(PTOOLSLIB_MPI_SRC  ${PTOOLS_SRCDIR}/mptools_mpi.f90)
set(PTOOLSLIB_OMPI_SRC ${PTOOLS_SRCDIR}/mptools_ompi.f90)
		
set(DIAGMPI_SRC  ${PTOOLS_SRCDIR}/diag_mpi.f90)
set(DIAGOMP_SRC  ${PTOOLS_SRCDIR}/diag_omp.f90)
set(DIAGOMPI_SRC ${PTOOLS_SRCDIR}/diag_ompi.f90)

# Fortran module directory
set(PTOOLS_Fortran_MODDIR ${CMAKE_CURRENT_BINARY_DIR}/modules)

# --- Object libraries ---
# Object library: General
add_library(ptoolslib_obj OBJECT ${PTOOLSLIB_SRC})
set_target_properties(ptoolslib_obj PROPERTIES POSITION_INDEPENDENT_CODE ON)
set_target_properties(ptoolslib_obj PROPERTIES Fortran_MODULE_DIRECTORY ${PTOOLS_Fortran_MODDIR})
target_include_directories(ptoolslib_obj PUBLIC ${PTOOLS_Fortran_MODDIR})

# Object library: OpenMP
add_library(ptoolslib_omp_obj OBJECT ${PTOOLSLIB_OMP_SRC})
set_target_properties(ptoolslib_omp_obj PROPERTIES POSITION_INDEPENDENT_CODE ON)
set_target_properties(ptoolslib_omp_obj PROPERTIES Fortran_MODULE_DIRECTORY ${PTOOLS_Fortran_MODDIR})
target_include_directories(ptoolslib_omp_obj PUBLIC ${PTOOLS_Fortran_MODDIR})
target_link_libraries(ptoolslib_omp_obj ptoolslib_obj OpenMP::OpenMP_Fortran)

# Object library: MPI
add_library(ptoolslib_mpi_obj OBJECT ${PTOOLSLIB_MPI_SRC})
set_target_properties(ptoolslib_mpi_obj PROPERTIES POSITION_INDEPENDENT_CODE ON)
set_target_properties(ptoolslib_mpi_obj PROPERTIES Fortran_MODULE_DIRECTORY ${PTOOLS_Fortran_MODDIR})
target_include_directories(ptoolslib_mpi_obj PUBLIC ${PTOOLS_Fortran_MODDIR})
target_link_libraries(ptoolslib_mpi_obj ptoolslib_obj MPI::MPI_Fortran)

# Object library: OpenMP+MPI
add_library(ptoolslib_ompi_obj OBJECT ${PTOOLSLIB_OMPI_SRC})
set_target_properties(ptoolslib_ompi_obj PROPERTIES POSITION_INDEPENDENT_CODE ON)
set_target_properties(ptoolslib_ompi_obj PROPERTIES Fortran_MODULE_DIRECTORY ${PTOOLS_Fortran_MODDIR})
target_include_directories(ptoolslib_ompi_obj PUBLIC ${PTOOLS_Fortran_MODDIR})
target_link_libraries(ptoolslib_ompi_obj ptoolslib_obj OpenMP::OpenMP_Fortran MPI::MPI_Fortran)

# --- Static libraries ---
# Static library: OpenMP
add_library(ptoolslib_omp_static STATIC "$<TARGET_OBJECTS:ptoolslib_omp_obj>" "$<TARGET_OBJECTS:ptoolslib_obj>")
set_target_properties(ptoolslib_omp_static PROPERTIES OUTPUT_NAME ptools_omp)
target_link_libraries(ptoolslib_omp_static ptoolslib_omp_obj OpenMP::OpenMP_Fortran)
install(TARGETS ptoolslib_omp_static
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Static library: MPI
add_library(ptoolslib_mpi_static STATIC "$<TARGET_OBJECTS:ptoolslib_mpi_obj>" "$<TARGET_OBJECTS:ptoolslib_obj>")
set_target_properties(ptoolslib_mpi_static PROPERTIES OUTPUT_NAME ptools_mpi)
target_link_libraries(ptoolslib_mpi_static ptoolslib_mpi_obj MPI::MPI_Fortran)
install(TARGETS ptoolslib_mpi_static
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Static library: OpenMP+MPI
add_library(ptoolslib_ompi_static STATIC "$<TARGET_OBJECTS:ptoolslib_ompi_obj>" "$<TARGET_OBJECTS:ptoolslib_omp_obj>" "$<TARGET_OBJECTS:ptoolslib_obj>")
set_target_properties(ptoolslib_ompi_static PROPERTIES OUTPUT_NAME ptools_ompi)
target_link_libraries(ptoolslib_ompi_static ptoolslib_ompi_obj OpenMP::OpenMP_Fortran MPI::MPI_Fortran)
install(TARGETS ptoolslib_ompi_static
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

# --- Shared libraries ---
# Shared library: OpenMP
add_library(ptoolslib_omp_shared SHARED "$<TARGET_OBJECTS:ptoolslib_omp_obj>" "$<TARGET_OBJECTS:ptoolslib_obj>")
set_target_properties(ptoolslib_omp_shared PROPERTIES OUTPUT_NAME ptools_omp)
target_link_libraries(ptoolslib_omp_shared ptoolslib_obj OpenMP::OpenMP_Fortran)
install(TARGETS ptoolslib_omp_shared
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Shared library: MPI
add_library(ptoolslib_mpi_shared SHARED "$<TARGET_OBJECTS:ptoolslib_mpi_obj>" "$<TARGET_OBJECTS:ptoolslib_obj>")
set_target_properties(ptoolslib_mpi_shared PROPERTIES OUTPUT_NAME ptools_mpi)
target_link_libraries(ptoolslib_mpi_shared ptoolslib_obj MPI::MPI_Fortran)
install(TARGETS ptoolslib_mpi_shared
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Shared library: OpenMP+MPI
add_library(ptoolslib_ompi_shared SHARED "$<TARGET_OBJECTS:ptoolslib_ompi_obj>" "$<TARGET_OBJECTS:ptoolslib_omp_obj>" "$<TARGET_OBJECTS:ptoolslib_obj>")
set_target_properties(ptoolslib_ompi_shared PROPERTIES OUTPUT_NAME ptools_ompi)
target_link_libraries(ptoolslib_ompi_shared ptoolslib_omp_obj ptoolslib_obj OpenMP::OpenMP_Fortran MPI::MPI_Fortran)
install(TARGETS ptoolslib_ompi_shared
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

# --- Executables ---
# Executable: diag_omp
add_executable(diag_omp ${DIAGOMP_SRC})
target_link_libraries(diag_omp ptoolslib_omp_static)
install(TARGETS diag_omp
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# Executable: diag_mpi
add_executable(diag_mpi ${DIAGMPI_SRC})
target_link_libraries(diag_mpi ptoolslib_mpi_static MPI::MPI_Fortran)
install(TARGETS diag_mpi
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# Executable: diag_ompi
add_executable(diag_ompi ${DIAGOMPI_SRC})
target_link_libraries(diag_ompi ptoolslib_ompi_static MPI::MPI_Fortran)
install(TARGETS diag_ompi
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# Exectutable: show_setup
set(SHOWSETUP_IN  ${PTOOLS_SRCDIR}/show_setup.bsh.in)
set(SHOWSETUP_OUT ${CMAKE_CURRENT_BINARY_DIR}/show_setup.bsh)
configure_file(${SHOWSETUP_IN} ${SHOWSETUP_OUT} @ONLY)
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/show_setup.bsh
        DESTINATION ${CMAKE_INSTALL_BINDIR})

# Install Fortran module files
install(FILES ${PTOOLS_Fortran_MODDIR}/mptools_mpi.mod
              ${PTOOLS_Fortran_MODDIR}/mptools_omp.mod
              ${PTOOLS_Fortran_MODDIR}/mptools_ompi.mod
              ${PTOOLS_Fortran_MODDIR}/mptools_parameters.mod
 	DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

if (BUILD_DOC)
  # Set input and output file.
  set(DOXYGEN_IN  ${PTOOLS_SRCDIR}/Doxyfile.in)
  set(DOXYGEN_OUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)

  if (BUILD_DOC_INTERNAL)
    set(HIDE_UNDOC_MEMBERS YES)
    set(HIDE_UNDOC_CLASSES YES)
    set(ENABLED_SECTIONS   _INTERNAL_)
  else ()
    set(HIDE_UNDOC_MEMBERS NO)
    set(HIDE_UNDOC_CLASSES NO)
    set(ENABLED_SECTION)
  endif ()

  # Configure the doxygen file.
  configure_file(${DOXYGEN_IN} ${DOXYGEN_OUT} @ONLY)

  add_custom_target(api_doc ALL
                    COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYGEN_OUT}
                    WORKING DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                    COMMENT "Generating API documentation with Doxygen"
                    VERBATIM)
  install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html/
          DESTINATION ${CMAKE_INSTALL_DOCDIR})
endif ()

