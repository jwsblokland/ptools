# OpenMP tests
add_test(NAME test_diag_omp COMMAND ${CMAKE_BINARY_DIR}/src/diag_omp)

# MPI tests
set(MPIEXEC_MAX_NUMPROC 2)
add_test(NAME test_diag_mpi       COMMAND ${CMAKE_BINARY_DIR}/src/diag_mpi)
add_test(NAME test_diag_mpi_ranks COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROC}
                                           ${MPIEXEC_PREFLAGS} ${CMAKE_BINARY_DIR}/src/diag_mpi ${MPIEXEC_POSTFLAGS})

# OpenMP+MPI tests
set(MPIEXEC_PREFLAGS "--map-by" "slot:PE=1")
add_test(NAME test_diag_ompi       COMMAND ${CMAKE_BINARY_DIR}/src/diag_ompi)
add_test(NAME test_diag_ompi_ranks COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROC}
                                           ${MPIEXEC_PREFLAGS} ${CMAKE_BINARY_DIR}/src/diag_ompi ${MPIEXEC_POSTFLAGS})

# Environment configuration
set_property(TEST test_diag_omp
                  test_diag_ompi test_diag_ompi_ranks APPEND
             PROPERTY ENVIRONMENT OMP_NUM_THREADS=1)
set_property(TEST test_diag_omp
                  test_diag_ompi test_diag_ompi_ranks APPEND
             PROPERTY ENVIRONMENT OMP_PROC_BIND=close)
set_property(TEST test_diag_omp
                  test_diag_ompi test_diag_ompi_ranks APPEND
             PROPERTY ENVIRONMENT OMP_PLACES=cores)
