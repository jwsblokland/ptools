!> \brief The program diag_ompi analyses the OpenMP+MPI configuration of the current environment.
program diag_ompi
  use, intrinsic :: iso_fortran_env, only: error_unit, int32
  use            :: mpi_f08,         only: MPI_COMM_WORLD, MPI_Datatype, MPI_Status,  &
                                           MPI_Init, MPI_Finalize, MPI_Comm_size, MPI_Comm_rank, MPI_Recv, MPI_Send
  use            :: mptools_ompi,    only: ompi_t, ompi_rank_t, ompi_init_dtypes, ompi_free_dtypes, ompi_rank_analysis, ompi_report
  implicit none

  ! Parameters
  integer, parameter :: MAX_THREADS = 512

  ! Locals
  logical                                                 :: valid
  integer(int32)                                          :: i, ierror, irank, nranks, nthreads
  integer(int32)                                          :: nelems, idx_s, idx_e
  type(ompi_t)                                            :: ompi_info
  type(ompi_rank_t),  dimension(MAX_THREADS)              :: info
  type(ompi_rank_t),  dimension(:),           allocatable :: rank_info
  type(MPI_Datatype)                                      :: ompi_dtype, ompi_rank_dtype 
  type(MPI_Status)                                        :: status

  call MPI_Init(ierror)
  if (ierror /= 0) then
     write(error_unit, 100) ierror
     error stop 1
  end if
  call ompi_init_dtypes(ompi_dtype, ompi_rank_dtype)

  call MPI_Comm_size(MPI_COMM_WORLD, nranks)
  call MPI_Comm_rank(MPI_COMM_WORLD, irank)
  if (irank == 0) then
     valid = ompi_rank_analysis(ompi_info, info)

     nthreads = ompi_info%nthreads
     allocate(rank_info(nranks * nthreads))
     rank_info(1:nthreads) = info(1:nthreads)

     nelems = nthreads
     do i = 1, nranks - 1
        call MPI_Recv(info, nelems, ompi_rank_dtype, i, 0, MPI_COMM_WORLD, status, ierror)
        idx_s                  =  status%mpi_source      * nthreads + 1
        idx_e                  = (status%mpi_source + 1) * nthreads
        rank_info(idx_s:idx_e) = info(1:nthreads)
     end do
  else 
     valid  = ompi_rank_analysis(ompi_info, info)
     nelems = ompi_info%nthreads
     call MPI_Send(info, nelems, ompi_rank_dtype, 0, 0, MPI_COMM_WORLD, ierror)
  end if
     
  if (irank == 0) then
     call ompi_report(ompi_info, rank_info)
  end if

  call ompi_free_dtypes(ompi_dtype, ompi_rank_dtype)
  if (allocated(rank_info))  deallocate(rank_info)
  
  call MPI_Finalize()

100 format("*** ERROR: Unable to initialize MPI (ierror = ", I0, ").")
end program diag_ompi
