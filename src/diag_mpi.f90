!> \brief The program diag_mpi analyses the MPI configuration of the current environment.
program diag_mpi
  use, intrinsic :: iso_fortran_env, only: error_unit
  use            :: mpi_f08,         only: MPI_COMM_WORLD, MPI_Datatype, MPI_Status,  &
                                           MPI_Init, MPI_Finalize, MPI_Comm_size, MPI_Comm_rank, MPI_Recv, MPI_Send
  use            :: mptools_mpi,     only: mpi_t, mpi_rank_t, mpi_init_dtypes, mpi_free_dtypes, mpi_rank_analysis, mpi_report
  implicit none

  ! Locals
  integer                                     :: i, ierror, irank, nranks
  type(mpi_t)                                 :: mpi_info
  type(mpi_rank_t)                            :: info
  type(mpi_rank_t), dimension(:), allocatable :: rank_info
  logical                                     :: valid
  type(MPI_Datatype)                          :: mpi_dtype, mpi_rank_dtype 
  type(MPI_Status)                            :: status

  call MPI_Init(ierror)
  if (ierror /= 0) then
     write(error_unit, 100) ierror
     error stop 1
  end if
  call mpi_init_dtypes(mpi_dtype, mpi_rank_dtype)

  call MPI_Comm_size(MPI_COMM_WORLD, nranks)
  call MPI_Comm_rank(MPI_COMM_WORLD, irank)
  if (irank == 0) then
     allocate(rank_info(nranks))

     valid = mpi_rank_analysis(mpi_info, info)
     rank_info(1) = info

     do i = 1, nranks - 1
        call MPI_Recv(info, 1, mpi_rank_dtype, i, 0, MPI_COMM_WORLD, status, ierror)
        rank_info(status%mpi_source + 1) = info
     end do
  else 
     valid = mpi_rank_analysis(mpi_info, info)
     call MPI_Send(info, 1, mpi_rank_dtype, 0, 0, MPI_COMM_WORLD, ierror)
  end if
     
  if (irank == 0) then
     call mpi_report(mpi_info, rank_info)
  end if

  call mpi_free_dtypes(mpi_dtype, mpi_rank_dtype)
  if (allocated(rank_info))  deallocate(rank_info)
  
  call MPI_Finalize()

100 format("*** ERROR: Unable to initialize MPI (ierror = ", I0, ").")
end program diag_mpi
