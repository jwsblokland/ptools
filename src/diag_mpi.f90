!> \brief The program diag_mpi analyses the MPI configuration of the current environment.
program diag_mpi
  use, intrinsic :: iso_fortran_env, only: error_unit, int32
  use            :: mpi_f08,         only: MPI_COMM_WORLD, MPI_Datatype, MPI_Status,  &
                                           MPI_Init, MPI_Finalize, MPI_Comm_size, MPI_Comm_rank, MPI_Recv, MPI_Send
  use            :: mptools_mpi,     only: mpi_t, mpi_rank_t, mpi_analysis, mpi_report
  implicit none

  ! Locals
  integer(int32)                              :: irank, ierror
  type(mpi_t)                                 :: mpi_info
  type(mpi_rank_t), dimension(:), allocatable :: rank_info

  call MPI_Init(ierror)
  if (ierror /= 0) then
     write(error_unit, 100) ierror
     error stop 1
  end if

  call MPI_Comm_rank(MPI_COMM_WORLD, irank)
  call mpi_analysis(mpi_info, rank_info)
  if (irank == 0) then
     call mpi_report(mpi_info, rank_info)
  end if

  if (allocated(rank_info))  deallocate(rank_info)
  
  call MPI_Finalize()

100 format("*** ERROR: Unable to initialize MPI (ierror = ", I0, ").")
end program diag_mpi
