!> \brief The program diag_ompi analyses the OpenMP+MPI configuration of the current environment.
program diag_ompi
  use, intrinsic :: iso_fortran_env, only: error_unit, int32
  use            :: mpi_f08,         only: MPI_COMM_WORLD, MPI_Comm_rank, MPI_Finalize, MPI_Init
  use            :: mptools_ompi,    only: ompi_t, ompi_rank_t, ompi_analysis, ompi_report
  implicit none

  ! Locals
  integer(int32)                                :: ierror, irank
  type(ompi_t)                                  :: ompi_info
  type(ompi_rank_t),  dimension(:), allocatable :: rank_info

  call MPI_Init(ierror)
  if (ierror /= 0) then
     write(error_unit, 100) ierror
     error stop 1
  end if

  call MPI_Comm_rank(MPI_COMM_WORLD, irank)
  call ompi_analysis(ompi_info, rank_info)
  if (irank == 0) then
     call ompi_report(ompi_info, rank_info)
  end if

  if (allocated(rank_info))  deallocate(rank_info)
  
  call MPI_Finalize()

100 format("*** ERROR: Unable to initialize MPI (ierror = ", I0, ").")
end program diag_ompi
