!> \brief The program diag_omp analyses the OpenMP configuration of the current environment.
program diag_omp
  use, intrinsic :: iso_fortran_env,  only: error_unit
  use            :: mptools_omp,      only: omp_t, omp_thread_t, omp_analysis, omp_report
  implicit none

  ! Locals
  type(omp_t)                                   :: omp_info
  type(omp_thread_t), dimension(:), allocatable :: thread_info

  call omp_analysis(omp_info, thread_info)
  call omp_report(omp_info, thread_info)

  if (allocated(thread_info))  deallocate(thread_info)
end program diag_omp
