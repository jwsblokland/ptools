!> \brief The program diag_omp analyses the OpenMP configuration of the current environment.
program diag_omp
  use, intrinsic :: iso_fortran_env,     only: error_unit
  use            :: mptools_omp,         only: omp_t, omp_thread_t, omp_analysis, omp_report
  use            :: mptools_parameters,  only: MAX_THREADS
  implicit none

  ! Locals
  type(omp_t)                                :: omp_info
  type(omp_thread_t), dimension(MAX_THREADS) :: thread_info

  if (omp_analysis(omp_info, thread_info)) then
     call omp_report(omp_info, thread_info)
  else
     write(error_unit,'(A)') "*** ERROR: Unable to create OpenMP analysis report"
     error stop 1
  end if
end program diag_omp
