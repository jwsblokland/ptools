!> \brief Module containing parameters, functions and subroutines for analyzing
!!        the OpenMP configuration.
module mptools_omp
  use, intrinsic :: iso_fortran_env, only: int32
  use, intrinsic :: omp_lib_kinds,   only: OMP_PROC_BIND_KIND, OMP_SCHED_KIND
  implicit none

  private
  public  :: omp_analysis, omp_report, get_omp_proc_bind_type, get_omp_schedule_type

  !> \brief OpenMP derived type.
  type, public :: omp_t
     integer(int32)              :: nthreads   = -1  !< Total number of OpenMP threads.
     integer(OMP_PROC_BIND_KIND) :: proc_bind  = -1  !< OpenMP proc bind identifier.
     integer(OMP_SCHED_KIND)     :: schedule   = -1  !< OpenMP schedule identitfier.
     integer(int32)              :: chunk_size = -1  !< OpenMP chunk size.
  end type omp_t

  !> \brief OpenMP thread derived type.
  type, public :: omp_thread_t
     integer(int32) :: threadID = -1  !< OpenMP thread identifier.
     integer(int32) :: vcoreID  = -1  !< Virtual core identifier.
     integer(int32) :: placeID  = -1  !< OpenMP place identifier.
  end type omp_thread_t

contains
  !> \cond _INTERNAL_
  !> \brief Checks the size of the array thread_info.
  function check_thread_info(thread_info, nthreads) result(valid)
    use, intrinsic :: iso_fortran_env,  only: error_unit
    
    type(omp_thread_t), dimension(:), intent(in) :: thread_info  !< OpenMP thread information.
    integer(int32),                   intent(in) :: nthreads     !< Total number of threads.
    logical                                      :: valid        !< TRUE if the sizes of thread_info are valid otherwise FALSE.

    valid = .true.
    if (size(thread_info) < nthreads) then
       valid = .false.
       write(error_unit, 100) size(thread_info), nthreads
    end if

100 format("*** ERROR: The length of the dimension of the array thread_info (", I0, ") is too small. ",  &
           "Its length should be at least ", I0, ".")
  end function check_thread_info
  !> \endcond
  
  !> \cond _INTERNAL_
  !> \brief Get the OpenMP schedule string.
  function get_omp_schedule_type(schedule_id) result(schedule)
    use, intrinsic :: iso_fortran_env,     only: int32
    use, intrinsic :: omp_lib_kinds,       only: OMP_SCHED_STATIC, OMP_SCHED_DYNAMIC, OMP_SCHED_GUIDED, OMP_SCHED_AUTO
    use            :: mptools_parameters,  only: STR_SIZE

    integer(OMP_SCHED_KIND), intent(in) :: schedule_id  !< OpenMP schedule identification number.
    character(len=STR_SIZE)             :: schedule     !< Schedule type as string.

    select case (schedule_id)
    case (OMP_SCHED_STATIC)
       schedule = "static"
    case (OMP_SCHED_DYNAMIC)
       schedule = "dynamic"
    case (OMP_SCHED_GUIDED)
       schedule = "guided"
    case (OMP_SCHED_AUTO)
       schedule = "auto"
    case default
       schedule = "unknown"
    end select
  end function get_omp_schedule_type
  !> \endcond

  !> \cond _INTERNAL_
  !> \brief Get the OpenMP schedule string.
  function get_omp_proc_bind_type(bind_id) result(bind)
    use, intrinsic :: iso_fortran_env,     only: int32
    use, intrinsic :: omp_lib,             only: OMP_PROC_BIND_FALSE, OMP_PROC_BIND_TRUE, OMP_PROC_BIND_MASTER,  &
                                                 OMP_PROC_BIND_CLOSE, OMP_PROC_BIND_SPREAD
    use            :: mptools_parameters,  only: STR_SIZE

    integer(OMP_PROC_BIND_KIND), intent(in) :: bind_id  !< OpenMP thread affinity identification number.
    character(len=STR_SIZE)                 :: bind     !< Thread affinity type as string.

    select case (bind_id)
    case (OMP_PROC_BIND_FALSE)
       bind = "false"
    case (OMP_PROC_BIND_TRUE)
       bind = "true"
    case (OMP_PROC_BIND_MASTER)
       bind = "master"
    case (OMP_PROC_BIND_CLOSE)
       bind = "close"
    case (OMP_PROC_BIND_SPREAD)
       bind = "spread"
    case default
       bind = "unknown"
    end select
  end function get_omp_proc_bind_type
  !> \endcond

  !> \brief Perform the OpenMP analysis.
  function omp_analysis(omp_info, thread_info) result(valid)
    use, intrinsic :: iso_fortran_env,  only: int32
    use            :: omp_lib,          only: omp_get_place_num, omp_get_proc_bind, omp_get_thread_num, omp_get_num_threads
    use            :: mptools_system,   only: get_vcore_id

    type(omp_t),                      intent(out) :: omp_info     !< General OpenMP information.
    type(omp_thread_t), dimension(:), intent(out) :: thread_info  !< OpenMP thread information.
    logical                                       :: valid        !< TRUE is the OpenMP analysis is performed succesfully, otherwise FALSE.

    ! Locals
    integer(int32) :: i, nthreads

    ! General informaton
    !$omp parallel
    !$omp single
    nthreads           = omp_get_num_threads()
    omp_info%nthreads  = nthreads
    omp_info%proc_bind = omp_get_proc_bind()
    call omp_get_schedule(omp_info%schedule, omp_info%chunk_size)
    !$omp end single
    !$omp end parallel

    ! Thread information
    valid = check_thread_info(thread_info, nthreads)
    if (valid) then
       !$omp parallel
       !$omp do  &
       !$omp   private(i)
       do i = 1, nthreads
          thread_info(i)%threadID = omp_get_thread_num()
          thread_info(i)%vcoreID  = get_vcore_id()
          thread_info(i)%placeID  = omp_get_place_num()
       end do
       !$omp end do
       !$omp end parallel
    end if
  end function omp_analysis

  !> \brief Reports the OpenMP configuration to the screen or file.
  subroutine omp_report(omp_info, thread_info, unit)
    use, intrinsic :: iso_fortran_env,  only: int32, output_unit
    use            :: mptools_version,  only: version

    type(omp_t),                      intent(in) :: omp_info     !< General OpenMP information.
    type(omp_thread_t), dimension(:), intent(in) :: thread_info  !< OpenMP thread information.
    integer(int32),     optional,     intent(in) :: unit         !< File unit.

    ! Locals
    integer(int32) :: i, lunit, nthreads

    ! Checks.
    nthreads = omp_info%nthreads
    if (.not. check_thread_info(thread_info, nthreads))  return
    
    lunit = output_unit
    if (present(unit))  lunit = unit

    write(lunit, '(A,A,A)') "--- ptools ", trim(version()), ": OpenMP analysis report ---"
    write(lunit, '(A,I0)')  "  Threads:    ", nthreads
    write(lunit, '(A,A)')   "  Proc bind:  ", trim(get_omp_proc_bind_type(omp_info%proc_bind))
    write(lunit, '(A,A)')   "  Schedule:   ", trim(get_omp_schedule_type(omp_info%schedule))
    write(lunit, '(A,I0)')  "  Chunk size: ", omp_info%chunk_size
    write(lunit, '(A)')     "  Thread info"
    write(lunit, 100)       "ThreadID", "vCoreID", "PlaceID"
    do i = 1, nthreads
       write(output_unit, 110) thread_info(i)%threadID, thread_info(i)%vcoreID, thread_info(i)%placeID
    end do
  
100 format(4X,A8,2X,A7,2X,A7)
110 format(4X,I8,2X,I7,2X,I7)
  end subroutine omp_report
end module mptools_omp
