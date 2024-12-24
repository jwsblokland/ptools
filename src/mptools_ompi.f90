!> \brief Module containing parameters, functions and subroutines for analyzing
!!        the MPI configuration.
module mptools_ompi
  use, intrinsic :: iso_fortran_env,     only: int32
  use, intrinsic :: omp_lib_kinds,       only: OMP_PROC_BIND_KIND, OMP_SCHED_KIND
  use            :: mptools_parameters,  only: NAME_SIZE, STR_SIZE
  implicit none

  private
  public  :: ompi_analysis, ompi_report

  !> \brief OpenMP+MPI derived type.
  type, public :: ompi_t
     integer(int32)              :: version    = -1  !< MPI major version number.
     integer(int32)              :: subversion = -1  !< MPI minor version number.
     integer(int32)              :: nranks     = -1  !< Total number of MPI ranks.
     integer(int32)              :: nthreads   = -1  !< Total number of OpenMP threads per MPI rank.
     integer(OMP_PROC_BIND_KIND) :: proc_bind  = -1  !< OpenMP proc bind identifier.
     integer(OMP_SCHED_KIND)     :: schedule   = -1  !< OpenMP schedule identitfier.
     integer(int32)              :: chunk_size = -1  !< OpenMP chunk size.
  end type ompi_t

  !> \brief OpenMP+MPI rank derived type.
  type, public :: ompi_rank_t
     character(len=NAME_SIZE) :: hostname = "(null)" !< Hostname of the rank.
     integer(int32)           :: rankID   = -1       !< MPI rank identifier.
     integer(int32)           :: threadID = -1       !< OpenMP thread identifier.
     integer(int32)           :: vcoreID  = -1       !< Virtual core identifier.
     integer(int32)           :: placeID  = -1       !< OpenMP place identifier.
  end type ompi_rank_t

contains
  !> \cond _INTERNAL_
  !> \brief Checks the size of the array rank_info.
  function check_rank_info(rank_info, nthreads) result(valid)
    use, intrinsic :: iso_fortran_env,  only: error_unit

    type(ompi_rank_t), dimension(:), intent(in) :: rank_info  !< OMPI rank information.
    integer(int32),                  intent(in) :: nthreads   !< Total number of threads per rank.
    logical                                     :: valid      !< TRUE if the sizes of rank_info are valid otherwise FALSE.

    valid = .true.
    if (size(rank_info) < nthreads) then
       valid = .false.
       write(error_unit, 100) size(rank_info), nthreads
    end if

100 format("*** ERROR: The length of the dimension of the array rank_info (", I0, ") is too small. ",  &
           "Its length should be at least ", I0, ".")
  end function check_rank_info
  !> \endcond

  !> \cond _INTERNAL_
  !> \brief Initializes and commits the MPI datatype ompi_dtype and ompi_rank_dtype.
  subroutine ompi_init_dtypes(ompi_dtype, ompi_rank_dtype)
    use :: mpi_f08,  only: MPI_ADDRESS_KIND, MPI_CHARACTER, MPI_INTEGER4, MPI_Datatype,  &
                           MPI_Type_create_struct, MPI_Type_commit

    type(MPI_Datatype), intent(out) :: ompi_dtype       !< MPI datatype of the derived type ompi_t
    type(MPI_Datatype), intent(out) :: ompi_rank_dtype  !< MPI datatype of the derived type ompi_rank_t

    ! Locals
    integer(int32)                                       :: count
    integer(int32),            dimension(:), allocatable :: blklen
    integer(MPI_ADDRESS_KIND), dimension(:), allocatable :: disp
    type(MPI_Datatype),        dimension(:), allocatable :: dtypes

    ! ompi_t
    count = 7
    allocate(blklen(count), disp(count), dtypes(count))
    blklen = [ 1,            1,            1,            1,            1,            1,            1            ]
    disp   = [ 0,            4,            8,            12,           16,           20,           24           ]
    dtypes = [ MPI_INTEGER4, MPI_INTEGER4, MPI_INTEGER4, MPI_INTEGER4, MPI_INTEGER4, MPI_INTEGER4, MPI_INTEGER4 ]
    call MPI_Type_create_struct(count, blklen, disp, dtypes, ompi_dtype)
    call MPI_Type_commit(ompi_dtype)
    if (allocated(blklen))  deallocate(blklen)
    if (allocated(disp))    deallocate(disp)
    if (allocated(dtypes))  deallocate(dtypes)

    ! ompi_rank_t
    count = 5
    allocate(blklen(count), disp(count), dtypes(count))
    blklen = [ 64,            1,            1,            1,            1            ]
    disp   = [ 0,             64,           68,           72,           76           ]
    dtypes = [ MPI_CHARACTER, MPI_INTEGER4, MPI_INTEGER4, MPI_INTEGER4, MPI_INTEGER4 ]
    call MPI_Type_create_struct(count, blklen, disp, dtypes, ompi_rank_dtype)
    call MPI_Type_commit(ompi_rank_dtype)
    if (allocated(blklen))  deallocate(blklen)
    if (allocated(disp))    deallocate(disp)
    if (allocated(dtypes))  deallocate(dtypes)
  end subroutine ompi_init_dtypes
  !> \endcond

  !> \cond _INTERNAL_
  !> \brief Frees the MPI datatype ompi_dtype and ompi_rank_dtype.
  subroutine ompi_free_dtypes(ompi_dtype, ompi_rank_dtype)
    use :: mpi_f08,  only: MPI_Datatype, MPI_Type_free

    type(MPI_Datatype), intent(inout) :: ompi_dtype       !< MPI datatype of the derived type ompi_t
    type(MPI_Datatype), intent(inout) :: ompi_rank_dtype  !< MPI datatype of the derived type ompi_rank_t

    call MPI_Type_free(ompi_dtype)
    call MPI_Type_free(ompi_rank_dtype)
  end subroutine ompi_free_dtypes
  !> \endcond

  !> \cond _INTERNAL_
  !> \brief Perform the OpenMP+MPI analysis for a rank.
  subroutine ompi_rank_analysis(ompi_info, rank_info)
    use, intrinsic :: iso_fortran_env,  only: int32
    use            :: mpi_f08,          only: MPI_COMM_WORLD,                                 &
                                              MPI_Comm_rank, MPI_Comm_size, MPI_Get_version
    use            :: mptools_omp,      only: omp_t, omp_thread_t, omp_analysis
    use            :: mptools_system,   only: get_hostname

    type(ompi_t),                                 intent(out) :: ompi_info  !< General OMPI information.
    type(ompi_rank_t), dimension(:), allocatable, intent(out) :: rank_info  !< OMPI rank information.

    ! Locals
    character(len=NAME_SIZE)                      :: hostname
    integer(int32)                                :: irank, nranks, ithread
    integer(int32)                                :: version, subversion
    type(omp_t)                                   :: omp_info
    type(omp_thread_t), dimension(:), allocatable :: thread_info

    version    = -1
    subversion = -1
    irank      = -1
    nranks     = -1
    ithread    = -1

    ! General informaton
    call MPI_Get_version(version, subversion)
    call MPI_Comm_size(MPI_COMM_WORLD, nranks)
    ompi_info%version    = version
    ompi_info%subversion = subversion
    ompi_info%nranks     = nranks

    ! Rank information
    call MPI_Comm_rank(MPI_COMM_WORLD, irank)
    call omp_analysis(omp_info, thread_info)

    ! General information
    ompi_info%nthreads   = omp_info%nthreads
    ompi_info%proc_bind  = omp_info%proc_bind
    ompi_info%schedule   = omp_info%schedule
    ompi_info%chunk_size = omp_info%chunk_size

    ! Thread information
    hostname = get_hostname()
    if (allocated(rank_info))  deallocate(rank_info)
    allocate(rank_info(omp_info%nthreads))
    do ithread = 1, omp_info%nthreads
       rank_info(ithread)%hostname = hostname
       rank_info(ithread)%rankid   = irank
       rank_info(ithread)%threadID = thread_info(ithread)%threadID
       rank_info(ithread)%vcoreID  = thread_info(ithread)%vcoreID
       rank_info(ithread)%placeID  = thread_info(ithread)%placeID
    end do

    if (allocated(thread_info))  deallocate(thread_info)
  end subroutine ompi_rank_analysis
  !> \endcond

  !> \brief Perform OpenMP+MPI analysis for all the ranks.
  !!
  !! \warning Be aware, the variable \p rank_info will only be allocated for rank 0.
  subroutine ompi_analysis(ompi_info, rank_info)
    use :: mpi_f08,  only: MPI_COMM_WORLD, MPI_Datatype, MPI_Status,          &
                           MPI_Comm_size, MPI_Comm_rank, MPI_Recv, MPI_Send

    type(ompi_t),                                 intent(out) :: ompi_info  !< General OMPI information.
    type(ompi_rank_t), dimension(:), allocatable, intent(out) :: rank_info  !< OMPI rank information.

    ! Locals
    integer(int32)                                :: i, ierror, irank, nranks, nthreads
    integer(int32)                                :: nelems, idx_s, idx_e
    type(ompi_rank_t),  dimension(:), allocatable :: info
    type(MPI_Datatype)                            :: ompi_dtype, ompi_rank_dtype
    type(MPI_Status)                              :: status

    call ompi_init_dtypes(ompi_dtype, ompi_rank_dtype)
    call MPI_Comm_size(MPI_COMM_WORLD, nranks)
    call MPI_Comm_rank(MPI_COMM_WORLD, irank)
    if (irank == 0) then
       call ompi_rank_analysis(ompi_info, info)
       nthreads = ompi_info%nthreads
       if (allocated(rank_info))  deallocate(rank_info)
       allocate(rank_info(nranks * nthreads))
       rank_info(1:nthreads) = info(1:nthreads)

       nelems = nthreads
       if (.not. allocated(info))  allocate(info(nelems))
       do i = 1, nranks - 1
          call MPI_Recv(info, nelems, ompi_rank_dtype, i, 0, MPI_COMM_WORLD, status, ierror)
          idx_s                  =  status%mpi_source      * nthreads + 1
          idx_e                  = (status%mpi_source + 1) * nthreads
          rank_info(idx_s:idx_e) = info(1:nthreads)
       end do
    else
       call ompi_rank_analysis(ompi_info, info)
       nelems = ompi_info%nthreads
       call MPI_Send(info, nelems, ompi_rank_dtype, 0, 0, MPI_COMM_WORLD, ierror)
    end if

    if (allocated(info))  deallocate(info)
    call ompi_free_dtypes(ompi_dtype, ompi_rank_dtype)
  end subroutine ompi_analysis

  !> \brief Reports the OpenMP+MPI configuration to the screen or file.
  subroutine ompi_report(ompi_info, rank_info, unit)
    use, intrinsic :: iso_fortran_env,  only: int32, output_unit
    use            :: mptools_omp,      only: get_omp_proc_bind_type, get_omp_schedule_type
    use            :: mptools_version,  only: version

    type(ompi_t),                    intent(in) :: ompi_info  !< General OMPI information.
    type(ompi_rank_t), dimension(:), intent(in) :: rank_info  !< OMPI rank information.
    integer(int32),    optional,     intent(in) :: unit       !< File unit.

    ! Locals
    integer(int32) :: i, lunit, nranks, nthreads

    ! Checks.
    nranks   = ompi_info%nranks
    nthreads = ompi_info%nthreads
    if (.not. check_rank_info(rank_info, nthreads))  return

    lunit = output_unit
    if (present(unit))  lunit = unit

    write(lunit, '(A,A,A)')      "--- ptools ", trim(version()), ": OpenMP+MPI analysis report ---"
    write(lunit, '(A,I0,A1,I0)') "  MPI version:      ", ompi_info%version, ".", ompi_info%subversion
    write(lunit, '(A,I0)')       "  Ranks:            ", nranks
    write(lunit, '(A,I0)')       "  Threads per rank: ", nthreads
    write(lunit, '(A,A)')        "  Proc bind:        ", trim(get_omp_proc_bind_type(ompi_info%proc_bind))
    write(lunit, '(A,A)')        "  Schedule:         ", trim(get_omp_schedule_type(ompi_info%schedule))
    write(lunit, '(A,I0)')       "  Chunk size:       ", ompi_info%chunk_size
    write(lunit, '(A)')          "  Rank info"
    write(lunit, 100)            "Hostname", "RankID", "ThreadID", "vCoreID", "PlaceID"
    do i = 1, nranks * nthreads
       write(output_unit, 110) trim(rank_info(i)%hostname), rank_info(i)%rankID,  rank_info(i)%threadID, rank_info(i)%vcoreID,  &
                               rank_info(i)%placeID
    end do

100 format(4X,A16,2X,A6,2X,A8,2X,A7,2X,A7)
110 format(4X,A16,2X,I6,2X,I8,2X,I7,2X,I7)
  end subroutine ompi_report
end module mptools_ompi
