!> \brief Module containing parameters, functions and subroutines for analyzing
!!        the MPI configuration.
module mptools_mpi
  use, intrinsic :: iso_fortran_env,     only: int32
  use, intrinsic :: iso_c_binding
  use            :: mptools_parameters,  only: NAME_SIZE, STR_SIZE
  implicit none

  private
  public  :: mpi_analysis, mpi_report

  !> \brief MPI derived type.
  type, public :: mpi_t
     integer(int32) :: version    = -1  !< MPI major version number.
     integer(int32) :: subversion = -1  !< MPI minor version number.
     integer(int32) :: nranks     = -1  !< Total number of MPI ranks.
  end type mpi_t

  !> \brief MPI rank derived type.
  type, public :: mpi_rank_t
     character(len=NAME_SIZE) :: hostname = "(null)" !< Hostname of the rank.
     integer(int32)           :: rankID   = -1       !< MPI rank identifier.
     integer(int32)           :: vcoreID  = -1       !< Virtual core identifier.
  end type mpi_rank_t

contains
  !> \cond _INTERNAL_
  !> \brief Checks the size of the array rank_info.
  function check_rank_info(rank_info, nranks) result(valid)
    use, intrinsic :: iso_fortran_env,  only: error_unit

    type(mpi_rank_t), dimension(:), intent(in) :: rank_info  !< OMPI rank information.
    integer(int32),                 intent(in) :: nranks     !< Total number of ranks
    logical                                    :: valid      !< TRUE if the sizes of rank_info are valid otherwise FALSE.

    valid = .true.
    if (size(rank_info) < nranks) then
       valid = .false.
       write(error_unit, 100) size(rank_info), nranks
    end if

100 format("*** ERROR: The length of the dimension of the array rank_info (", I0, ") is too small. ",  &
           "Its length should be at least ", I0, ".")
  end function check_rank_info
  !> \endcond

  !> \cond _INTERNAL_
  !> \brief Initializes and commits the MPI datatype ompi_dtype and ompi_rank_dtype.
  subroutine mpi_init_dtypes(mpi_dtype, mpi_rank_dtype)
    use :: mpi_f08,  only: MPI_ADDRESS_KIND, MPI_CHARACTER, MPI_INTEGER4, MPI_Datatype, &
                           MPI_Type_create_struct, MPI_Type_commit

    type(MPI_Datatype), intent(out) :: mpi_dtype       !< MPI datatype of the derived type mpi_t
    type(MPI_Datatype), intent(out) :: mpi_rank_dtype  !< MPI datatype of the derived type mpi_rank_t

    ! Locals
    integer(int32)                                       :: count
    integer(int32),            dimension(:), allocatable :: blklen
    integer(MPI_ADDRESS_KIND), dimension(:), allocatable :: disp
    type(MPI_Datatype),        dimension(:), allocatable :: dtypes

    ! mpi_t
    count = 3
    allocate(blklen(count), disp(count), dtypes(count))
    blklen = [ 1,            1,            1            ]
    disp   = [ 0,            4,            8            ]
    dtypes = [ MPI_INTEGER4, MPI_INTEGER4, MPI_INTEGER4 ]
    call MPI_Type_create_struct(count, blklen, disp, dtypes, mpi_dtype)
    call MPI_Type_commit(mpi_dtype)
    if (allocated(blklen))  deallocate(blklen)
    if (allocated(disp))    deallocate(disp)
    if (allocated(dtypes))  deallocate(dtypes)

    ! mpi_rank_t
    count = 3
    allocate(blklen(count), disp(count), dtypes(count))
    blklen = [ 64,            1,            1            ]
    disp   = [ 0,             64,           68           ]
    dtypes = [ MPI_CHARACTER, MPI_INTEGER4, MPI_INTEGER4 ]
    call MPI_Type_create_struct(count, blklen, disp, dtypes, mpi_rank_dtype)
    call MPI_Type_commit(mpi_rank_dtype)
    if (allocated(blklen))  deallocate(blklen)
    if (allocated(disp))    deallocate(disp)
    if (allocated(dtypes))  deallocate(dtypes)
  end subroutine mpi_init_dtypes
  !> \endcond

  !> \cond _INTERNAL_
  !> \brief Frees the MPI datatype ompi_dtype and ompi_rank_dtype.
  subroutine mpi_free_dtypes(mpi_dtype, mpi_rank_dtype)
    use :: mpi_f08,  only: MPI_Datatype, MPI_Type_free

    type(MPI_Datatype), intent(inout) :: mpi_dtype       !< MPI datatype of the derived type mpi_t
    type(MPI_Datatype), intent(inout) :: mpi_rank_dtype  !< MPI datatype of the derived type mpi_rank_t

    call MPI_Type_free(mpi_dtype)
    call MPI_Type_free(mpi_rank_dtype)
  end subroutine mpi_free_dtypes
  !> \endcond

  !> \cond _INTERNAL_
  !> \brief Perform the MPI analysis for a rank.
  function mpi_rank_analysis(mpi_info, rank_info) result(valid)
    use, intrinsic :: iso_fortran_env,  only: int32
    use            :: mpi_f08,          only: MPI_COMM_WORLD,                                 &
                                              MPI_Comm_rank, MPI_Comm_size, MPI_Get_version
    use            :: mptools_system,   only: get_hostname, get_vcore_id

    type(mpi_t),      intent(out) :: mpi_info   !< General MPI information.
    type(mpi_rank_t), intent(out) :: rank_info  !< MPI rank information.
    logical                       :: valid      !< TRUE is the MPI analysis is performed succesfully, otherwise FALSE.

    ! Locals
    integer(int32) :: irank, nranks, version, subversion

    valid      = .true.
    version    = -1
    subversion = -1

    ! General informaton
    call MPI_Get_version(version, subversion)
    call MPI_Comm_size(MPI_COMM_WORLD, nranks)
    mpi_info%version    = version
    mpi_info%subversion = subversion
    mpi_info%nranks     = nranks

    ! Rank information
    call MPI_Comm_rank(MPI_COMM_WORLD, irank)
    rank_info%hostname = get_hostname()
    rank_info%rankID   = irank
    rank_info%vcoreID  = get_vcore_id()
  end function mpi_rank_analysis
  !> \endcond

  !> \brief Perfom the MPI analysis for all ranks.
  !!
  !! \warning Be aware, the variable \p rank_info will only be allocated for rank 0.
  subroutine mpi_analysis(mpi_info, rank_info)
    use :: mpi_f08,  only: MPI_COMM_WORLD, MPI_Datatype, MPI_Status,          &
                           MPI_Comm_size, MPI_Comm_rank, MPI_Recv, MPI_Send

    type(mpi_t),                                 intent(out) :: mpi_info   !< General MPI information.
    type(mpi_rank_t), dimension(:), allocatable, intent(out) :: rank_info  !< MPI rank information.

    ! Locals
    integer            :: i, ierror, irank, nranks
    type(mpi_rank_t)   :: info
    logical            :: valid
    type(MPI_Datatype) :: mpi_dtype, mpi_rank_dtype
    type(MPI_Status)   :: status

    call mpi_init_dtypes(mpi_dtype, mpi_rank_dtype)
    call MPI_Comm_size(MPI_COMM_WORLD, nranks)
    call MPI_Comm_rank(MPI_COMM_WORLD, irank)
    if (irank == 0) then
       if (allocated(rank_info))  deallocate(rank_info)
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

    call mpi_free_dtypes(mpi_dtype, mpi_rank_dtype)
  end subroutine mpi_analysis

  !> \brief Reports the MPI configuration to the screen or file.
  subroutine mpi_report(mpi_info, rank_info, unit)
    use, intrinsic :: iso_fortran_env,  only: int32, output_unit, error_unit
    use            :: mptools_version,  only: version

    type(mpi_t),                    intent(in) :: mpi_info   !< General MPI information.
    type(mpi_rank_t), dimension(:), intent(in) :: rank_info  !< MPI rank information.
    integer(int32),   optional,     intent(in) :: unit       !< File unit.

    ! Locals
    integer(int32) :: i, lunit, nranks

    ! Checks.
    if (.not. check_rank_info(rank_info, mpi_info%nranks))  return

    nranks = mpi_info%nranks

    lunit = output_unit
    if (present(unit))  lunit = unit

    write(lunit, '(A,A,A)')      "--- ptools ", trim(version()), ": MPI analysis report ---"
    write(lunit, '(A,I0,A1,I0)') "  MPI version: ", mpi_info%version, ".", mpi_info%subversion
    write(lunit, '(A,I0)')       "  Ranks:       ", nranks
    write(lunit, '(A)')          "  Rank info"
    write(lunit, 200)            "Hostname", "RankID", "vCoreID"
    do i = 1, nranks
       write(output_unit, 210) trim(rank_info(i)%hostname), rank_info(i)%rankID, rank_info(i)%vcoreID
    end do

200 format(4X,A16,2X,A6,2X,A7)
210 format(4X,A16,2X,I6,2X,I7)
  end subroutine mpi_report
end module mptools_mpi
