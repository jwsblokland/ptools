  !> \cond _INTERNAL_
!> \brief Module containing parameters, functions and subroutines for retrieving
!!        the system information.
module mptools_system
  use, intrinsic :: iso_c_binding,  only: c_int, c_ptr, c_size_t
  implicit none

  private
  public  :: get_hostname, get_vcore_id

  interface
     !> \brief Interface to the C function gethostname() for retrieving the hostname of the machine.
     !!
     !! \return  On success, zero is returned.  On error, -1 is returned, and errno is set to indicate the error.
     function gethostname_c(name, len) result(res) bind(C, name="gethostname")
       import c_int, c_ptr, c_size_t

       type(c_ptr),       value, intent(in) :: name  !< Name of the host.
       integer(c_size_t),        intent(in) :: len   !< Maximum allowed string length.
       integer(c_int)                       :: res
     end function gethostname_c
     
     !> \brief Interface to the C function sched_getcpu() for retrieving the CPU identification number.
     !!
     !! \return  CPU identification number.
     function sched_getcpu_c() result(cpu_id) bind(C, name="sched_getcpu")
       import c_int

       integer(c_int) :: cpu_id
     end function sched_getcpu_c
  end interface

contains
  !> \brief Get the name of the host.
  function get_hostname() result(hostname)
    use, intrinsic :: iso_fortran_env,     only: int32
    use, intrinsic :: iso_c_binding,       only: c_char, c_loc
    use            :: mptools_parameters,  only: NAME_SIZE
    
    character(len=NAME_SIZE) :: hostname  !< Name of the host.

    ! Locals
    integer(int32)                                                   :: res
    character(len=1, kind=c_char), dimension(:), allocatable, target :: hostname_c
    type(c_ptr)                                                      :: phostname_c

    hostname = " "
    allocate(hostname_c(NAME_SIZE))
    phostname_c = c_loc(hostname_c)
    res = int(gethostname_c(phostname_c, NAME_SIZE), int32)
    call C_F_string(hostname_c, hostname)

    if (allocated(hostname_c))  deallocate(hostname_c)
  end function get_hostname
  
  !> \brief Get the virtual core identification number.
  function get_vcore_id() result(core_id)
    use :: iso_fortran_env, only: int32

    integer(int32) :: core_id  !< Core identification number

    core_id = int(sched_getcpu_c(), int32)
  end function get_vcore_id

  !> \brief Converts a C type string to a Fortran one.
  subroutine C_F_string(C_string, F_string)
    use, intrinsic :: iso_c_binding,  only: c_char, c_null_char
    
    character(len=1, kind=c_char), dimension(*), intent(in)  :: C_string  !< C-type string.
    character(len=*),                            intent(out) :: F_string  !< Fortran-type string.

    ! Locals
    integer :: i

    i = 1
    do while (C_string(i) /= c_null_char .and. i <= len(F_string))
      F_string(i:i) = C_string(i)
      i             = i+1
    end do
    if (i<len(F_string)) F_string(i:) = ' '
  end subroutine C_F_string
end module mptools_system
!> \endcond
