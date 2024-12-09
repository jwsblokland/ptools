module mptools_parameters
  use, intrinsic :: iso_fortran_env, only: int32, int64
  implicit none

  private

  ! General
  integer(int64), parameter, public :: NAME_SIZE   =  64  !< Maximum string lenght of the hostname.
  integer(int64), parameter, public :: STR_SIZE    = 256  !< Maximum size of a string.
  integer(int32), parameter, public :: MAX_THREADS = 512  !< Maximum number of threads.
  
end module mptools_parameters
