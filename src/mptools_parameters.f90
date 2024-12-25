module mptools_parameters
  use, intrinsic :: iso_fortran_env,  only: int64
  implicit none

  private

  ! General
  integer(int64), parameter, public :: NAME_SIZE =  64  !< Maximum string lenght of the hostname.
  integer(int64), parameter, public :: STR_SIZE  = 256  !< Maximum size of a string.
end module mptools_parameters
