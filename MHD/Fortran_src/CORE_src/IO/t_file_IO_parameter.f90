!> @file  t_file_IO_parameter.f90
!!      module t_file_IO_parameter
!!
!! @author  H. Matsui
!! @date Programmed in June, 2013
!
!> @brief Structure for file information (prefix, format, IO flag)
!!
!!@verbatim
!!@endverbatim
!
      module t_file_IO_parameter
!
      use m_precision
!
      implicit none
!
!
!>      Structure for field data IO paramters
      type field_IO_params
!>        Output flag for spherical harmonics coefficients data
        integer(kind = kint) :: iflag_IO = 0
!>        file header for spherical harmonics coefficients data
        character(len=kchara) :: file_prefix
!>        file header for spherical harmonics coefficients data
        integer(kind = kint) ::  iflag_format = 0
      end type field_IO_params
!
      end module t_file_IO_parameter
