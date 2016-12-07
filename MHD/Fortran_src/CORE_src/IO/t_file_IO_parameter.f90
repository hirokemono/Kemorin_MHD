!> @file  t_file_IO_parameter.f90
!!      module t_file_IO_parameter
!!
!! @author  H. Matsui
!! @date Programmed in June, 2013
!
!> @brief Structure for file information (prefix, format, IO flag)
!!
!!@verbatim
!!      subroutine copy_mesh_format_and_prefix(prefix, i_fmt, f_param)
!!        type(field_IO_params), intent(inout) :: f_param
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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_mesh_format_and_prefix(prefix, i_fmt, f_param)
!
      character(len=kchara), intent(in) :: prefix
      integer(kind = kint), intent(in)  :: i_fmt
      type(field_IO_params), intent(inout) :: f_param
!
!
      f_param%file_prefix =  prefix
      f_param%iflag_format = i_fmt
      f_param%iflag_IO =     i_fmt
!
      end subroutine copy_mesh_format_and_prefix
!
!  ---------------------------------------------------------------------
!
      end module t_file_IO_parameter
