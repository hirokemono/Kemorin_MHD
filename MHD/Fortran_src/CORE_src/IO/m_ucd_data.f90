!>@file   m_ucd_data.f90
!!@brief  module m_ucd_data
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO
!!
!!@verbatim
!!      subroutine set_control_ucd_file_def
!!      subroutine set_ucd_file_prefix(file_prefix)
!!      subroutine set_ucd_file_format(ifile_format)
!!@endverbatim
!
      module m_ucd_data
!
      use m_precision
      use m_field_file_format
      use m_file_format_switch
!
      use t_ucd_data
!
      implicit none
!
!>        Instance for FEM field data IO
      type(ucd_data), save :: fem_ucd
!
!>        Instance for numbers of FEM mesh for merged IO
      type(merged_ucd_data), save :: merged_ucd
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_ucd_file_def
!
      use ucd_IO_select
!
      call set_ucd_file_define(fem_ucd)
!
      end subroutine set_control_ucd_file_def
!
! -----------------------------------------------------------------------
!
      subroutine set_ucd_file_prefix(file_prefix)
!
      character(len=kchara), intent(in) :: file_prefix
!
      fem_ucd%file_prefix = file_prefix
!
      end subroutine set_ucd_file_prefix
!
! -----------------------------------------------------------------------
!
      subroutine set_ucd_file_format(ifile_format)
!
      integer(kind = kint), intent(in) :: ifile_format
!
      fem_ucd%ifmt_file = ifile_format
!
      end subroutine set_ucd_file_format
!
! -----------------------------------------------------------------------
!
      end module m_ucd_data
