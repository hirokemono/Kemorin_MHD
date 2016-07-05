!>@file   m_interpolate_table_dest.f90
!!@brief  module m_interpolate_table_dest
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in  Aug., 2006
!
!> @brief Interpolation table for target grid
!!@verbatim
!!      subroutine allocate_itp_num_dest(num_org_pe)
!!      subroutine allocate_itp_table_dest
!!
!!      subroutine deallocate_itp_num_dest
!!      subroutine deallocate_itp_table_dest
!!@endverbatim
!
!
      module m_interpolate_table_dest
!
      use m_precision
      use t_interpolate_tbl_dest
!
      implicit none
!
!> Structure of interpolation table for target grid
      type(interpolate_table_dest), save :: itp1_dest
!
      end module m_interpolate_table_dest
