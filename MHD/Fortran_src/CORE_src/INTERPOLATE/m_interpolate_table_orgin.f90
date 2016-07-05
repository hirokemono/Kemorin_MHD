!>@file   m_interpolate_table_orgin.f90
!!@brief  module m_interpolate_table_orgin
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in  Aug., 2006
!
!> @brief Interpolation table for source grid
!!@verbatim
!!      subroutine allocate_itp_num_org(np_smp, num_dest_pe)
!!      subroutine allocate_itp_table_org
!!
!!      subroutine set_stack_tbl_wtype_org_smp
!!@endverbatim
!
      module m_interpolate_table_orgin
!
      use m_precision
      use t_interpolate_tbl_org
!
      implicit none
!
!> Structure of interpolation table for source grid
      type(interpolate_table_org) :: itp1_org
!
      end module m_interpolate_table_orgin
