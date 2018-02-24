!
!      module m_geometry_data_4_merge
!
!      Written by H. Matsui
!      Modified by H. Matsui on Apr., 2012
!
      module m_geometry_data_4_merge
!
      use m_precision
!
      use m_constants
      use t_mesh_data_4_merge
!
      implicit    none
!
!  ==============================
! . for mesh data & result data
!  ==============================
!
      type(merged_mesh), save :: mgd_mesh1
!
      type(second_mesh), save :: sec_mesh1
!
      end module m_geometry_data_4_merge
