!
!     module set_consist_mass_connect
!
      module set_consist_mass_connect
!
!     Written by H. Matsui on Oct., 2006
!
      use m_precision
!
      implicit none
!
!      subroutine s_set_consist_mass_connect
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_consist_mass_connect
!
      use m_geometry_parameter
      use m_crs_connect
      use m_crs_consist_mass_mat
!
!
      ntot_mass_l = ntot_crs_l
      ntot_mass_u = ntot_crs_u
!
      im_mass_d = 1
      im_mass_l = numnod + 1
      im_mass_u = numnod + ntot_crs_l + 1
      num_mass_mat = numnod + ntot_crs_l + ntot_crs_u
!
!
      call allocate_mass_connect
!
      istack_mass_l(0:numnod) = istack_crs_l
      istack_mass_u(0:numnod) = istack_crs_u
!
      item_mass_l(1:ntot_mass_l) = item_crs_l
      item_mass_u(1:ntot_mass_u) = item_crs_u
!
      call allocate_aiccg_mass
!
      end subroutine s_set_consist_mass_connect
!
!-----------------------------------------------------------------------
!
      end module set_consist_mass_connect
