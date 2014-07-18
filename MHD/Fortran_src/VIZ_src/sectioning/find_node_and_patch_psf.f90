!find_node_and_patch_psf.f90
!      module find_node_and_patch_psf
!
!      Written by H. Matsui on June, 2006
!
!      subroutine set_node_and_patch_psf(numnod, numele, numedge,       &
!     &          nnod_4_ele, nnod_4_edge, inod_global, xx,              &
!     &          ie, ie_edge, iedge_4_ele, num_surf, num_surf_bc,       &
!     &          surf_istack, surf_item, ntot_node_sf_grp,              &
!     &          inod_stack_sf_grp, inod_surf_grp)
!      subroutine set_node_and_patch_iso(numnod, numele, numedge,       &
!     &           nnod_4_ele, nnod_4_edge, inod_global, xx,             &
!     &           ie, ie_edge, iedge_4_ele,                             &
!     &           num_phys, ntot_phys, istack_ncomp, d_nod)
!
      module find_node_and_patch_psf
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_and_patch_psf(numnod, numele, numedge,        &
     &          nnod_4_ele, nnod_4_edge, inod_global, xx,               &
     &          ie, ie_edge, iedge_4_ele, num_surf, num_surf_bc,        &
     &          surf_istack, surf_item, ntot_node_sf_grp,               &
     &          inod_stack_sf_grp, inod_surf_grp)
!
      use m_geometry_constants
      use m_machine_parameter
      use m_control_params_4_psf
      use m_search_list_4_psf
      use m_geometry_list_4_psf
      use m_patch_data_psf
!
      use set_nodes_for_psf
      use set_patches_for_psf
!
      integer(kind = kint), intent(in) :: numnod, numele, numedge
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind = kint), intent(in) :: inod_global(numnod)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in)                                  &
     &                     :: iedge_4_ele(numele,nedge_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in)                                  &
     &                     :: surf_istack(0:num_surf)
      integer(kind = kint), intent(in)                                  &
     &                      :: surf_item(2,num_surf_bc)
!
      integer(kind = kint), intent(in) :: ntot_node_sf_grp
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_surf_grp(ntot_node_sf_grp)
!
!
      if (iflag_debug.eq.1)  write(*,*) 'count_nodes_4_psf'
      call count_nodes_4_psf(numnod, numedge, nnod_4_edge, ie_edge,     &
     &    num_surf, inod_stack_sf_grp)
!
      call allocate_inod_psf(num_psf)
      call allocate_position_psf
!
      if (iflag_debug.eq.1)  write(*,*) 'set_nodes_4_psf'
      call set_nodes_4_psf(numnod, numedge, nnod_4_edge, inod_global,   &
     &    xx, ie_edge, num_surf, ntot_node_sf_grp, inod_stack_sf_grp,   &
     &    inod_surf_grp)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_psf_patches'
      call count_psf_patches(numnod, numele, numedge, nnod_4_ele,       &
     &    ie, iedge_4_ele, num_surf, surf_istack)
!
      call allocate_patch_data_psf
!
      if (iflag_debug.eq.1)  write(*,*) 'set_psf_patches'
      call set_psf_patches(numele, numedge, nnod_4_ele, ie,             &
     &    iedge_4_ele, num_surf, num_surf_bc, surf_istack, surf_item)
!
!
      call allocate_dat_on_patch_psf(max_ncomp_psf_out)
!
      end subroutine set_node_and_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_and_patch_iso(numnod, numele, numedge,        &
     &           nnod_4_ele, nnod_4_edge, inod_global, xx,              &
     &           ie, ie_edge, iedge_4_ele,                              &
     &           num_phys, ntot_phys, istack_ncomp, d_nod)
!
      use m_geometry_constants
      use m_control_params_4_iso
      use m_search_list_4_iso
      use m_geometry_list_4_iso
      use m_patch_data_iso
!
      use set_nodes_for_psf
      use set_patches_for_psf
      use set_fields_for_psf
!
      integer(kind = kint), intent(in) :: numnod, numele, numedge
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind = kint), intent(in) :: inod_global(numnod)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in)                                  &
     &                      :: iedge_4_ele(numele,nedge_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: num_phys, ntot_phys
      integer(kind = kint), intent(in) :: istack_ncomp(0:num_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,ntot_phys)
!
!
      call count_nodes_4_iso(numnod, numedge, nnod_4_edge, ie_edge)
!
      call allocate_inod_iso(num_iso)
      call allocate_position_iso
!
      call set_nodes_4_iso(numnod, numedge, nnod_4_edge,                &
     &    inod_global, xx, ie_edge)
!
!
      call count_iso_patches(numnod, numele, numedge, nnod_4_ele,       &
     &    ie, iedge_4_ele)
!
      call allocate_patch_data_iso
!
      call set_iso_patches(numele, numedge, iedge_4_ele)
!
!
      call allocate_dat_on_patch_iso(max_ncomp_iso_out)
!
      call set_field_4_iso(numnod, numedge, nnod_4_edge, ie_edge,       &
     &     num_phys, ntot_phys, istack_ncomp, d_nod)
!
      end subroutine set_node_and_patch_iso
!
!  ---------------------------------------------------------------------
!
      end module find_node_and_patch_psf
