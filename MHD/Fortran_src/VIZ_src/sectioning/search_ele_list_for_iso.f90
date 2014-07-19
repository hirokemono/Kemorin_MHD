!search_ele_list_for_iso.f90
!      module search_ele_list_for_iso
!
!      Written by H. Matsui on June, 2006
!
!      subroutine set_search_mesh_list_4_iso                            &
!     &         (numnod, numele, numsurf, numedge, nnod_4_edge,         &
!     &          ie_edge, isf_4_ele, iedge_4_sf, interior_ele,          &
!     &          inod_smp_stack, iele_smp_stack,                        &
!     &          isurf_smp_stack, iedge_smp_stack, num_ele_grp,         &
!     &          ntot_ele_grp, istack_ele_grp, item_ele_grp)
!
      module search_ele_list_for_iso
!
      use m_precision
!
      use m_machine_parameter
      use m_control_params_4_iso
!
      implicit none
!
      private :: set_searched_element_list_4_iso
      private :: set_searched_surface_list_4_iso
      private :: set_searched_edge_list_4_iso
      private :: set_searched_node_list_4_iso
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_search_mesh_list_4_iso                             &
     &         (numnod, numele, numsurf, numedge, nnod_4_edge,          &
     &          ie_edge, isf_4_ele, iedge_4_sf, interior_ele,           &
     &          inod_smp_stack, iele_smp_stack,                         &
     &          isurf_smp_stack, iedge_smp_stack, num_ele_grp,          &
     &          ntot_ele_grp, istack_ele_grp, item_ele_grp)
!
      use m_geometry_constants
!
      integer(kind=kint), intent(in) :: numnod, numele
      integer(kind=kint), intent(in) :: numsurf, numedge
      integer(kind=kint), intent(in) :: nnod_4_edge
      integer(kind=kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind=kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind=kint), intent(in) :: iedge_4_sf(numsurf,nedge_4_surf)
      integer(kind=kint), intent(in) :: interior_ele(numele)
!
      integer(kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: isurf_smp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: iedge_smp_stack(0:np_smp)
!
      integer(kind=kint), intent(in) :: num_ele_grp, ntot_ele_grp
      integer(kind=kint), intent(in) :: istack_ele_grp(0:num_ele_grp)
      integer(kind=kint), intent(in) :: item_ele_grp(ntot_ele_grp)
!
!
      call set_searched_element_list_4_iso(numele, interior_ele,        &
     &          iele_smp_stack, num_ele_grp, ntot_ele_grp,              &
     &          istack_ele_grp, item_ele_grp)
!
      call set_searched_surface_list_4_iso(numele, numsurf,             &
     &          isf_4_ele, isurf_smp_stack)
!
      call set_searched_edge_list_4_iso(numsurf, numedge,               &
     &          iedge_4_sf, iedge_smp_stack)
!
      call set_searched_node_list_4_iso(numnod, numedge,                &
     &          nnod_4_edge, ie_edge, inod_smp_stack)
!
      end subroutine set_search_mesh_list_4_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_searched_element_list_4_iso(numele, interior_ele,  &
     &          iele_smp_stack, num_ele_grp, ntot_ele_grp,              &
     &          istack_ele_grp, item_ele_grp)
!
      use set_element_list_for_psf
      use m_iso_data
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: interior_ele(numele)
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: num_ele_grp, ntot_ele_grp
      integer(kind = kint), intent(in) :: istack_ele_grp(0:num_ele_grp)
      integer(kind = kint), intent(in) :: item_ele_grp(ntot_ele_grp)
!
      integer(kind = kint) :: i, ist, ist_smp
!
!
      call allocate_work_4_mark_psf(numele)
!
      do i = 1, num_iso
        call alloc_num_psf_search_list(np_smp, iso_search(i)%elem_list)
!
        ist = istack_grp_area_iso(i-1) + 1
        call mark_element_list_4_psf(numele, interior_ele,              &
     &      num_ele_grp, ntot_ele_grp, istack_ele_grp, item_ele_grp,    &
     &      nele_grp_area_iso(i), id_ele_grp_area_iso(ist) )
        call count_element_list_4_psf                                   &
     &     (iele_smp_stack, iso_search(i)%elem_list)
!
        call alloc_psf_search_list(iso_search(i)%elem_list)
!
        call mark_element_list_4_psf(numele, interior_ele,              &
     &      num_ele_grp, ntot_ele_grp, istack_ele_grp, item_ele_grp,    &
     &      nele_grp_area_iso(i), id_ele_grp_area_iso(ist) )
        call set_element_list_4_psf                                     &
     &     (iele_smp_stack, iso_search(i)%elem_list)
      end do
!
      call deallocate_work_4_mark_psf
!
      end subroutine set_searched_element_list_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine set_searched_surface_list_4_iso(numele, numsurf,       &
     &          isf_4_ele, isurf_smp_stack)
!
      use m_geometry_constants
      use m_iso_data
      use set_surface_list_for_psf
!
      integer(kind = kint), intent(in) :: numele, numsurf
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: isurf_smp_stack(0:np_smp)
!
      integer(kind = kint) :: i, ist_smp
!
!
      call allocate_work_4_mark_surf_psf(numsurf)
!
      do i = 1, num_iso
        call alloc_num_psf_search_list(np_smp, iso_search(i)%surf_list)
!
        ist_smp = (i-1)*np_smp
        call mark_surface_list_4_psf(numele, numsurf, isf_4_ele,        &
     &      iso_search(i)%elem_list)
        call count_surf_list_4_psf                                      &
     &      (isurf_smp_stack, iso_search(i)%surf_list)
!
        call alloc_psf_search_list(iso_search(i)%surf_list)
        call set_surface_list_4_psf                                     &
     &      (isurf_smp_stack, iso_search(i)%surf_list)
      end do
!
      call deallocate_work_4_mark_surf_psf
!
      end subroutine set_searched_surface_list_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine set_searched_edge_list_4_iso(numsurf, numedge,         &
     &          iedge_4_sf, iedge_smp_stack)
!
      use m_geometry_constants
      use m_iso_data
      use set_edge_list_for_psf
!
      integer(kind = kint), intent(in) :: numsurf, numedge
      integer(kind = kint), intent(in)                                  &
     &              :: iedge_4_sf(numsurf,nedge_4_surf)
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
!
      integer(kind = kint) :: i, ist_smp
!
!
      call allocate_work_4_mark_edge_psf(numedge)
!
      do i = 1, num_iso
        call alloc_num_psf_search_list(np_smp, iso_search(i)%edge_list)
        iso_search(i)%edge_list%istack_search_smp(0) = 0
!
        ist_smp = (i-1)*np_smp
        call mark_edge_list_4_psf(numsurf, numedge, iedge_4_sf,         &
     &      iso_search(i)%surf_list)
        call count_edge_list_4_psf                                      &
     &     (iedge_smp_stack, iso_search(i)%edge_list)
!
        call alloc_psf_search_list(iso_search(i)%edge_list)
        call set_edge_list_4_psf                                        &
     &     (iedge_smp_stack, iso_search(i)%edge_list)
      end do
!
      call deallocate_work_4_mark_edge_psf
!
      end subroutine set_searched_edge_list_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine set_searched_node_list_4_iso(numnod, numedge,          &
     &          nnod_4_edge, ie_edge, inod_smp_stack)
!
      use m_geometry_constants
      use m_iso_data
      use set_node_list_for_psf
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      integer(kind = kint) :: i, ist_smp
!
!
      call allocate_work_4_mark_node_psf(numnod)
!
      do i = 1, num_iso
        call alloc_num_psf_search_list(np_smp, iso_search(i)%node_list)
!
        ist_smp = (i-1)*np_smp
        call mark_node_list_4_psf(numnod, numedge, nnod_4_edge,         &
     &      ie_edge, iso_search(i)%edge_list)
        call count_node_list_4_psf                                      &
     &     (inod_smp_stack,  iso_search(i)%node_list)
!
        call alloc_psf_search_list(iso_search(i)%node_list)
        call set_node_list_4_psf                                        &
     &     (inod_smp_stack, iso_search(i)%node_list)
      end do
!
      call deallocate_work_4_mark_node_psf
!
      end subroutine set_searched_node_list_4_iso
!
!  ---------------------------------------------------------------------
!
      end module search_ele_list_for_iso
