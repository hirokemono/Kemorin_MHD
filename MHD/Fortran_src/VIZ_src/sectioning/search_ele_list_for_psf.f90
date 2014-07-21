!search_ele_list_for_psf.f90
!      module search_ele_list_for_psf
!
!      Written by H. Matsui on June, 2006
!
!      subroutine set_search_mesh_list_4_psf                            &
!     &       (numnod, numele, numsurf, numedge, nnod_4_edge, ie_edge,  &
!     &        isf_4_ele, iedge_4_sf, interior_ele, inod_smp_stack,     &
!     &        iele_smp_stack, isurf_smp_stack, iedge_smp_stack,        &
!     &        num_ele_grp, ntot_ele_grp, istack_ele_grp, item_ele_grp)
!
      module search_ele_list_for_psf
!
      use m_precision
!
      use m_machine_parameter
      use m_control_params_4_psf
!
      implicit none
!
      private :: set_searched_element_list_4_psf
      private :: set_searched_surface_list_4_psf
      private :: set_searched_edge_list_4_psf
      private :: set_searched_node_list_4_psf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_search_mesh_list_4_psf                             &
     &       (numnod, numele, numsurf, numedge, nnod_4_edge, ie_edge,   &
     &        isf_4_ele, iedge_4_sf, interior_ele, inod_smp_stack,      &
     &        iele_smp_stack, isurf_smp_stack, iedge_smp_stack,         &
     &        num_ele_grp, ntot_ele_grp, istack_ele_grp, item_ele_grp)
!
      use m_geometry_constants
!
      integer(kind=kint), intent(in) :: numnod, numele
      integer(kind=kint), intent(in) :: numsurf, numedge
      integer(kind=kint), intent(in) :: nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
!
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in)                                  &
     &              :: iedge_4_sf(numsurf,nedge_4_surf)
      integer(kind = kint), intent(in) :: interior_ele(numele)
!
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: isurf_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: num_ele_grp, ntot_ele_grp
      integer(kind = kint), intent(in) :: istack_ele_grp(0:num_ele_grp)
      integer(kind = kint), intent(in) :: item_ele_grp(ntot_ele_grp)
!
!
      call set_searched_element_list_4_psf(numele, interior_ele,        &
     &          iele_smp_stack, num_ele_grp, ntot_ele_grp,              &
     &          istack_ele_grp, item_ele_grp)
!
      call set_searched_surface_list_4_psf(numele, numsurf,             &
     &          isf_4_ele, isurf_smp_stack)
!
      call set_searched_edge_list_4_psf(numsurf, numedge,               &
     &          iedge_4_sf, iedge_smp_stack)
!
      call set_searched_node_list_4_psf(numnod, numedge,                &
     &          nnod_4_edge, ie_edge, inod_smp_stack)
!
      end subroutine set_search_mesh_list_4_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_searched_element_list_4_psf(numele, interior_ele,  &
     &          iele_smp_stack, num_ele_grp, ntot_ele_grp,              &
     &          istack_ele_grp, item_ele_grp)
!
      use m_psf_data
      use set_element_list_for_psf
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: interior_ele(numele)
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: num_ele_grp, ntot_ele_grp
      integer(kind = kint), intent(in) :: istack_ele_grp(0:num_ele_grp)
      integer(kind = kint), intent(in) :: item_ele_grp(ntot_ele_grp)
!
      integer(kind = kint) :: i
!
!
      call allocate_work_4_mark_psf(numele)
!
      do i = 1, num_psf
        call alloc_num_psf_search_list(np_smp, psf_search(i)%elem_list)
!
        call mark_element_list_4_psf(numele, interior_ele,              &
     &      num_ele_grp, ntot_ele_grp, istack_ele_grp, item_ele_grp,    &
     &      psf_param(i)%nele_grp_area, psf_param(i)%id_ele_grp_area)
        call count_element_list_4_psf                                   &
     &     (iele_smp_stack, psf_search(i)%elem_list)
!
        call alloc_psf_search_list(psf_search(i)%elem_list)
        call set_element_list_4_psf                                     &
     &     (iele_smp_stack, psf_search(i)%elem_list)
      end do
!
      call deallocate_work_4_mark_psf
!
      end subroutine set_searched_element_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_searched_surface_list_4_psf(numele, numsurf,       &
     &          isf_4_ele, isurf_smp_stack)
!
      use m_geometry_constants
      use m_psf_data
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
      do i = 1, num_psf
        call alloc_num_psf_search_list(np_smp, psf_search(i)%surf_list)
!
        ist_smp = (i-1)*np_smp
        call mark_surface_list_4_psf(numele, numsurf, isf_4_ele,        &
     &      psf_search(i)%elem_list)
        call count_surf_list_4_psf                                      &
     &      (isurf_smp_stack, psf_search(i)%surf_list)
!
        call alloc_psf_search_list(psf_search(i)%surf_list)
        call set_surface_list_4_psf                                     &
     &      (isurf_smp_stack, psf_search(i)%surf_list)
      end do
!
      call deallocate_work_4_mark_surf_psf
!
      end subroutine set_searched_surface_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_searched_edge_list_4_psf(numsurf, numedge,         &
     &          iedge_4_sf, iedge_smp_stack)
!
      use m_geometry_constants
      use m_psf_data
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
      do i = 1, num_psf
        call alloc_num_psf_search_list(np_smp, psf_search(i)%edge_list)
!
        ist_smp = (i-1)*np_smp
        call mark_edge_list_4_psf(numsurf, numedge, iedge_4_sf,         &
     &      psf_search(i)%surf_list)
        call count_edge_list_4_psf                                      &
     &     (iedge_smp_stack, psf_search(i)%edge_list)
!
        call alloc_psf_search_list(psf_search(i)%edge_list)
        call set_edge_list_4_psf                                        &
     &     (iedge_smp_stack, psf_search(i)%edge_list)
      end do
!
      call deallocate_work_4_mark_edge_psf
!
      end subroutine set_searched_edge_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_searched_node_list_4_psf(numnod, numedge,          &
     &          nnod_4_edge, ie_edge, inod_smp_stack)
!
      use m_psf_data
      use set_node_list_for_psf
!
      integer(kind = kint), intent(in) :: numedge, numnod, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      integer(kind = kint) :: i, ist_smp
!
!
      call allocate_work_4_mark_node_psf(numnod)
!
      do i = 1, num_psf
        call alloc_num_psf_search_list(np_smp, psf_search(i)%node_list)
!
        ist_smp = (i-1)*np_smp
        call mark_node_list_4_psf(numnod, numedge, nnod_4_edge,         &
     &      ie_edge, psf_search(i)%edge_list)
        call count_node_list_4_psf                                      &
     &     (inod_smp_stack, psf_search(i)%node_list)
!
        call alloc_psf_search_list(psf_search(i)%node_list)
        call set_node_list_4_psf                                        &
     &     (inod_smp_stack, psf_search(i)%node_list)
      end do
!
      call deallocate_work_4_mark_node_psf
!
      end subroutine set_searched_node_list_4_psf
!
!  ---------------------------------------------------------------------
!
      end module search_ele_list_for_psf
