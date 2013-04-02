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
      use m_search_list_4_psf
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
      integer(kind = kint) :: i, ist, ist_smp
!
!
      call allocate_element_num_4_psf(np_smp, num_psf)
      call allocate_work_4_mark_psf(numele)
!
      istack_ele_search_psf_s(0) = 0
      do i = 1, num_psf
!
        ist_smp = (i-1)*np_smp
        ist = istack_grp_area_psf(i-1) + 1
        call mark_element_list_4_psf(numele, interior_ele,              &
     &      num_ele_grp, ntot_ele_grp, istack_ele_grp, item_ele_grp,    &
     &      nele_grp_area_psf(i), id_ele_grp_area_psf(ist) )
        call count_element_list_4_psf(iele_smp_stack,                   &
     &      istack_ele_search_psf_s(ist_smp))
!
      end do
      nele_search_psf_tot = istack_ele_search_psf_s(num_psf*np_smp)
!
      call allocate_element_list_4_psf
!
      do i = 1, num_psf
!
        ist_smp = (i-1)*np_smp
        ist = istack_grp_area_psf(i-1) + 1
        call mark_element_list_4_psf(numele, interior_ele,              &
     &      num_ele_grp, ntot_ele_grp, istack_ele_grp, item_ele_grp,    &
     &      nele_grp_area_psf(i), id_ele_grp_area_psf(ist) )
!
        call set_element_list_4_psf(iele_smp_stack,                     &
     &      nele_search_psf_tot, istack_ele_search_psf_s(ist_smp),      &
     &      iele_search_psf)
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
      use set_surface_list_for_psf
!
      integer(kind = kint), intent(in) :: numele, numsurf
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: isurf_smp_stack(0:np_smp)
!
      integer(kind = kint) :: i, ist_smp
!
!
      call allocate_surf_num_4_psf(np_smp, num_psf)
      call allocate_work_4_mark_surf_psf(numsurf)
!
      istack_surf_search_psf_s(0) = 0
      do i = 1, num_psf
!
        ist_smp = (i-1)*np_smp
        call mark_surface_list_4_psf(numele, numsurf, isf_4_ele,        &
     &      nele_search_psf_tot, istack_ele_search_psf_s(ist_smp),      &
     &      iele_search_psf)
        call count_surf_list_4_psf(isurf_smp_stack,                     &
     &      istack_surf_search_psf_s(ist_smp))
!
      end do
      nsurf_search_psf_tot                                              &
     &            = istack_surf_search_psf_s(num_psf*np_smp)
!
      call allocate_surface_list_4_psf
!
      do i = 1, num_psf
!
        ist_smp = (i-1)*np_smp
        call mark_surface_list_4_psf(numele, numsurf, isf_4_ele,        &
     &      nele_search_psf_tot, istack_ele_search_psf_s(ist_smp),      &
     &      iele_search_psf)
        call set_surface_list_4_psf(isurf_smp_stack,                    &
     &      nsurf_search_psf_tot, istack_surf_search_psf_s(ist_smp),    &
     &      isurf_search_psf)
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
      call allocate_edge_num_4_psf(np_smp, num_psf)
      call allocate_work_4_mark_edge_psf(numedge)
!
      istack_edge_search_psf_s(0) = 0
      do i = 1, num_psf
!
        ist_smp = (i-1)*np_smp
        call mark_edge_list_4_psf(numsurf, numedge, iedge_4_sf,         &
     &      nsurf_search_psf_tot, istack_surf_search_psf_s(ist_smp),    &
     &      isurf_search_psf )
        call count_edge_list_4_psf(iedge_smp_stack,                     &
     &      istack_edge_search_psf_s(ist_smp) )
!
      end do
      nedge_search_psf_tot                                              &
     &      = istack_edge_search_psf_s(num_psf*np_smp)
!
      call allocate_edge_list_4_psf
!
      do i = 1, num_psf
!
        ist_smp = (i-1)*np_smp
        call mark_edge_list_4_psf(numsurf, numedge, iedge_4_sf,         &
     &      nsurf_search_psf_tot, istack_surf_search_psf_s(ist_smp),    &
     &      isurf_search_psf )
        call set_edge_list_4_psf(iedge_smp_stack, nedge_search_psf_tot, &
     &      istack_edge_search_psf_s(ist_smp), iedge_search_psf)
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
      use set_node_list_for_psf
!
      integer(kind = kint), intent(in) :: numedge, numnod, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      integer(kind = kint) :: i, ist_smp
!
!
      call allocate_node_num_4_psf(np_smp, num_psf)
      call allocate_work_4_mark_node_psf(numnod)
!
      istack_nod_search_psf_s(0) = 0
      do i = 1, num_psf
!
        ist_smp = (i-1)*np_smp
        call mark_node_list_4_psf(numnod, numedge, nnod_4_edge,         &
     &      ie_edge, nedge_search_psf_tot,                              &
     &      istack_edge_search_psf_s(ist_smp), iedge_search_psf)
        call count_node_list_4_psf(inod_smp_stack,                      &
     &      istack_nod_search_psf_s(ist_smp) )
      end do
      nnod_search_psf_tot = istack_nod_search_psf_s(num_psf*np_smp)
!
      call allocate_node_list_4_psf
!
      do i = 1, num_psf
!
        ist_smp = (i-1)*np_smp
        call mark_node_list_4_psf(numnod, numedge, nnod_4_edge,         &
     &      ie_edge, nedge_search_psf_tot,                              &
     &      istack_edge_search_psf_s(ist_smp), iedge_search_psf)
        call set_node_list_4_psf(inod_smp_stack, nnod_search_psf_tot,   &
     &      istack_nod_search_psf_s(ist_smp), inod_search_psf)
      end do
!
      call deallocate_work_4_mark_node_psf
!
      end subroutine set_searched_node_list_4_psf
!
!  ---------------------------------------------------------------------
!
      end module search_ele_list_for_psf
