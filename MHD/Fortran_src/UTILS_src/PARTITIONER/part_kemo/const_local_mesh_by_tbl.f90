!const_local_mesh_by_tbl.f90
!      module const_local_mesh_by_tbl
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine s_const_local_mesh_by_tbl(n_domain)
!      subroutine const_local_mesh_surf_by_tbl(n_domain)
!
!      subroutine const_local_node_by_near_tbl(n_domain)
!      subroutine const_local_ele_by_near_tbl(n_domain)
!      subroutine const_local_surf_by_near_tbl(n_domain)
!      subroutine const_local_edge_by_near_tbl(n_domain)
!
      module const_local_mesh_by_tbl
!
      use m_precision
      use m_constants
!
      use cal_minmax_and_stacks
      use set_internals_by_group_tbl
      use set_subdomain_by_group_tbl
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_const_local_mesh_by_tbl(n_domain)
!
      integer(kind = kint), intent(in) :: n_domain
!
!
      call const_local_ele_by_near_tbl(n_domain)
      call const_local_node_by_near_tbl(n_domain)
!
      end subroutine s_const_local_mesh_by_tbl
!
!   --------------------------------------------------------------------
!
      subroutine const_local_mesh_surf_by_tbl(n_domain)
!
      integer(kind = kint), intent(in) :: n_domain
!
!
      call const_local_ele_by_near_tbl(n_domain)
      call const_local_node_by_near_tbl(n_domain)
!
      call const_local_surf_by_near_tbl(n_domain)
      call const_local_edge_by_near_tbl(n_domain)
!
      end subroutine const_local_mesh_surf_by_tbl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine const_local_node_by_near_tbl(n_domain)
!
      use m_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: n_domain
!
!
      call allocate_num_internod_4_part(n_domain)
      call count_internal_nod_by_tbl(n_domain)
!
      call s_cal_minmax_and_stacks(n_domain, num_intnod_sub, izero,     &
     &    istack_intnod_sub, ntot_intnod_sub,                           &
     &    nmax_intnod_sub, nmin_intnod_sub)
!
!
      call allocate_internod_4_part
      call set_internal_nod_by_tbl(n_domain)
!C
!C-- count INTERIOR and EXTERIOR NODEs

      call count_subdomain_nod_by_tbl(n_domain)
!
      call s_cal_minmax_and_stacks(n_domain, numnod_4_subdomain, izero, &
     &    istack_numnod_sub, ntot_numnod_sub,                           &
     &    nmax_numnod_sub, nmin_numnod_sub)
!C
!C-- define INTERIOR and EXTERIOR NODEs
!
      call allocate_inod_4_subdomain
      call set_subdomain_nod_by_tbl(n_domain)
!
      end subroutine const_local_node_by_near_tbl
!
!   --------------------------------------------------------------------
!
      subroutine const_local_ele_by_near_tbl(n_domain)
!
      use m_internal_4_partitioner
      use m_near_element_id_4_node
      use ordering_by_element_group
!
      integer(kind = kint), intent(in) :: n_domain
!
!
      call allocate_num_interele_4_part(n_domain)
!
      numele_4_subdomain(1:n_domain) = nele_near_nod(1:n_domain)
      istack_numele_sub(0:n_domain) =  iele_stack_near_nod(0:n_domain)
      nmax_numele_sub = nmax_ele_near_nod
      nmin_numele_sub = nmin_ele_near_nod
      ntot_numele_sub = ntot_ele_near_nod
!
      call allocate_iele_4_subdomain
!
      call set_local_element_table(n_domain)
!
      call deallocate_near_element
      call deallocate_num_4_near_ele
!
      call count_internal_ele_by_tbl(n_domain)
!
      call s_cal_minmax_and_stacks(n_domain, num_intele_sub, izero,     &
     &    istack_intele_sub, ntot_intele_sub,                           &
     &    nmax_intele_sub, nmin_intele_sub)
!
      call allocate_interele_4_part
!
      call set_internal_ele_by_tbl(n_domain)
!
      end subroutine const_local_ele_by_near_tbl
!
!   --------------------------------------------------------------------
!
      subroutine const_local_surf_by_near_tbl(n_domain)
!
      use m_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: n_domain
!
!
      call allocate_num_intersurf_4_part(n_domain)
!C
!C-- count INTERIOR and EXTERIOR surfaces
!
      call count_subdomain_surf_by_tbl(n_domain)
!
      call s_cal_minmax_and_stacks(n_domain, numsurf_4_subdomain,       &
     &    izero, istack_numsurf_sub, ntot_numsurf_sub,                  &
     &    nmax_numsurf_sub, nmin_numsurf_sub)
!
!C
!C-- define INTERIOR and EXTERIOR surfaces
      call allocate_isurf_4_subdomain
      call set_subdomain_surf_by_tbl(n_domain)
!C
!C-- count INTERIOR and surfaces

      call count_internal_surf_by_tbl(n_domain)
!
      call s_cal_minmax_and_stacks(n_domain, num_intsurf_sub, izero,    &
     &    istack_intsurf_sub, ntot_intsurf_sub,                         &
     &    nmax_intsurf_sub, nmin_intsurf_sub)
!C
!C-- define INTERIOR surfaces
!
      call allocate_intersurf_4_part
      call set_internal_surf_by_tbl(n_domain)
!
      end subroutine const_local_surf_by_near_tbl
!
!   --------------------------------------------------------------------
!
      subroutine const_local_edge_by_near_tbl(n_domain)
!
      use m_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: n_domain
!
!
      call allocate_num_interedge_4_part(n_domain)
!C
!C-- count INTERIOR and EXTERIOR edges
!
      call count_subdomain_edge_by_tbl(n_domain)
!
      call s_cal_minmax_and_stacks(n_domain, numedge_4_subdomain,       &
     &    izero, istack_numedge_sub, ntot_numedge_sub,                  &
     &    nmax_numedge_sub, nmin_numedge_sub)
!
!C
!C-- define INTERIOR and EXTERIOR edges
      call allocate_iedge_4_subdomain
      call set_subdomain_edge_by_tbl(n_domain)
!C
!C-- count INTERIOR edges

      call count_internal_edge_by_tbl(n_domain)
!
      call s_cal_minmax_and_stacks(n_domain, num_intedge_sub, izero,    &
     &    istack_intedge_sub, ntot_intedge_sub,                         &
     &    nmax_intedge_sub, nmin_intedge_sub)
!C
!C-- define INTERIOR edges
!
      call allocate_interedge_4_part
      call set_internal_edge_by_tbl(n_domain)
!
      end subroutine const_local_edge_by_near_tbl
!
!   --------------------------------------------------------------------
!
      end module const_local_mesh_by_tbl
