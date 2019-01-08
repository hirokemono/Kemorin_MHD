!const_local_mesh_by_tbl.f90
!      module const_local_mesh_by_tbl
!
!      Written by H. Matsui on Sep., 2007
!
!!      subroutine s_const_local_mesh_by_tbl(numnod, ele, ele_grp,      &
!!     &          n_domain, domain_grp, included_ele)
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: ele_grp
!!        type(domain_groups_4_partitioner), intent(inout) :: domain_grp
!!        type(near_mesh), intent(inout) :: included_ele
!!
!!      subroutine const_local_node_by_near_tbl                         &
!!     &         (ele, n_domain, intnod_s_domin, nod_d_grp)
!!        type(element_data), intent(in) :: ele
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!!      subroutine const_local_ele_by_near_tbl(numnod, numele,          &
!!     &          n_domain, ele_grp, ele_d_grp, included_ele)
!!        type(group_data), intent(in) :: ele_grp
!!        type(domain_group_4_partition), intent(in) :: ele_d_grp
!!        type(near_mesh), intent(inout) :: included_ele
!!      subroutine const_local_surf_by_near_tbl                         &
!!     &         (ele, surf, n_domain, surf_d_grp)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(domain_group_4_partition), intent(inout) :: surf_d_grp
!!      subroutine const_local_edge_by_near_tbl                         &
!!     &         (ele, edge, n_domain, edge_d_grp)
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(domain_group_4_partition), intent(inout) :: edge_d_grp
!
      module const_local_mesh_by_tbl
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_domain_group_4_partition
      use cal_minmax_and_stacks
      use set_internals_by_group_tbl
      use set_subdomain_by_group_tbl
!
      implicit none
!
      private :: const_local_node_by_near_tbl
      private :: const_local_ele_by_near_tbl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_const_local_mesh_by_tbl(numnod, ele, ele_grp,        &
     &          n_domain, domain_grp, included_ele)
!
      use t_group_data
      use t_near_mesh_id_4_node
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: n_domain
!
      type(domain_groups_4_partitioner), intent(inout) :: domain_grp
      type(near_mesh), intent(inout) :: included_ele
!
!
      call const_local_ele_by_near_tbl(numnod, ele%numele, n_domain,    &
     &    ele_grp, domain_grp%ele_d_grp, included_ele)
      call const_local_node_by_near_tbl(ele, n_domain,                  &
     &    domain_grp%intnod_s_domin, domain_grp%nod_d_grp)
!
      end subroutine s_const_local_mesh_by_tbl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine const_local_node_by_near_tbl                           &
     &         (ele, n_domain, intnod_s_domin, nod_d_grp)
!
      use m_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: intnod_s_domin
      type(element_data), intent(in) :: ele
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
!
      call allocate_num_internod_4_part(n_domain)
      call count_internal_nod_by_tbl                                    &
     &   (n_domain, intnod_s_domin, nod_d_grp)
!
      call s_cal_minmax_and_stacks(n_domain, num_intnod_sub, izero,     &
     &    istack_intnod_sub, ntot_intnod_sub,                           &
     &    nmax_intnod_sub, nmin_intnod_sub)
!
!
      call allocate_internod_4_part
      call set_internal_nod_by_tbl                                      &
     &   (n_domain, intnod_s_domin, nod_d_grp)
!C
!C-- count INTERIOR and EXTERIOR NODEs

      call alloc_domain_group_imark(nod_d_grp)
      call count_subdomain_nod_by_tbl                                   &
     &   (ele, n_domain, nod_d_grp%num_s_domin, nod_d_grp%IGROUP,       &
     &    nod_d_grp%imark)
!
      call s_cal_minmax_and_stacks(n_domain, numnod_4_subdomain, izero, &
     &    istack_numnod_sub, ntot_numnod_sub,                           &
     &    nmax_numnod_sub, nmin_numnod_sub)
!C
!C-- define INTERIOR and EXTERIOR NODEs
!
      call allocate_inod_4_subdomain
      call set_subdomain_nod_by_tbl                                     &
     &   (ele, n_domain, nod_d_grp%num_s_domin, nod_d_grp%IGROUP,       &
     &    nod_d_grp%imark)
      call dealloc_domain_group_imark(nod_d_grp)
!
      end subroutine const_local_node_by_near_tbl
!
!   --------------------------------------------------------------------
!
      subroutine const_local_ele_by_near_tbl(numnod, numele,            &
     &          n_domain, ele_grp, ele_d_grp, included_ele)
!
      use m_internal_4_partitioner
      use t_group_data
      use t_near_mesh_id_4_node
      use ordering_by_element_group
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: n_domain
      type(group_data), intent(in) :: ele_grp
      type(domain_group_4_partition), intent(in) :: ele_d_grp
!
      type(near_mesh), intent(inout) :: included_ele
!
!
      call allocate_num_interele_4_part(n_domain)
!
      numele_4_subdomain(1:n_domain)                                    &
     &     = included_ele%num_nod(1:n_domain)
      istack_numele_sub(0:n_domain)                                     &
     &     =  included_ele%istack_nod(0:n_domain)
      nmax_numele_sub = included_ele%nmax
      nmin_numele_sub = included_ele%nmin
      ntot_numele_sub = included_ele%ntot
!
      call allocate_iele_4_subdomain
!
      call set_local_element_table(numnod, numele,                      &
     &    ele_grp, n_domain, included_ele%ntot,                         &
     &    included_ele%istack_nod, included_ele%id_near_nod)
!
      call dealloc_near_node(included_ele)
      call dealloc_num_4_near_node(included_ele)
!
      call count_internal_ele_by_tbl(n_domain, ele_d_grp)
!
      call s_cal_minmax_and_stacks(n_domain, num_intele_sub, izero,     &
     &    istack_intele_sub, ntot_intele_sub,                           &
     &    nmax_intele_sub, nmin_intele_sub)
!
      call allocate_interele_4_part
!
      call set_internal_ele_by_tbl(n_domain, ele_d_grp)
!
      end subroutine const_local_ele_by_near_tbl
!
!   --------------------------------------------------------------------
!
      subroutine const_local_surf_by_near_tbl                           &
     &         (ele, surf, n_domain, surf_d_grp)
!
      use t_surface_data
      use m_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: n_domain
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(domain_group_4_partition), intent(inout) :: surf_d_grp
!
!
      call allocate_num_intersurf_4_part(n_domain)
!C
!C-- count INTERIOR and EXTERIOR surfaces
!
      call alloc_domain_group_imark(surf_d_grp)
      call count_subdomain_surf_by_tbl(ele%numele, surf%isf_4_ele,      &
     &    n_domain, surf_d_grp%num_s_domin, surf_d_grp%imark)
!
      call s_cal_minmax_and_stacks(n_domain, numsurf_4_subdomain,       &
     &    izero, istack_numsurf_sub, ntot_numsurf_sub,                  &
     &    nmax_numsurf_sub, nmin_numsurf_sub)
!
!C
!C-- define INTERIOR and EXTERIOR surfaces
      call allocate_isurf_4_subdomain
      call set_subdomain_surf_by_tbl(ele%numele, surf%isf_4_ele,        &
     &    n_domain, surf_d_grp%num_s_domin, surf_d_grp%imark)
      call dealloc_domain_group_imark(surf_d_grp)
!C
!C-- count INTERIOR and surfaces

      call count_internal_surf_by_tbl(n_domain, surf_d_grp)
!
      call s_cal_minmax_and_stacks(n_domain, num_intsurf_sub, izero,    &
     &    istack_intsurf_sub, ntot_intsurf_sub,                         &
     &    nmax_intsurf_sub, nmin_intsurf_sub)
!C
!C-- define INTERIOR surfaces
!
      call allocate_intersurf_4_part
      call set_internal_surf_by_tbl(n_domain, surf_d_grp)
!
      end subroutine const_local_surf_by_near_tbl
!
!   --------------------------------------------------------------------
!
      subroutine const_local_edge_by_near_tbl                           &
     &         (ele, edge, n_domain, edge_d_grp)
!
      use t_edge_data
      use m_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: n_domain
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
!
      type(domain_group_4_partition), intent(inout) :: edge_d_grp
!
!
      call allocate_num_interedge_4_part(n_domain)
!C
!C-- count INTERIOR and EXTERIOR edges
!
      call alloc_domain_group_imark(edge_d_grp)
      call count_subdomain_edge_by_tbl(ele%numele, edge%iedge_4_ele,    &
     &    n_domain, edge_d_grp%num_s_domin, edge_d_grp%imark)
!
      call s_cal_minmax_and_stacks(n_domain, numedge_4_subdomain,       &
     &    izero, istack_numedge_sub, ntot_numedge_sub,                  &
     &    nmax_numedge_sub, nmin_numedge_sub)
!
!C
!C-- define INTERIOR and EXTERIOR edges
      call allocate_iedge_4_subdomain
      call set_subdomain_edge_by_tbl(ele%numele, edge%iedge_4_ele,      &
     &    n_domain, edge_d_grp%num_s_domin, edge_d_grp%imark)
      call dealloc_domain_group_imark(edge_d_grp)
!C
!C-- count INTERIOR edges

      call count_internal_edge_by_tbl(n_domain, edge_d_grp)
!
      call s_cal_minmax_and_stacks(n_domain, num_intedge_sub, izero,    &
     &    istack_intedge_sub, ntot_intedge_sub,                         &
     &    nmax_intedge_sub, nmin_intedge_sub)
!C
!C-- define INTERIOR edges
!
      call allocate_interedge_4_part
      call set_internal_edge_by_tbl(n_domain, edge_d_grp)
!
      end subroutine const_local_edge_by_near_tbl
!
!   --------------------------------------------------------------------
!
      end module const_local_mesh_by_tbl
