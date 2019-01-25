!const_local_mesh_by_tbl.f90
!      module const_local_mesh_by_tbl
!
!      Written by H. Matsui on Sep., 2007
!
!!      subroutine s_const_local_mesh_by_tbl                            &
!!     &         (part_p, numnod, ele, ele_grp, n_domain,               &
!!     &          itl_nod_part, itl_ele_part, domain_grp, included_ele)
!!      subroutine const_local_mesh_sf_ed_by_tbl                        &
!!     &         (part_p, numnod, ele, surf, edge, ele_grp, n_domain,   &
!!     &          internals_part, domain_grp, included_ele)
!!
!!      subroutine dealloc_nod_ele_4_subdomain(internals_part)
!!      subroutine dealloc_inter_sfed_subdomain(internals_part)
!!        type(ctl_param_partitioner), intent(in) :: part_p
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(group_data), intent(in) :: ele_grp
!!        type(internal_4_partitioner), intent(inout) :: itl_nod_part
!!        type(internal_4_partitioner), intent(inout) :: itl_ele_part
!!        type(domain_groups_4_partitioner), intent(inout) :: domain_grp
!!        type(near_mesh), intent(inout) :: included_ele
!
      module const_local_mesh_by_tbl
!
      use m_precision
      use m_constants
      use m_geometry_constants
!
      use t_ctl_param_partitioner
      use t_geometry_data
      use t_domain_group_4_partition
      use t_internal_4_partitioner
      use cal_minmax_and_stacks
      use set_internals_by_group_tbl
      use set_subdomain_by_group_tbl
!
      implicit none
!
      private :: const_local_node_by_near_tbl
      private :: const_local_ele_by_near_tbl
      private :: const_local_surf_by_near_tbl
      private :: const_local_edge_by_near_tbl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_const_local_mesh_by_tbl                              &
     &         (part_p, numnod, ele, ele_grp, n_domain,                 &
     &          internals_part, domain_grp, included_ele)
!
      use t_group_data
      use t_near_mesh_id_4_node
!
      type(ctl_param_partitioner), intent(in) :: part_p
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: n_domain
!
      type(internals_4_part), intent(inout) :: internals_part
      type(domain_groups_4_partitioner), intent(inout) :: domain_grp
      type(near_mesh), intent(inout) :: included_ele
!
!
      call const_local_ele_by_near_tbl                                  &
     &   (part_p, numnod, ele%numele, n_domain, ele_grp,                &
     &    domain_grp%ele_d_grp, internals_part%itl_ele_part,            &
     &    included_ele)
      call const_local_node_by_near_tbl(ele, n_domain,                  &
     &    domain_grp%intnod_s_domin, internals_part%itl_ele_part,       &
     &    internals_part%itl_nod_part, domain_grp%nod_d_grp)
!
      end subroutine s_const_local_mesh_by_tbl
!
!   --------------------------------------------------------------------
!
      subroutine const_local_mesh_sf_ed_by_tbl                          &
     &         (part_p, numnod, ele, surf, edge, ele_grp, n_domain,     &
     &          internals_part, domain_grp, included_ele)
!
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_near_mesh_id_4_node
!
      type(ctl_param_partitioner), intent(in) :: part_p
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(group_data), intent(in) :: ele_grp
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: n_domain
!
      type(internals_4_part), intent(inout) :: internals_part
      type(domain_groups_4_partitioner), intent(inout) :: domain_grp
      type(near_mesh), intent(inout) :: included_ele
!
!
      call s_const_local_mesh_by_tbl                                    &
     &   (part_p, numnod, ele, ele_grp, n_domain,                       &
     &    internals_part, domain_grp, included_ele)
!
      call const_local_surf_by_near_tbl(ele, surf, n_domain,            &
     &    internals_part%itl_ele_part, internals_part%itl_surf_part,    &
     &    domain_grp%surf_d_grp)
      call const_local_edge_by_near_tbl(ele, edge, n_domain,            &
     &    internals_part%itl_ele_part, internals_part%itl_edge_part,    &
     &    domain_grp%edge_d_grp)
!
      end subroutine const_local_mesh_sf_ed_by_tbl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_nod_ele_4_subdomain(internals_part)
!
      type(internals_4_part), intent(inout) :: internals_part
!
!
      call dealloc_internal_4_part(internals_part%itl_ele_part)
!
      call dealloc_id_4_subdomain(internals_part%itl_ele_part)
      call dealloc_num_4_subdomain(internals_part%itl_ele_part)
!
      call dealloc_internal_4_part(internals_part%itl_nod_part)
!
      call dealloc_id_4_subdomain(internals_part%itl_nod_part)
      call dealloc_num_4_subdomain(internals_part%itl_nod_part)
!
      end subroutine dealloc_nod_ele_4_subdomain
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_inter_sfed_subdomain(internals_part)
!
      type(internals_4_part), intent(inout) :: internals_part
!
!
      call dealloc_internal_4_part(internals_part%itl_surf_part)
      call dealloc_id_4_subdomain(internals_part%itl_surf_part)
      call dealloc_num_4_subdomain(internals_part%itl_surf_part)
!
      call dealloc_internal_4_part(internals_part%itl_edge_part)
      call dealloc_id_4_subdomain(internals_part%itl_edge_part)
      call dealloc_num_4_subdomain(internals_part%itl_edge_part)
!
      end subroutine dealloc_inter_sfed_subdomain
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine const_local_node_by_near_tbl(ele, n_domain,            &
     &          intnod_s_domin, itl_ele_part, itl_nod_part, nod_d_grp)
!
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: intnod_s_domin
      type(element_data), intent(in) :: ele
      type(internal_4_partitioner), intent(in) :: itl_ele_part
!
      type(internal_4_partitioner), intent(inout) :: itl_nod_part
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
!
      call alloc_numbers_4_part(n_domain, itl_nod_part)
      call count_internal_nod_by_tbl                                    &
     &   (n_domain, intnod_s_domin, nod_d_grp, itl_nod_part)
!
      call s_cal_minmax_and_stacks                                      &
     &   (n_domain, itl_nod_part%num_inter_sub, izero,                  &
     &    itl_nod_part%istack_inter_sub, itl_nod_part%ntot_inter_sub,   &
     &    itl_nod_part%nmax_inter_sub, itl_nod_part%nmin_inter_sub)
!
!
      call alloc_internal_4_part(itl_nod_part)
      call set_internal_nod_by_tbl                                      &
     &   (n_domain, intnod_s_domin, nod_d_grp, itl_nod_part)
!C
!C-- count INTERIOR and EXTERIOR NODEs

      call alloc_domain_group_imark(nod_d_grp)
      call count_subdomain_nod_by_tbl                                   &
     &   (ele, n_domain, nod_d_grp%num_s_domin, nod_d_grp%IGROUP,       &
     &    itl_ele_part, itl_nod_part, nod_d_grp%imark)
!
      call s_cal_minmax_and_stacks                                      &
     &   (n_domain, itl_nod_part%num_4_subdomain, izero,                &
     &    itl_nod_part%istack_4_subdomain, itl_nod_part%ntot_sub,       &
     &    itl_nod_part%nmax_sub, itl_nod_part%nmin_sub)
!C
!C-- define INTERIOR and EXTERIOR NODEs
!
      call alloc_id_4_subdomain(itl_nod_part)
      call set_subdomain_nod_by_tbl                                     &
     &   (ele, n_domain, nod_d_grp%num_s_domin, nod_d_grp%IGROUP,       &
     &    itl_ele_part, itl_nod_part, nod_d_grp%imark)
      call dealloc_domain_group_imark(nod_d_grp)
!
      end subroutine const_local_node_by_near_tbl
!
!   --------------------------------------------------------------------
!
      subroutine const_local_ele_by_near_tbl                            &
     &         (part_p, numnod, numele, n_domain, ele_grp, ele_d_grp,   &
     &          itl_ele_part, included_ele)
!
      use t_group_data
      use t_near_mesh_id_4_node
      use ordering_by_element_group
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: n_domain
      type(group_data), intent(in) :: ele_grp
      type(domain_group_4_partition), intent(in) :: ele_d_grp
      type(ctl_param_partitioner), intent(in) :: part_p
!
      type(internal_4_partitioner), intent(inout) :: itl_ele_part
      type(near_mesh), intent(inout) :: included_ele
!
!
      call alloc_numbers_4_part(n_domain, itl_ele_part)
!
      itl_ele_part%num_4_subdomain(1:n_domain)                          &
     &     = included_ele%num_nod(1:n_domain)
      itl_ele_part%istack_4_subdomain(0:n_domain)                       &
     &     =  included_ele%istack_nod(0:n_domain)
      itl_ele_part%nmax_sub = included_ele%nmax
      itl_ele_part%nmin_sub = included_ele%nmin
      itl_ele_part%ntot_sub = included_ele%ntot
!
      call alloc_id_4_subdomain(itl_ele_part)
!
      call set_local_element_table                                      &
     &   (part_p%nele_grp_ordering, part_p%ele_grp_ordering,            &
     &    numnod, numele, ele_grp, n_domain, included_ele%ntot,         &
     &    included_ele%istack_nod, included_ele%id_near_nod,            &
     &    itl_ele_part%ntot_sub, itl_ele_part%id_4_subdomain)
!
      call dealloc_near_node(included_ele)
      call dealloc_num_4_near_node(included_ele)
!
      call count_internal_id_by_tbl(n_domain, ele_d_grp, itl_ele_part)
!
      call s_cal_minmax_and_stacks                                      &
     &   (n_domain, itl_ele_part%num_inter_sub, izero,                  &
     &    itl_ele_part%istack_inter_sub, itl_ele_part%ntot_inter_sub,   &
     &    itl_ele_part%nmax_inter_sub, itl_ele_part%nmin_inter_sub)
!
      call alloc_internal_4_part(itl_ele_part)
!
      call set_internal_id_by_tbl(n_domain, ele_d_grp, itl_ele_part)
!
      end subroutine const_local_ele_by_near_tbl
!
!   --------------------------------------------------------------------
!
      subroutine const_local_surf_by_near_tbl(ele, surf, n_domain,      &
     &          itl_ele_part, itl_surf_part, surf_d_grp)
!
      use t_surface_data
!
      integer(kind = kint), intent(in) :: n_domain
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(internal_4_partitioner), intent(in) :: itl_ele_part
!
      type(internal_4_partitioner), intent(inout) :: itl_surf_part
      type(domain_group_4_partition), intent(inout) :: surf_d_grp
!
!
      call alloc_numbers_4_part(n_domain, itl_surf_part)
!C
!C-- count INTERIOR and EXTERIOR surfaces
!
      call alloc_domain_group_imark(surf_d_grp)
      call count_subdomain_id_by_tbl                                    &
     &   (ele%numele, nsurf_4_ele, surf%isf_4_ele, n_domain,            &
     &    surf_d_grp%num_s_domin, itl_ele_part, itl_surf_part,          &
     &    surf_d_grp%imark)
!
      call s_cal_minmax_and_stacks                                      &
     &   (n_domain, itl_surf_part%num_4_subdomain, izero,               &
     &    itl_surf_part%istack_4_subdomain, itl_surf_part%ntot_sub,     &
     &    itl_surf_part%nmax_sub, itl_surf_part%nmin_sub)
!
!C
!C-- define INTERIOR and EXTERIOR surfaces
      call alloc_id_4_subdomain(itl_surf_part)
      call set_subdomain_id_by_tbl                                      &
     &   (ele%numele, nsurf_4_ele, surf%isf_4_ele, n_domain,            &
     &    surf_d_grp%num_s_domin, itl_ele_part, itl_surf_part,          &
     &    surf_d_grp%imark)
      call dealloc_domain_group_imark(surf_d_grp)
!C
!C-- count INTERIOR and surfaces

      call count_internal_id_by_tbl                                     &
     &   (n_domain, surf_d_grp, itl_surf_part)
!
      call s_cal_minmax_and_stacks                                      &
     &   (n_domain, itl_surf_part%num_inter_sub, izero,                 &
     &    itl_surf_part%istack_inter_sub, itl_surf_part%ntot_sub,       &
     &    itl_surf_part%nmax_inter_sub, itl_surf_part%nmin_inter_sub)
!C
!C-- define INTERIOR surfaces
!
      call alloc_internal_4_part(itl_surf_part)
      call set_internal_id_by_tbl                                       &
     &   (n_domain, surf_d_grp, itl_surf_part)
!
      end subroutine const_local_surf_by_near_tbl
!
!   --------------------------------------------------------------------
!
      subroutine const_local_edge_by_near_tbl(ele, edge, n_domain,      &
     &          itl_ele_part, itl_edge_part, edge_d_grp)
!
      use t_edge_data
!
      integer(kind = kint), intent(in) :: n_domain
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(internal_4_partitioner), intent(in) :: itl_ele_part
!
      type(internal_4_partitioner), intent(inout) :: itl_edge_part
      type(domain_group_4_partition), intent(inout) :: edge_d_grp
!
!
      call alloc_numbers_4_part(n_domain, itl_edge_part)
!C
!C-- count INTERIOR and EXTERIOR edges
!
      call alloc_domain_group_imark(edge_d_grp)
      call count_subdomain_id_by_tbl                                    &
     &   (ele%numele, nedge_4_ele, edge%iedge_4_ele, n_domain,          &
     &    edge_d_grp%num_s_domin, itl_ele_part, itl_edge_part,          &
     &    edge_d_grp%imark)
!
      call s_cal_minmax_and_stacks                                      &
     &   (n_domain, itl_edge_part%num_4_subdomain, izero,               &
     &    itl_edge_part%istack_4_subdomain, itl_edge_part%ntot_sub,     &
     &    itl_edge_part%nmax_sub, itl_edge_part%nmin_sub)
!
!C
!C-- define INTERIOR and EXTERIOR edges
      call alloc_id_4_subdomain(itl_edge_part)
      call set_subdomain_id_by_tbl                                      &
     &   (ele%numele, nedge_4_ele, edge%iedge_4_ele, n_domain,          &
     &    edge_d_grp%num_s_domin, itl_ele_part, itl_edge_part,          &
     &    edge_d_grp%imark)
      call dealloc_domain_group_imark(edge_d_grp)
!C
!C-- count INTERIOR edges
      call count_internal_id_by_tbl                                     &
     &   (n_domain, edge_d_grp, itl_edge_part)
!
      call s_cal_minmax_and_stacks                                      &
     &   (n_domain, itl_edge_part%num_inter_sub, izero,                 &
     &    itl_edge_part%istack_inter_sub, itl_edge_part%ntot_inter_sub, &
     &    itl_edge_part%nmax_inter_sub, itl_edge_part%nmin_inter_sub)
!C
!C-- define INTERIOR edges
!
      call alloc_internal_4_part(itl_edge_part)
      call set_internal_id_by_tbl                                       &
     &   (n_domain, edge_d_grp, itl_edge_part)
!
      end subroutine const_local_edge_by_near_tbl
!
!   --------------------------------------------------------------------
!
      end module const_local_mesh_by_tbl
