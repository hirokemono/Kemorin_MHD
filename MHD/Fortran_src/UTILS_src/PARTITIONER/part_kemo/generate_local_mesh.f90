!generate_local_mesh.f90
!      module generate_local_mesh
!
!      Written by H. Matsui on Aug., 2007
!
!!      subroutine PROC_LOCAL_MESH(node_org, ele_org, edge_org,         &
!!     &          ele_grp, included_ele)
!
      module generate_local_mesh
!
      use m_precision
      use m_constants
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine PROC_LOCAL_MESH(node_org, ele_org, edge_org,       &
     &          surf_org, field_org,                                &
     &          group_org, included_ele)
!
      use t_mesh_data
      use t_near_mesh_id_4_node
      use t_geometry_data
      use t_group_data
      use t_edge_data
      use m_ctl_param_partitioner
      use m_subdomain_table_IO
!
      use check_domain_prop_4_part
      use find_local_elements
      use increase_overlap
      use const_local_mesh_by_tbl
      use check_domain_prop_4_part
      use generate_comm_tables
      use local_mesh_by_part
      use intelligent_partition
!
      type(node_data), intent(in) :: node_org
      type(element_data), intent(in) :: ele_org
      type(mesh_groups), intent(in) :: group_org
      type(edge_data), intent(in) :: edge_org
      type(near_mesh), intent(inout) :: included_ele
      type(surface_data), intent(in) :: surf_org
      type(vector_field), intent(in) :: field_org
!
      character(len=kchara), parameter :: work_file_header = 'work'
!C
!C
!C-- OVERLAPPED ELEMENTs
      call count_overlapped_ele                                         &
     &   (ele_org%numele, ele_org%nodelm(1), ele_org%ie, nod_d_grp1)
!
      call CRE_LOCAL_DATA(num_domain, node_org%numnod,                  &
     &    ele_org, nod_d_grp1, included_ele)
      call increase_overlapping(num_domain, node_org, ele_org,          &
     &    surf_org, field_org, nod_d_grp1, iflag_new_ghost_cell,        &
     &    included_ele)
!
!C
!C-- INTERFACE info.
!C
!C +---------------------------------------------+
!C | create INITIAL FILE : LOCAL        pointers |
!C +---------------------------------------------+
!C===
!C
      call s_const_local_mesh_by_tbl(node_org%numnod, ele_org,          &
     &    group_org%ele_grp, num_domain, included_ele)
      call open_partition_log                                           &
     &   (num_domain, edge_org%numedge, org_mesh_header,                &
     &    nod_d_grp1, ele_d_grp1)
!C
!C +---------------------------------------+
!C | create INITIAL FILE : IMPORT pointers |
!C +---------------------------------------+
!C===
!
      call gen_node_import_tables(num_domain, work_file_header)
!C
!C +-------------------------------+
!C | update FILE : EXPORT pointers |
!C +-------------------------------+
!C===
      call gen_node_export_tables(num_domain, work_file_header)
!C
!C-- distributed Local DATA
      call local_fem_mesh(izero, ione, work_file_header,                &
     &    node_org, ele_org, group_org)
!
      end subroutine PROC_LOCAL_MESH
!
!   --------------------------------------------------------------------
!
      end module generate_local_mesh

