!generate_local_mesh.f90
!      module generate_local_mesh
!
!      Written by H. Matsui on Aug., 2007
!
!!      subroutine PROC_LOCAL_MESH(node_org, ele_org, edge_org,         &
!!     &          ele_grp, new_fem, included_ele)
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
      subroutine PROC_LOCAL_MESH(node_org, ele_org, edge_org,           &
     &          ele_grp, new_fem, included_ele)
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
      use const_mesh_info
!
      type(node_data), intent(in) :: node_org
      type(element_data), intent(in) :: ele_org
      type(edge_data), intent(in) :: edge_org
      type(group_data), intent(in) :: ele_grp
      type(mesh_data), intent(inout) :: new_fem
      type(near_mesh), intent(inout) :: included_ele
!
      character(len=kchara), parameter :: work_file_header = 'work'
!C
!C
!C-- OVERLAPPED ELEMENTs
      call count_overlapped_ele                                         &
     &   (ele_org%numele, ele_org%nodelm(1), ele_org%ie)
!
      call CRE_LOCAL_DATA(num_domain, node_org%numnod,                  &
     &    ele_org, included_ele)
      call increase_overlapping(num_domain, node_org%numnod, ele_org,   &
     &    n_overlap, i_sleeve_ele, included_ele)
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
     &    ele_grp, num_domain, included_ele)
      call open_partition_log                                           &
     &   (num_domain, edge_org%numedge, org_mesh_header)
!C
!C +---------------------------------------+
!C | create INITIAL FILE : IMPORT pointers |
!C +---------------------------------------+
!C===
!
      call gen_node_import_tables                                       &
     &    (num_domain, work_file_header, new_fem%mesh%nod_comm)
!C
!C +-------------------------------+
!C | update FILE : EXPORT pointers |
!C +-------------------------------+
!C===
      call gen_node_export_tables                                       &
     &    (num_domain, work_file_header, new_fem%mesh%nod_comm)
!C
!C-- distributed Local DATA
      call local_fem_mesh                                               &
     &    (izero, ione, work_file_header, node_org, ele_org, new_fem)
!
      end subroutine PROC_LOCAL_MESH
!
!   --------------------------------------------------------------------
!
      end module generate_local_mesh

