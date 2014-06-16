!generate_local_all_mesh.f90
!      module generate_local_all_mesh
!
!      Written by H. Matsui on Aug., 2007
!
!      subroutine const_communication_table(new_fem,                    &
!     &          new_ele_mesh, new_surf_mesh, new_edge_mesh)
!
      module generate_local_all_mesh
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine const_communication_table(new_fem,                     &
     &          new_ele_mesh, new_surf_mesh, new_edge_mesh)
!
      use t_mesh_data
      use m_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_ctl_param_partitioner
      use m_subdomain_table_IO
!
      use check_domain_prop_4_part
      use find_local_elements
      use increase_overlap
      use const_local_mesh_by_tbl
      use check_domain_prop_4_part
      use generate_comm_tables
      use local_data_by_part
      use const_mesh_info
!
      type(mesh_data), intent(inout) :: new_fem
!
      type(element_comms), intent(inout) ::    new_ele_mesh
      type(surface_geometry), intent(inout) :: new_surf_mesh
      type(edge_geometry), intent(inout) ::    new_edge_mesh
!
      character(len=kchara), parameter :: work_file_header = 'work'
!C
!C-- count domain boundary edge
!
      call cal_edgecut(numedge, nnod_4_edge, ie_edge)
!C
!C-- cont overlapped elements
      call count_overlapped_ele(numele, nodelm(1), ie)
!
      call CRE_LOCAL_DATA(num_domain)
      call increase_overlapping(num_domain, n_overlap, i_sleeve_ele)
!
!C
!C-- INTERFACE info.
!C
!C +---------------------------------------------+
!C | create INITIAL FILE : LOCAL        pointers |
!C +---------------------------------------------+
!C===
!C
      call const_local_mesh_surf_by_tbl(num_domain)
!C
      call open_partition_log(num_domain, numedge, org_mesh_header)
!
!C +---------------------------------------+
!C | create INITIAL FILE : IMPORT pointers |
!C +---------------------------------------+
!C===
!
      call gen_all_import_tables(num_domain, work_file_header,          &
     &    new_fem%mesh%nod_comm, new_ele_mesh%ele_comm,                 &
     &    new_surf_mesh%surf_comm, new_edge_mesh%edge_comm)
!C
!C +-------------------------------+
!C | update FILE : EXPORT pointers |
!C +-------------------------------+
!C===
      call gen_all_export_tables(num_domain, work_file_header,          &
     &    new_fem%mesh%nod_comm, new_ele_mesh%ele_comm,                 &
     &    new_surf_mesh%surf_comm, new_edge_mesh%edge_comm)
!C
!C-- distributed Local DATA
      call local_mesh_surf_edge                                         &
     &    (izero, ione, work_file_header, new_fem,                      &
     &     new_ele_mesh, new_surf_mesh, new_edge_mesh)
!
!C
!C-- Finalize
!
      call deallocate_mesh_infomations
!
      end subroutine const_communication_table
!
!   --------------------------------------------------------------------
!
      end module generate_local_all_mesh

