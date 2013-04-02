!generate_local_all_mesh.f90
!      module generate_local_all_mesh
!
!      Written by H. Matsui on Aug., 2007
!
      module generate_local_all_mesh
!
      use m_precision
!
      implicit none
!
!      subroutine PROC_LOCAL
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine PROC_LOCAL
!
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
      call gen_all_import_tables(num_domain, work_file_header)
!C
!C +-------------------------------+
!C | update FILE : EXPORT pointers |
!C +-------------------------------+
!C===
      call gen_all_export_tables(num_domain, work_file_header)
!C
!C-- distributed Local DATA
      call local_mesh_surf_edge(izero, ione, work_file_header)
!C
!C-- Finalize
!
      call deallocate_mesh_infomations
!
      end subroutine PROC_LOCAL
!
!   --------------------------------------------------------------------
!
      end module generate_local_all_mesh

