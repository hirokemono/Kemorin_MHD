!generate_local_mesh.f90
!      module generate_local_mesh
!
!      Written by H. Matsui on Aug., 2007
!
!!      subroutine PROC_LOCAL_MESH                                      &
!!     &         (part_p, node_org, ele_org, edge_org, surf_org,        &
!!     &          field_org, group_org, internals_part, domain_grp,     &
!!     &          comm_part, included_ele)
!!        type(ctl_param_partitioner), intent(in) :: part_p
!!        type(node_data), intent(in) :: node_org
!!        type(element_data), intent(in) :: ele_org
!!        type(mesh_groups), intent(in) :: group_org
!!        type(edge_data), intent(in) :: edge_org
!!        type(surface_data), intent(in) :: surf_org
!!        type(vector_field), intent(in) :: field_org
!!        type(internals_4_part), intent(inout) :: internals_part
!!        type(near_mesh), intent(inout) :: included_ele
!!        type(domain_groups_4_partitioner), intent(inout) :: domain_grp
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
      subroutine PROC_LOCAL_MESH                                        &
     &         (part_p, node_org, ele_org, edge_org, surf_org,          &
     &          field_org, group_org, internals_part, domain_grp,       &
     &          comm_part, included_ele)
!
      use t_mesh_data
      use t_near_mesh_id_4_node
      use t_geometry_data
      use t_group_data
      use t_edge_data
      use t_domain_group_4_partition
      use t_internal_4_partitioner
      use t_partitioner_comm_table
      use t_ctl_param_partitioner
!
      use m_work_time
      use m_elapsed_labels_4_PART
      use check_domain_prop_4_part
      use find_local_elements
      use increase_overlap
      use const_local_mesh_by_tbl
      use check_domain_prop_4_part
      use generate_comm_tables
      use local_mesh_by_part
      use const_local_mesh_by_tbl
      use intelligent_partition
      use delete_data_files
!
      type(ctl_param_partitioner), intent(in) :: part_p
      type(node_data), intent(in) :: node_org
      type(element_data), intent(in) :: ele_org
      type(mesh_groups), intent(in) :: group_org
      type(edge_data), intent(in) :: edge_org
      type(surface_data), intent(in) :: surf_org
      type(vector_field), intent(in) :: field_org
!
      type(internals_4_part), intent(inout) :: internals_part
      type(near_mesh), intent(inout) :: included_ele
      type(domain_groups_4_partitioner), intent(inout) :: domain_grp
      type(partitioner_comm_tables), intent(inout) :: comm_part
!
      integer :: num_pe
!C
!C
!C-- OVERLAPPED ELEMENTs
      call count_overlapped_ele                                         &
     &   (ele_org%numele, ele_org%nodelm(1), ele_org%ie,                &
     &    domain_grp%nod_d_grp)
!
      call CRE_LOCAL_DATA(part_p%num_domain, node_org%numnod,           &
     &    ele_org, domain_grp%nod_d_grp, included_ele)

      call start_elapsed_time(ist_elapsed_PART+1)
      call increase_overlapping(part_p, node_org, ele_org,              &
     &    surf_org, field_org, domain_grp%nod_d_grp, included_ele)
      call end_elapsed_time(ist_elapsed_PART+1)
!
!C
!C-- INTERFACE info.
!C
!C +---------------------------------------------+
!C | create INITIAL FILE : LOCAL        pointers |
!C +---------------------------------------------+
!C===
!C
      call s_const_local_mesh_by_tbl                                    &
     &   (part_p, node_org%numnod, ele_org, group_org%ele_grp,          &
     &    internals_part, domain_grp, included_ele)
      call open_partition_log(part_p%num_domain,                        &
     &    edge_org%numedge, part_p%global_mesh_file%file_prefix,        &
     &    internals_part%itl_nod_part, internals_part%itl_ele_part,     &
     &    domain_grp%nod_d_grp, domain_grp%ele_d_grp)
!C
!C +---------------------------------------+
!C | create INITIAL FILE : IMPORT pointers |
!C +---------------------------------------+
!C===
!
      num_pe = int(part_p%num_domain)
      call start_elapsed_time(ist_elapsed_PART+4)
      call gen_node_import_tables(num_pe, internals_part%itl_nod_part,  &
     &    domain_grp%nod_d_grp, comm_part)
!C
!C +-------------------------------+
!C | update FILE : EXPORT pointers |
!C +-------------------------------+
!C===
      call gen_node_export_tables(num_pe, internals_part%itl_nod_part,  &
     &    domain_grp%nod_d_grp, comm_part)
      call end_elapsed_time(ist_elapsed_PART+4)
!C
!C-- distributed Local DATA
      call local_fem_mesh                                               &
     &   (0, 1, part_p, node_org, ele_org, group_org, internals_part,   &
     &    domain_grp%nod_d_grp, domain_grp%ele_d_grp, comm_part)
!
      if(comm_part%iflag_memory_conserve .ne. 0) then
        call delete_parallel_files                                      &
     &     (ione, part_p%num_domain, comm_part%work_f_head)
      end if
!
      call dealloc_nod_ele_4_subdomain(internals_part)
!
      end subroutine PROC_LOCAL_MESH
!
!   --------------------------------------------------------------------
!
      end module generate_local_mesh

