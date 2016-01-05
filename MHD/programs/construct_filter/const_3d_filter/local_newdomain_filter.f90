!local_newdomain_filter.f90
!      module local_newdomain_filter
!
!      Written by H. Matsui on May, 2008
!
!      subroutine  local_newdomain_filter_para(newmesh)
!      subroutine local_newdomain_filter_sngl                           &
!     &          (org_node, org_ele, newmesh)
!
      module local_newdomain_filter
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_internal_4_partitioner
      use set_filters_4_new_domains
      use const_new_mesh_filter
!
      use t_mesh_data
      use t_geometry_data
!
      implicit none
!
      character(len=kchara), parameter :: work_file_header = 'work'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine local_newdomain_filter_para                            &
     &         (org_node, org_ele, newmesh)
!
      use m_2nd_pallalel_vector
!
      use set_inod_newdomain_filter
      use generate_comm_tables
      use bcast_nodes_for_trans
!
      type(node_data),    intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(mesh_geometry), intent(inout) :: newmesh
!
!
      call allocate_num_internod_4_part(nprocs_2nd)
      call allocate_imark_whole_nod(nnod_s_domin)
!
!   set each number of node (on rank 0)
!
      if (my_rank .eq. 0) then
        ntot_numnod_sub = istack_numnod_sub(0)
        call allocate_inod_4_subdomain
!
        write(*,*) 'set_inod_4_newdomain_filter'
        call set_inod_4_newdomain_filter                                &
     &     (org_node, org_ele, newmesh%node)
!
!    construct communication table
!
        call gen_node_import_tables                                     &
     &     (nprocs_2nd, work_file_header, newmesh%nod_comm)
        call gen_node_export_tables                                     &
     &      (nprocs_2nd, work_file_header, newmesh%nod_comm)
      end if
!
      call bcast_num_filter_part_table(nprocs_2nd)
!
      if (my_rank .ne. 0) call allocate_inod_4_subdomain
      call allocate_internod_4_part
!
      call bcast_xx_whole_nod(nnod_s_domin)
!
      write(*,*) 'const_mesh_newdomain_filter', my_rank
      call const_mesh_each_filter_domain(work_file_header, my_rank,     &
     &    newmesh%nod_comm)
!
      call deallocate_internod_4_part
      call deallocate_nodes_4_subdomain
!
      call deallocate_imark_whole_nod
!
      end subroutine  local_newdomain_filter_para
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine local_newdomain_filter_sngl                            &
     &          (org_node, org_ele, newmesh)
!
      use set_inod_newdomain_filter
      use generate_comm_tables
!
      type(node_data),    intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(mesh_geometry), intent(inout) :: newmesh
!
!
      call allocate_num_internod_4_part(nprocs_2nd)
      call allocate_imark_whole_nod(nnod_s_domin)
!
      ntot_numnod_sub = istack_numnod_sub(0)
      call allocate_inod_4_subdomain
!
!      write(*,*) 'set_inod_4_newdomain_filter'
      call set_inod_4_newdomain_filter(org_node, org_ele, newmesh%node)
!
!     construct communication table
!
      call gen_node_import_tables                                       &
    &     (nprocs_2nd, work_file_header, newmesh%nod_comm)
      call gen_node_export_tables                                       &
    &     (nprocs_2nd, work_file_header, newmesh%nod_comm)
!
      call allocate_internod_4_part
!
      write(*,*) 'const_mesh_newdomain_filter'
      call const_mesh_newdomain_filter                                  &
     &   (work_file_header, newmesh%nod_comm)
!
      call deallocate_internod_4_part
      call deallocate_nodes_4_subdomain
!
      call deallocate_imark_whole_nod
!
      end subroutine  local_newdomain_filter_sngl
!
!   --------------------------------------------------------------------
!
      end module local_newdomain_filter
