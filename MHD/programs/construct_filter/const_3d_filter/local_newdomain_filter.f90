!local_newdomain_filter.f90
!      module local_newdomain_filter
!
!      Written by H. Matsui on May, 2008
!
!!      subroutine local_newdomain_filter_para                          &
!!     &         (mesh_file, nod_d_grp, org_node, org_ele, newmesh)
!!      subroutine local_newdomain_filter_sngl                          &
!!     &         (mesh_file, nod_d_grp, org_node, org_ele, newmesh)
!!        type(field_IO_params), intent(in) :: mesh_file
!!        type(domain_group_4_partition), intent(in)  :: nod_d_grp
!!        type(node_data),    intent(inout) :: org_node
!!        type(element_data), intent(inout) :: org_ele
!!        type(mesh_geometry), intent(inout) :: newmesh
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
      use t_file_IO_parameter
      use t_mesh_data
      use t_geometry_data
      use t_domain_group_4_partition
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
     &         (mesh_file, nod_d_grp, org_node, org_ele, newmesh)
!
      use m_2nd_pallalel_vector
!
      use set_inod_newdomain_filter
      use generate_comm_tables
      use bcast_nodes_for_trans
!
      type(field_IO_params), intent(in) :: mesh_file
      type(domain_group_4_partition), intent(inout)  :: nod_d_grp
      type(node_data),    intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(mesh_geometry), intent(inout) :: newmesh
!
      integer(kind = kint) :: ierr
!
!
      call alloc_numbers_4_part(nprocs_2nd, itl_nod_part)
      call allocate_imark_whole_nod(nod_d_grp%num_s_domin)
!
!   set each number of node (on rank 0)
!
      if (my_rank .eq. 0) then
        itl_nod_part%ntot_sub = itl_nod_part%istack_4_subdomain(0)
        call alloc_id_4_subdomain(itl_nod_part)
!
        write(*,*) 'set_inod_4_newdomain_filter'
        call set_inod_4_newdomain_filter(mesh_file, nod_d_grp,          &
     &      org_node, org_ele, newmesh%node, ierr)
        if(ierr .gt. 0) then
          call calypso_mpi_abort(ierr, 'Fileter is wrong!!')
        end if
!
!    construct communication table
!
        call gen_node_import_tables                                     &
     &     (nprocs_2nd, work_file_header, nod_d_grp)
        call gen_node_export_tables                                     &
     &     (nprocs_2nd, work_file_header, nod_d_grp)
      end if
!
      call bcast_num_filter_part_table(nprocs_2nd)
!
      if (my_rank .ne. 0) call alloc_id_4_subdomain(itl_nod_part)
      call alloc_internal_4_part(itl_nod_part)
!
      call bcast_xx_whole_nod(nod_d_grp%num_s_domin)
!
      write(*,*) 'const_mesh_newdomain_filter', my_rank
      call const_mesh_each_filter_domain(work_file_header, my_rank,     &
     &    newmesh%nod_comm)
!
      call dealloc_internal_4_part(itl_nod_part)
      call dealloc_num_4_subdomain(itl_nod_part)
      call dealloc_id_4_subdomain(itl_nod_part)
!
      call deallocate_imark_whole_nod
!
      end subroutine  local_newdomain_filter_para
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine local_newdomain_filter_sngl                            &
     &         (mesh_file, nod_d_grp, org_node, org_ele, newmesh)
!
      use set_inod_newdomain_filter
      use generate_comm_tables
!
      type(field_IO_params), intent(in) :: mesh_file
      type(domain_group_4_partition), intent(inout)  :: nod_d_grp
      type(node_data),    intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(mesh_geometry), intent(inout) :: newmesh
!
      integer(kind = kint) :: ierr
!
!
      call alloc_numbers_4_part(nprocs_2nd, itl_nod_part)
      call allocate_imark_whole_nod(nod_d_grp%num_s_domin)
!
      itl_nod_part%ntot_sub = itl_nod_part%istack_4_subdomain(0)
      call alloc_id_4_subdomain(itl_nod_part)
!
!      write(*,*) 'set_inod_4_newdomain_filter'
      call set_inod_4_newdomain_filter                                  &
     &   (mesh_file, nod_d_grp, org_node, org_ele, newmesh%node, ierr)
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Fileter is wrong!!')
      end if
!
!     construct communication table
!
      call gen_node_import_tables                                       &
     &   (nprocs_2nd, work_file_header, nod_d_grp)
      call gen_node_export_tables                                       &
     &   (nprocs_2nd, work_file_header, nod_d_grp)
!
      call alloc_internal_4_part(itl_nod_part)
!
      write(*,*) 'const_mesh_newdomain_filter'
      call const_mesh_newdomain_filter                                  &
     &   (work_file_header, newmesh%nod_comm)
!
      call dealloc_internal_4_part(itl_nod_part)
      call dealloc_num_4_subdomain(itl_nod_part)
      call dealloc_id_4_subdomain(itl_nod_part)
!
      call deallocate_imark_whole_nod
!
      end subroutine  local_newdomain_filter_sngl
!
!   --------------------------------------------------------------------
!
      end module local_newdomain_filter
