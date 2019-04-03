!local_newdomain_filter.f90
!      module local_newdomain_filter
!
!      Written by H. Matsui on May, 2008
!
!!      subroutine local_newdomain_filter_para(newfil_p,                &
!!     &          itl_nod_part, nod_d_grp, comm_part, org_node, org_ele,&
!!     &          newmesh, fil_coef, whole_fil_sort, fluid_fil_sort)
!!      subroutine local_newdomain_filter_sngl(newfil_p, itl_nod_part,  &
!!     &          nod_d_grp, comm_part, org_node, org_ele, newmesh,     &
!!     &          fil_coef, whole_fil_sort, fluid_fil_sort)
!!        type(internal_4_partitioner), intent(inout)  :: itl_nod_part
!!        type(domain_group_4_partition), intent(inout)  :: nod_d_grp
!!        type(partitioner_comm_tables), intent(inout) :: comm_part
!!        type(node_data),    intent(inout) :: org_node
!!        type(element_data), intent(inout) :: org_ele
!!        type(mesh_geometry), intent(inout) :: newmesh
!!        type(each_filter_coef), intent(inout) :: fil_coef
!!        type(filter_func_4_sorting), intent(inout) :: whole_fil_sort
!!        type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort
!
      module local_newdomain_filter
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use set_filters_4_new_domains
      use const_new_mesh_filter
!
      use t_ctl_param_newdom_filter
      use t_file_IO_parameter
      use t_mesh_data
      use t_geometry_data
      use t_domain_group_4_partition
      use t_internal_4_partitioner
      use t_partitioner_comm_table
      use t_filter_coefs
      use t_filter_func_4_sorting
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine local_newdomain_filter_para(newfil_p,                  &
     &          itl_nod_part, nod_d_grp, comm_part, org_node, org_ele,  &
     &          newmesh, fil_coef, whole_fil_sort, fluid_fil_sort)
!
      use m_2nd_pallalel_vector
!
      use set_inod_newdomain_filter
      use generate_comm_tables
      use bcast_nodes_for_trans
!
      type(ctl_param_newdom_filter), intent(in) :: newfil_p
      type(internal_4_partitioner), intent(inout)  :: itl_nod_part
      type(domain_group_4_partition), intent(inout)  :: nod_d_grp
      type(partitioner_comm_tables), intent(inout) :: comm_part
      type(node_data),    intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(mesh_geometry), intent(inout) :: newmesh
      type(each_filter_coef), intent(inout) :: fil_coef
      type(filter_func_4_sorting), intent(inout) :: whole_fil_sort
      type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort
!
      integer(kind = kint) :: ierr, num_pe
!
!
      num_pe = int(nprocs_2nd,KIND(num_pe))
      call alloc_numbers_4_part(num_pe, itl_nod_part)
      call allocate_imark_whole_nod(nod_d_grp%num_s_domin)
!
!   set each number of node (on rank 0)
!
      if (my_rank .eq. 0) then
        itl_nod_part%ntot_sub = itl_nod_part%istack_4_subdomain(0)
        call alloc_id_4_subdomain(itl_nod_part)
!
        write(*,*) 'set_inod_4_newdomain_filter'
        call set_inod_4_newdomain_filter(newfil_p, nod_d_grp,           &
     &      org_node, org_ele, newmesh%node, itl_nod_part, fil_coef,    &
     &      whole_fil_sort, fluid_fil_sort, ierr)
        if(ierr .gt. 0) then
          call calypso_mpi_abort(ierr, 'Fileter is wrong!!')
        end if
!
!    construct communication table
!
        call gen_node_import_tables                                     &
     &     (nprocs_2nd, itl_nod_part, nod_d_grp, comm_part)
        call gen_node_export_tables                                     &
     &     (nprocs_2nd, itl_nod_part, nod_d_grp, comm_part)
      end if
!
      call bcast_num_filter_part_table(int(nprocs_2nd), itl_nod_part)
!
      if (my_rank .ne. 0) call alloc_id_4_subdomain(itl_nod_part)
      call alloc_internal_4_part(itl_nod_part)
!
      call bcast_xx_whole_nod(nod_d_grp%num_s_domin, itl_nod_part)
!
      write(*,*) 'const_mesh_each_filter_domain', my_rank
      call const_mesh_each_filter_domain                                &
     &   (my_rank, newfil_p%new_filter_coef_head,                       &
     &    itl_nod_part, newmesh%nod_comm, comm_part)
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
      subroutine local_newdomain_filter_sngl(newfil_p, itl_nod_part,    &
     &          nod_d_grp, comm_part, org_node, org_ele, newmesh,       &
     &          fil_coef, whole_fil_sort, fluid_fil_sort)
!
      use set_inod_newdomain_filter
      use generate_comm_tables
!
      type(ctl_param_newdom_filter), intent(in) :: newfil_p
      type(internal_4_partitioner), intent(inout)  :: itl_nod_part
      type(domain_group_4_partition), intent(inout)  :: nod_d_grp
      type(partitioner_comm_tables), intent(inout) :: comm_part
      type(node_data),    intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(mesh_geometry), intent(inout) :: newmesh
      type(each_filter_coef), intent(inout) :: fil_coef
      type(filter_func_4_sorting), intent(inout) :: whole_fil_sort
      type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort
!
      integer(kind = kint) :: ierr, num_pe
!
!
      num_pe = int(nprocs_2nd,KIND(num_pe))
      call alloc_numbers_4_part(num_pe, itl_nod_part)
      call allocate_imark_whole_nod(nod_d_grp%num_s_domin)
!
      itl_nod_part%ntot_sub = itl_nod_part%istack_4_subdomain(0)
      call alloc_id_4_subdomain(itl_nod_part)
!
!      write(*,*) 'set_inod_4_newdomain_filter'
      call set_inod_4_newdomain_filter(newfil_p, nod_d_grp,             &
     &    org_node, org_ele, newmesh%node, itl_nod_part, fil_coef,      &
     &    whole_fil_sort, fluid_fil_sort, ierr)
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Fileter is wrong!!')
      end if
!
!     construct communication table
!
      call gen_node_import_tables                                       &
     &   (nprocs_2nd, itl_nod_part, nod_d_grp, comm_part)
      call gen_node_export_tables                                       &
     &   (nprocs_2nd, itl_nod_part, nod_d_grp, comm_part)
!
      call alloc_internal_4_part(itl_nod_part)
!
      write(*,*) 'const_mesh_newdomain_filter'
      call const_mesh_newdomain_filter(newfil_p%new_filter_coef_head,   &
     &    itl_nod_part, newmesh%nod_comm, comm_part)
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
