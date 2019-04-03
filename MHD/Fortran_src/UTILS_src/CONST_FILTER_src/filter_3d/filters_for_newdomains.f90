!filters_for_newdomains.f90
!      module filters_for_newdomains
!
!      Written by H. Matsui on May, 2008
!
!!      subroutine filters_4_newdomains_para                            &
!!     &         (newfil_p, filtering, org_node, org_ele,               &
!!     &          nod_d_grp, newmesh, fil_coef, fils_sort)
!!      subroutine filters_4_newdomains_single                          &
!!     &         (newfil_p, filtering, org_node, org_ele,               &
!!     &          nod_d_grp, newmesh, fil_coef, fils_sort)
!!        type(ctl_param_newdom_filter), intent(in) :: newfil_p
!!        type(filtering_data_type), intent(inout) :: filtering
!!        type(node_data), intent(inout) :: org_node
!!        type(element_data), intent(inout) :: org_ele
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!!        type(mesh_geometry), intent(inout) :: newmesh
!!        type(filters_4_sorting), intent(inout) :: fils_sort
!
      module filters_for_newdomains
!
      use m_precision
!
      use m_constants
      use t_mesh_data
      use t_geometry_data
      use t_filtering_data
      use t_file_IO_parameter
      use t_filter_coefs
      use t_filter_func_4_sorting
      use t_ctl_param_newdom_filter
!
      use set_filters_4_new_domains
!
      implicit none
!
      private :: filters_4_each_newdomain
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine filters_4_newdomains_para                              &
     &         (newfil_p, filtering, org_node, org_ele,                 &
     &          nod_d_grp, newmesh, fil_coef, fils_sort)
!
      use calypso_mpi
      use t_domain_group_4_partition
!
      type(ctl_param_newdom_filter), intent(in) :: newfil_p
      type(filtering_data_type), intent(inout) :: filtering
      type(node_data), intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
      type(mesh_geometry), intent(inout) :: newmesh
      type(each_filter_coef), intent(inout) :: fil_coef
      type(filters_4_sorting), intent(inout) :: fils_sort
!
      integer(kind = kint) :: ierr
!
!
      call filters_4_each_newdomain(my_rank, newfil_p,                  &
     &    filtering, org_node, org_ele, nod_d_grp, newmesh%node,        &
     &    newmesh%ele, fil_coef, fils_sort, ierr)
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Mesh or filter data is wrong!!')
      end if
!
      end subroutine filters_4_newdomains_para
!
!  ---------------------------------------------------------------------
!
      subroutine filters_4_newdomains_single                            &
     &         (newfil_p, filtering, org_node, org_ele,                 &
     &          nod_d_grp, newmesh, fil_coef, fils_sort)
!
      use m_2nd_pallalel_vector
      use t_domain_group_4_partition
!
      type(ctl_param_newdom_filter), intent(in) :: newfil_p
      type(filtering_data_type), intent(inout) :: filtering
      type(node_data), intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
      type(mesh_geometry), intent(inout) :: newmesh
      type(each_filter_coef), intent(inout) :: fil_coef
      type(filters_4_sorting), intent(inout) :: fils_sort
!
      integer :: ip2, my_rank_2nd
      integer(kind = kint) ::  ierr
!
!
      do ip2 = 1, nprocs_2nd
        my_rank_2nd = ip2 - 1
        call filters_4_each_newdomain(my_rank_2nd, newfil_p,            &
     &      filtering, org_node, org_ele, nod_d_grp,                    &
     &      newmesh%node, newmesh%ele, fil_coef, fils_sort, ierr)
        if(ierr .gt. 0) stop 'Mesh or filter data is wrong!!'
      end do
!
      end subroutine filters_4_newdomains_single
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine filters_4_each_newdomain(my_rank2,                     &
     &          newfil_p, filtering, org_node, org_ele, nod_d_grp,      &
     &          new_node, new_ele, fil_coef, fils_sort, ierr)
!
      use t_filter_func_4_sorting
!
      use m_2nd_pallalel_vector
      use m_nod_filter_comm_table
      use m_filter_file_names
      use m_field_file_format
!
      use mesh_IO_select
      use copy_filters_4_sorting
      use const_newdomain_filter
      use set_parallel_file_name
      use filter_IO_for_newdomain
      use set_filter_geometry_4_IO
      use filter_moment_IO_select
      use filter_coefs_file_IO
      use filter_coefs_file_IO_b
      use mesh_data_IO
      use mesh_data_IO_b
      use binary_IO
!
      use t_mesh_data
      use t_filter_file_data
!
      integer, intent(in) :: my_rank2
      type(ctl_param_newdom_filter), intent(in) :: newfil_p
!
      type(node_data), intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(filtering_data_type), intent(inout) :: filtering
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      type(node_data), intent(inout) :: new_node
      type(element_data), intent(inout) :: new_ele
      type(each_filter_coef), intent(inout) :: fil_coef
      type(filters_4_sorting), intent(inout) :: fils_sort
      integer(kind = kint), intent(inout) :: ierr
!
      type(mesh_geometry) :: mesh_IO_f
      type(filter_file_data) :: filter_IO
      character(len=kchara) :: file_name
      integer(kind = kint) :: ip2
!
!
        ip2 = int(my_rank2 + 1, KIND(ip2))
!
        call sel_read_geometry_size                                     &
     &     (newfil_p%tgt_mesh_file, my_rank2, mesh_IO_f, ierr)
        if(ierr .gt. 0) return
!
        new_node%internal_node = mesh_IO_f%node%internal_node
        new_node%numnod = mesh_IO_f%node%numnod
        new_ele%numele =  mesh_IO_f%ele%numele
!
        call dealloc_node_geometry_IO(mesh_IO_f)
!
        file_name = add_process_id(my_rank2,                            &
     &                             newfil_p%new_filter_coef_head)
!
        ifmt_filter_file = ifmt_3d_filter
        filter_file_head = newfil_p%new_filter_coef_head
        call sel_read_filter_geometry_file(my_rank2, filter_IO, ierr)
        if(ierr .gt. 0) return
!
!        write(*,*) 'copy_filter_comm_tbl_from_IO'
        call copy_comm_tbl_type(filter_IO%nod_comm, filtering%comm)
        call copy_filtering_geometry_from_IO(filter_IO%node)
!
        call dealloc_filter_geometry_data(filter_IO)
!
!        write(*,*) 'set_global_nodid_4_newfilter'
        call set_global_nodid_4_newfilter(nod_d_grp)
!
!        write(*,*) 'inter_nod_3dfilter', inter_nod_3dfilter
        call alloc_whole_filter_stack2(inter_nod_3dfilter, fils_sort)
        call trans_filter_4_new_domains(ip2, ifmt_3d_filter,            &
     &      newfil_p%org_filter_coef_head, newfil_p%org_mesh_file,      &
     &      nod_d_grp, org_node, org_ele%numele, fil_coef, fils_sort)
        call reorder_filter_new_domain(fils_sort)
!
        call dealloc_whole_filter_stack2(fils_sort)
!
        call alloc_each_filter_coef(new_node%numnod, fil_coef)
        call alloc_each_ele_filter_coef(new_ele%numele, fil_coef)
!
        call write_new_whole_filter_coef                                &
     &     (file_name, fils_sort%whole_fil_sort, fil_coef)
        call write_new_fluid_filter_coef                                &
     &     (file_name, fils_sort%fluid_fil_sort, fil_coef)
!
!
        call dealloc_each_ele_filter_coef(fil_coef)
        call dealloc_each_filter_coef(fil_coef)
        call dealloc_filter_func_4_sort(fils_sort%whole_fil_sort)
        call dealloc_filter_num_4_sort(fils_sort%whole_fil_sort)
        call dealloc_filter_func_4_sort(fils_sort%fluid_fil_sort)
        call dealloc_filter_num_4_sort(fils_sort%fluid_fil_sort)
!
        call deallocate_globalnod_filter
        call dealloc_comm_table(filtering%comm)
!
      end subroutine filters_4_each_newdomain
!
!  ---------------------------------------------------------------------
!
      end module filters_for_newdomains
