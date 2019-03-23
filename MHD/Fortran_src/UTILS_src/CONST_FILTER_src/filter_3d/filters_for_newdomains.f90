!filters_for_newdomains.f90
!      module filters_for_newdomains
!
!      Written by H. Matsui on May, 2008
!
!!      subroutine filters_4_newdomains_para                            &
!!     &         (mesh_file, filtering, org_node, org_ele, nod_d_grp,   &
!!     &          newmesh, fil_coef)
!!      subroutine filters_4_newdomains_single                          &
!!     &         (mesh_file, filtering, org_node, org_ele, nod_d_grp,   &
!!     &          newmesh, fil_coef)
!!       type(field_IO_params), intent(in) :: mesh_file
!!       type(filtering_data_type), intent(inout) :: filtering
!!       type(node_data), intent(inout) :: org_node
!!       type(element_data), intent(inout) :: org_ele
!!       type(domain_group_4_partition), intent(inout) :: nod_d_grp
!!       type(mesh_geometry), intent(inout) :: newmesh
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
     &         (mesh_file, filtering, org_node, org_ele, nod_d_grp,     &
     &          newmesh, fil_coef)
!
      use calypso_mpi
      use t_domain_group_4_partition
!
      type(field_IO_params), intent(in) :: mesh_file
      type(filtering_data_type), intent(inout) :: filtering
      type(node_data), intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
      type(mesh_geometry), intent(inout) :: newmesh
      type(each_filter_coef), intent(inout) :: fil_coef
!
      integer(kind = kint) :: ierr
!
!
      call filters_4_each_newdomain(my_rank, mesh_file, filtering,      &
     &    org_node, org_ele, nod_d_grp, newmesh%node, newmesh%ele,      &
     &    fil_coef, ierr)
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Mesh or filter data is wrong!!')
      end if
!
      end subroutine filters_4_newdomains_para
!
!  ---------------------------------------------------------------------
!
      subroutine filters_4_newdomains_single                            &
     &         (mesh_file, filtering, org_node, org_ele, nod_d_grp,     &
     &          newmesh, fil_coef)
!
      use m_2nd_pallalel_vector
      use t_domain_group_4_partition
!
      type(field_IO_params), intent(in) :: mesh_file
      type(filtering_data_type), intent(inout) :: filtering
      type(node_data), intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
      type(mesh_geometry), intent(inout) :: newmesh
      type(each_filter_coef), intent(inout) :: fil_coef
!
      integer :: ip2, my_rank_2nd
      integer(kind = kint) ::  ierr
!
!
      do ip2 = 1, nprocs_2nd
        my_rank_2nd = ip2 - 1
        call filters_4_each_newdomain                                   &
     &     (my_rank_2nd, mesh_file, filtering, org_node, org_ele,       &
     &      nod_d_grp, newmesh%node, newmesh%ele, fil_coef, ierr)
        if(ierr .gt. 0) stop 'Mesh or filter data is wrong!!'
      end do
!
      end subroutine filters_4_newdomains_single
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine filters_4_each_newdomain                               &
     &         (my_rank2, mesh_file, filtering, org_node, org_ele,      &
     &          nod_d_grp, new_node, new_ele, fil_coef, ierr)
!
      use m_ctl_param_newdom_filter
      use m_2nd_pallalel_vector
      use m_nod_filter_comm_table
      use m_filter_func_4_sorting
      use m_new_filter_func_4_sorting
      use m_filter_file_names
      use m_field_file_format
      use m_filter_coefs
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
      type(field_IO_params), intent(in) :: mesh_file
!
      type(node_data), intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(filtering_data_type), intent(inout) :: filtering
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      type(node_data), intent(inout) :: new_node
      type(element_data), intent(inout) :: new_ele
      type(each_filter_coef), intent(inout) :: fil_coef
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
     &     (tgt_mesh_file, my_rank2, mesh_IO_f, ierr)
        if(ierr .gt. 0) return
!
        new_node%internal_node = mesh_IO_f%node%internal_node
        new_node%numnod = mesh_IO_f%node%numnod
        new_ele%numele =  mesh_IO_f%ele%numele
!
        call dealloc_node_geometry_IO(mesh_IO_f)
!
        file_name = add_process_id(my_rank2, new_filter_coef_head)
!
        ifmt_filter_file = ifmt_3d_filter
        filter_file_head = new_filter_coef_head
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
        intnod_w_fliter2 = inter_nod_3dfilter
        ntot_nod_near_w_filter2 = 0
        ntot_nod_near_f_filter2 = 0
        call allocate_whole_filter_stack2
        call allocate_fluid_filter_stack2
!
        call allocate_whole_filter_coefs2
        call allocate_fluid_filter_coefs2
!
!        write(*,*) 'trans_filter_4_new_domains'
        call trans_filter_4_new_domains                                 &
     &     (ip2, ifmt_3d_filter, mesh_file, nod_d_grp,                  &
     &      org_node, org_ele%numele, fil_coef)
!        write(*,*) 'reorder_filter_new_domain'
        call reorder_filter_new_domain
!
        call allocate_nod_ele_near_1nod                                 &
     &     (new_node%numnod, new_ele%numele, fil_coef)
!
        call write_new_whole_filter_coef(file_name, fil_coef)
        call write_new_fluid_filter_coef(file_name, fil_coef)
!
!
        call deallocate_nod_ele_near_1nod(fil_coef)
        call deallocate_whole_filter_coefs
        call deallocate_fluid_filter_coefs
!
        call deallocate_globalnod_filter
        call dealloc_comm_table(filtering%comm)
!
      end subroutine filters_4_each_newdomain
!
!  ---------------------------------------------------------------------
!
      end module filters_for_newdomains
