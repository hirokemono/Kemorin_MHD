!filters_for_newdomains.f90
!      module filters_for_newdomains
!
!      Written by H. Matsui on May, 2008
!
!!      subroutine filters_4_newdomains_para                            &
!!     &         (filtering, org_node, org_ele, newmesh)
!!      subroutine filters_4_newdomains_single                          &
!!     &         (filtering, org_node, org_ele, newmesh)
!
      module filters_for_newdomains
!
      use m_precision
!
      use m_constants
      use m_internal_4_partitioner
      use t_mesh_data
      use t_geometry_data
      use t_filtering_data
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
     &         (filtering, org_node, org_ele, newmesh)
!
      use calypso_mpi
      use m_domain_group_4_partition
!
      type(filtering_data_type), intent(inout) :: filtering
      type(node_data), intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(mesh_geometry), intent(inout) :: newmesh
!
!
      call filters_4_each_newdomain(my_rank, filtering,                 &
     &    org_node, org_ele, newmesh%node, newmesh%ele)
      call deallocate_local_nese_id_tbl
!
      end subroutine filters_4_newdomains_para
!
!  ---------------------------------------------------------------------
!
      subroutine filters_4_newdomains_single                            &
     &         (filtering, org_node, org_ele, newmesh)
!
      use m_2nd_pallalel_vector
      use m_domain_group_4_partition
!
      type(filtering_data_type), intent(inout) :: filtering
      type(node_data), intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(mesh_geometry), intent(inout) :: newmesh
!
      integer(kind = kint) :: ip2, my_rank_2nd
!
!
      do ip2 = 1, nprocs_2nd
        my_rank_2nd = ip2 - 1
        call filters_4_each_newdomain(my_rank_2nd, filtering,           &
     &      org_node, org_ele, newmesh%node, newmesh%ele)
      end do
!
      call deallocate_local_nese_id_tbl
!
      end subroutine filters_4_newdomains_single
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine filters_4_each_newdomain(my_rank2, filtering,          &
     &          org_node, org_ele, new_node, new_ele)
!
      use m_ctl_param_newdom_filter
      use m_2nd_pallalel_vector
      use m_nod_filter_comm_table
      use m_filter_func_4_sorting
      use m_new_filter_func_4_sorting
      use m_filter_file_names
      use m_filter_coefs
      use m_field_file_format
      use m_read_mesh_data
      use m_comm_data_IO
      use mesh_IO_select
      use copy_filters_4_sorting
      use const_newdomain_filter
      use set_parallel_file_name
      use filter_coefs_file_IO
      use filter_IO_for_newdomain
      use set_filter_geometry_4_IO
      use filter_coefs_file_IO_b
      use set_comm_table_4_IO
      use binary_IO
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: my_rank2
!
      type(node_data), intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(filtering_data_type), intent(inout) :: filtering
!
      type(node_data), intent(inout) :: new_node
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint) :: ip2
      integer(kind = kint):: ierr
!
!
        ip2 = my_rank2 + 1
!
        mesh_file_head = target_mesh_head
        call sel_read_geometry_size(my_rank2)
        call deallocate_node_data_dummy
        call deallocate_neib_domain_IO
!
        new_node%internal_node = nod_IO%internal_node
        new_node%numnod = nod_IO%numnod
        new_ele%numele =  ele_IO%numele
!
!
        call add_int_suffix(my_rank2, new_filter_coef_head,             &
     &      mesh_file_name)
!
        if (ifmt_3d_filter .eq. iflag_ascii) then
!          write(*,*) 'ascii mesh file: ', trim(mesh_file_name)
          open (filter_coef_code, file = mesh_file_name,                &
     &      form = 'formatted')
!          write(*,*) 'read_filter_geometry'
          call read_filter_geometry(filter_coef_code)
          close(filter_coef_code)
        else
          call open_read_binary_file(mesh_file_name, my_rank2)
          call read_filter_geometry_b
          call close_binary_file
        end if
!
!        write(*,*) 'copy_filter_comm_tbl_from_IO'
        call copy_comm_tbl_type_from_IO(filtering%comm)
!        write(*,*) 'copy_filtering_geometry_from_IO'
        call copy_filtering_geometry_from_IO
!
!        write(*,*) 'set_global_nodid_4_newfilter'
        call set_global_nodid_4_newfilter
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
     &     (ip2, ifmt_3d_filter, org_node, org_ele%numele)
!        write(*,*) 'reorder_filter_new_domain'
        call reorder_filter_new_domain
!
        call allocate_nod_ele_near_1nod                                 &
     &     (new_node%numnod, new_ele%numele)
!
        call write_new_whole_filter_coef(mesh_file_name)
        call write_new_fluid_filter_coef(mesh_file_name)
!
!
        call deallocate_nod_ele_near_1nod
        call deallocate_whole_filter_coefs
        call deallocate_fluid_filter_coefs
!
        call deallocate_globalnod_filter
        call deallocate_type_comm_tbl(filtering%comm)
!
      end subroutine filters_4_each_newdomain
!
!  ---------------------------------------------------------------------
!
      end module filters_for_newdomains
