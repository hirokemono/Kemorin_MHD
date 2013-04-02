!filters_for_newdomains.f90
!      module filters_for_newdomains
!
!      Written by H. Matsui on May, 2008
!
!      subroutine filters_4_newdomains_para
!      subroutine filters_4_newdomains_single
!
      module filters_for_newdomains
!
      use m_precision
!
      use m_constants
      use m_internal_4_partitioner
      use set_filters_4_new_domains
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine filters_4_newdomains_para
!
      use m_parallel_var_dof
      use m_domain_group_4_partition
!
!
      call filters_4_each_newdomain(my_rank)
      call deallocate_local_nese_id_tbl
!
      end subroutine filters_4_newdomains_para
!
!  ---------------------------------------------------------------------
!
      subroutine filters_4_newdomains_single
!
      use m_2nd_pallalel_vector
      use m_domain_group_4_partition
!
      integer(kind = kint) :: ip2, my_rank_2nd
!
!
      do ip2 = 1, nprocs_2nd
        my_rank_2nd = ip2 - 1
        call filters_4_each_newdomain(my_rank_2nd)
      end do
!
      call deallocate_local_nese_id_tbl
!
      end subroutine filters_4_newdomains_single
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine filters_4_each_newdomain(my_rank2)
!
      use m_ctl_param_newdom_filter
      use m_2nd_pallalel_vector
      use m_2nd_geometry_param
      use m_nod_filter_comm_table
      use m_filter_func_4_sorting
      use m_new_filter_func_4_sorting
      use m_filter_file_names
      use m_filter_coefs
      use m_read_mesh_data
      use m_comm_data_IO
      use mesh_IO_select
      use copy_filters_4_sorting
      use set_filter_comm_tbl_4_IO
      use const_newdomain_filter
      use set_parallel_file_name
      use filter_moment_data_IO
      use filter_moment_data_IO_b
      use filter_geometry_IO
      use filter_IO_for_newdomain
      use set_filter_geometry_4_IO
      use filter_moments_file_IO
!
      integer(kind = kint), intent(in) :: my_rank2
      integer(kind = kint) :: ip2
!
!
        ip2 = my_rank2 + 1
!
        mesh_file_head = target_mesh_head
        call sel_read_geometry_size(my_rank2)
        call deallocate_node_data_dummy
        call deallocate_neib_domain_IO
!
        internal_nod_2nd = internal_node_dummy
        nnod_2nd = numnod_dummy
        nele_2nd = numele_dummy
!
!
        call add_int_suffix(my_rank2, new_filter_coef_head,             &
     &      mesh_file_name)
        if (ifile_type .eq. 1) then
!
!          write(*,*) 'binary mesh file: ', trim(mesh_file_name)
          open (filter_coef_code, file = mesh_file_name,                &
     &      form = 'unformatted')
          call read_filter_geometry_b(filter_coef_code)
!
        else
!
!          write(*,*) 'ascii mesh file: ', trim(mesh_file_name)
          open (filter_coef_code, file = mesh_file_name,                &
     &      form = 'formatted')
!          write(*,*) 'read_filter_geometry'
          call read_filter_geometry(filter_coef_code)
!
        end if
!
!        write(*,*) 'copy_filter_comm_tbl_from_IO'
        call copy_filter_comm_tbl_from_IO
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
        call trans_filter_4_new_domains(ip2, ifile_type)
!        write(*,*) 'reorder_filter_new_domain'
        call reorder_filter_new_domain
!
        call allocate_nod_ele_near_1nod(nnod_2nd, nele_2nd)
!
        call write_new_whole_filter_coef
        call write_new_fluid_filter_coef
!
        close(filter_coef_code)
!
        call deallocate_nod_ele_near_1nod
        call deallocate_whole_filter_coefs
        call deallocate_fluid_filter_coefs
!
        call deallocate_globalnod_filter
        call deallocate_filter_import_item
        call deallocate_filter_export_item
        call deallocate_neib_filter_id
!
      end subroutine filters_4_each_newdomain
!
!  ---------------------------------------------------------------------
!
      end module filters_for_newdomains
