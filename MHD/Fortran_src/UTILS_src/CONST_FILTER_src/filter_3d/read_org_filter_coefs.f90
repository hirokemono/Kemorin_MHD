!read_org_filter_coefs.f90
!      module read_org_filter_coefs
!
!      Written by H. Matsui on May, 2008
!
!      subroutine read_original_filter_coefs(ifile_type, my_rank)
!
      module read_org_filter_coefs
!
      use m_precision
!
      use m_geometry_data
      use m_ctl_param_newdom_filter
      use set_parallel_file_name
      use filter_IO_for_newdomain
      use filter_geometry_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_original_filter_coefs(ifile_type, my_rank)
!
      use m_filter_file_names
      use m_filter_coefs
      use m_read_mesh_data
      use m_comm_data_IO
!
      integer(kind = kint), intent(in) :: ifile_type
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: file_name
!
!
      call allocate_nod_ele_near_1nod(node1%numnod, ele1%numele)
!
      call add_int_suffix(my_rank, org_filter_coef_head, file_name)
!
      if (ifile_type .eq. 0) then
        write(*,*) 'ascii coefficients file name: ', trim(file_name)
        open(id_org_filter_coef, file=file_name, form='formatted')
        call read_filter_geometry(id_org_filter_coef)
!
        inter_nod_3dfilter = internal_node_dummy
        call read_filter_coef_4_newdomain(id_org_filter_coef)
!
      else if(ifile_type .eq. 1) then
        write(*,*) 'binary coefficients file name: ', trim(file_name)
        open(id_org_filter_coef, file=file_name, form='unformatted')
        call read_filter_geometry_b(id_org_filter_coef)
!
        inter_nod_3dfilter = internal_node_dummy
        call read_filter_coef_4_newdomain_b(id_org_filter_coef)
!
      end if
      close(id_org_filter_coef)
!
      call deallocate_nod_ele_near_1nod
!
      call deallocate_node_data_dummy
      call deallocate_comm_item_IO
!
      end subroutine read_original_filter_coefs
!
!  ---------------------------------------------------------------------
!
      end module read_org_filter_coefs
