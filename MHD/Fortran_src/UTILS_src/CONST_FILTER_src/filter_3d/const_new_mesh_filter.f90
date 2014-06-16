!const_new_mesh_filter.f90
!      module const_new_mesh_filter
!
!     Written by H. Matsui on May., 2008
!
!      subroutine const_mesh_newdomain_filter(work_f_head, new_comm)
!      subroutine const_mesh_each_filter_domain(work_f_head, my_rank2,  &
!     &          new_comm)
!
      module const_new_mesh_filter
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine const_mesh_newdomain_filter(work_f_head, new_comm)
!
      use m_2nd_pallalel_vector
      use t_comm_table
!
      character(len=kchara), intent(in) :: work_f_head
      type(communication_table), intent(inout) :: new_comm
      integer(kind = kint) :: my_rank2
!
!     output node data with communication table
!
      do my_rank2 = 0, nprocs_2nd-1
        call const_mesh_each_filter_domain(work_f_head, my_rank2,       &
     &      new_comm)
      end do
!
      end subroutine const_mesh_newdomain_filter
!
!   --------------------------------------------------------------------
!
      subroutine const_mesh_each_filter_domain(work_f_head, my_rank2,   &
     &          new_comm)
!
      use calypso_mpi
      use m_ctl_param_newdom_filter
      use m_nod_filter_comm_table
      use m_filter_file_names
      use m_file_format_switch
      use m_internal_4_partitioner
      use m_partitioner_comm_table
      use set_parallel_file_name
      use filter_geometry_IO
      use work_comm_table_IO
      use set_filter_geometry_4_IO
      use set_filters_4_new_domains
      use sel_part_nod_comm_input
      use set_comm_tbl_type_4_IO
!
      use t_comm_table
!
      character(len=kchara), intent(in) :: work_f_head
      integer(kind = kint), intent(in) :: my_rank2
      type(communication_table), intent(inout) :: new_comm
      integer(kind = kint) :: ip2
!
!
      ip2 = my_rank2 + 1
!C
!C +--------------------------+
!C | read INITIAL LOCAL files |
!C +--------------------------+
!C===
      call load_node_comm_tbl_4_part(ip2, work_f_head, new_comm)
!C
!C +-----------------+
!C | LOCAL NUMBERING |
!C +-----------------+
!C===
      new_comm%id_neib(1:new_comm%num_neib)                             &
     &        = new_comm%id_neib(1:new_comm%num_neib) - 1
!
      nnod_filtering =     numnod_4_subdomain(ip2)
      inter_nod_3dfilter = num_intnod_sub(ip2)
      call allocate_globalnod_filter
!          write(*,*) 'set_newdomain_filtering_nod'
      call set_newdomain_filtering_nod(ip2)
!
!          write(*,*) 'copy_comm_tbl_type_to_IO(my_rank, new_comm)'
      call copy_comm_tbl_type_to_IO(my_rank, new_comm)
      call deallocate_type_comm_tbl(new_comm)
!
!          write(*,*) 'copy_filtering_geometry_to_IO'
      call copy_filtering_geometry_to_IO
      call deallocate_globalnod_filter
!
      call add_int_suffix(my_rank2, new_filter_coef_head,               &
     &    mesh_file_name)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
!
        write(*,*) 'binary mesh file: ', mesh_file_name
        open (filter_coef_code, file = mesh_file_name,                  &
     &    form = 'unformatted')
!          write(*,*) 'write_filter_geometry_b'
        call write_filter_geometry_b(filter_coef_code)
!
      else
!
        write(*,*) 'ascii mesh file: ', trim(mesh_file_name)
        open (filter_coef_code, file = mesh_file_name,                  &
     &    form = 'formatted')
!          write(*,*) 'write_filter_geometry'
        call write_filter_geometry(filter_coef_code)
!
      end if
!
      write(*,*) 'write filter file end'
      close(filter_coef_code)
!
      end subroutine const_mesh_each_filter_domain
!
!   --------------------------------------------------------------------
!
      end module const_new_mesh_filter
