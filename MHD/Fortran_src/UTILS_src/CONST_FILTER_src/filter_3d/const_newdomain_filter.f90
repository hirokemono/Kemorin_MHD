!
!      module const_newdomain_filter
!
      module const_newdomain_filter
!
!      modified by H. Matsui on Apr., 2008
!
      use m_precision
!
      use calypso_mpi
      use m_geometry_data
      use m_read_mesh_data
      use m_filter_func_4_sorting
      use m_filter_coefs
      use m_comm_data_IO
      use set_parallel_file_name
      use mesh_IO_select
      use read_org_filter_coefs
      use set_node_geometry_4_IO
      use set_filters_4_new_domains
!
      implicit none
!
!      subroutine marking_used_node_4_filtering(ip2, ifile_type)
!
!      subroutine trans_filter_4_new_domains(ip2, ifile_type)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine marking_used_node_4_filtering(ip2, ifile_type)
!
      integer(kind = kint), intent(in) :: ip2, ifile_type
      integer(kind = kint) :: ip, my_rank
!
!
      call clear_imark_whole_nod
!
      do ip = 1, nprocs
        my_rank = ip - 1
!
        mesh_file_head = mesh_file_head
        call sel_read_geometry_size(my_rank)
        call deallocate_neib_domain_IO
        call copy_node_geometry_from_IO
!
        ele1%numele = numele_dummy
!
!     read filtering information
!
        call read_original_filter_coefs(ifile_type, my_rank)
!
        call nod_marking_by_filtering_data(ip2)
!
        call deallocate_whole_filter_coefs
        call deallocate_fluid_filter_coefs
!
        call deallocate_node_geometry
!
      end do
!
      end subroutine marking_used_node_4_filtering
!
!------------------------------------------------------------------
!
      subroutine trans_filter_4_new_domains(ip2, ifile_type)
!
      integer(kind = kint), intent(in) :: ip2, ifile_type
      integer(kind = kint) :: ip, my_rank, icou_st
!
!
      icou_st = 0
      do ip = 1, nprocs
        my_rank = ip - 1
!
        call sel_read_geometry_size(my_rank)
        call deallocate_neib_domain_IO
        call copy_node_geometry_from_IO
!
        ele1%numele = numele_dummy
!
!     read filtering information
!
        call read_original_filter_coefs(ifile_type, my_rank)
!
        call set_filter_for_new_each_domain(ip2, icou_st)
!
        call deallocate_whole_filter_coefs
        call deallocate_fluid_filter_coefs
!
        call deallocate_node_geometry
      end do
!
      end subroutine trans_filter_4_new_domains
!
!------------------------------------------------------------------
!
      end module const_newdomain_filter
