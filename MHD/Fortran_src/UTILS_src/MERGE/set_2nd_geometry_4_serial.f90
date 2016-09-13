!
!      module set_2nd_geometry_4_serial
!
      module set_2nd_geometry_4_serial
!
!      Written by H. Matsui on Feb., 2007
!
      use m_precision
!
      use m_file_format_switch
      use m_2nd_geometry_4_merge
!
      implicit none
!
      private :: set_2nd_mesh_for_single
!
!      subroutine s_set_2nd_geometry_4_serial
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_2nd_geometry_4_serial
!
      use count_number_with_overlap
!
!
      call allocate_number_of_2nd_mesh
!
      call set_2nd_mesh_for_single
!
      call count_num_overlap_geom_type(num_pe2,                         &
     &    subdomains_2, merge_tbl_2)
!
      call allocate_2nd_merge_table
!
      end subroutine s_set_2nd_geometry_4_serial
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_2nd_mesh_for_single
!
       use t_mesh_data
       use mesh_IO_select
       use set_element_data_4_IO
       use copy_mesh_structures
!
      type(mesh_geometry) :: mesh_IO_2
      integer (kind = kint) :: ip, my_rank, ierr
!
!
      do ip =1, num_pe2
        my_rank = ip - 1
!
        call sel_read_mesh_geometry(my_rank, mesh_IO_2, ierr)
        if(ierr .gt. 0) stop 'Error in Mesh data'
!
        call copy_node_geometry_types                                   &
     &     (mesh_IO_2%node, subdomains_2(ip)%node)
        call copy_ele_connect_from_IO                                   &
     &     (mesh_IO_2%ele, subdomains_2(ip)%ele)
!
        call allocate_sph_node_geometry(subdomains_2(ip)%node)
!
        call deallocate_ele_connect_type(mesh_IO_2%ele)
        call dealloc_node_geometry_base(mesh_IO_2%node)
        call deallocate_type_comm_tbl(mesh_IO_2%nod_comm)
      end do
!
      end subroutine set_2nd_mesh_for_single
!
!  ---------------------------------------------------------------------
!
      end module set_2nd_geometry_4_serial
