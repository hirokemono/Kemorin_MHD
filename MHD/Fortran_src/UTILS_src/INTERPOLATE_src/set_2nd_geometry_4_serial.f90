!
!      module set_2nd_geometry_4_serial
!
!      Written by H. Matsui on Feb., 2007
!
!!      subroutine s_set_2nd_geometry_4_serial(mesh_file, sec_mesh)
!!        type(field_IO_params), intent(in) :: mesh_file
!!        type(second_mesh), intent(inout) :: sec_mesh
!
      module set_2nd_geometry_4_serial
!
      use m_precision
!
      use m_file_format_switch
      use t_file_IO_parameter
!
      implicit none
!
      private :: set_2nd_mesh_for_single
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_2nd_geometry_4_serial(mesh_file, sec_mesh)
!
      use t_mesh_data_4_merge
      use count_number_with_overlap
!
      type(field_IO_params), intent(in) :: mesh_file
      type(second_mesh), intent(inout) :: sec_mesh
!
!
      call alloc_number_of_2nd_mesh(sec_mesh)
!
      call set_2nd_mesh_for_single                                      &
     &   (mesh_file, sec_mesh%num_pe2, sec_mesh%subdomains_2)
!
      call count_num_overlap_geom_type(sec_mesh%num_pe2,                &
     &    sec_mesh%subdomains_2, sec_mesh%merge_tbl_2)
!
      call alloc_2nd_merge_table(sec_mesh%merge_tbl_2)
!
      end subroutine s_set_2nd_geometry_4_serial
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_2nd_mesh_for_single                                &
     &         (mesh_file, num_pe2, subdomains_2)
!
       use t_mesh_data
       use mesh_IO_select
       use set_element_data_4_IO
       use copy_mesh_structures
!
      type(field_IO_params), intent(in) :: mesh_file
!
      integer(kind = kint), intent(in) :: num_pe2
      type(mesh_geometry), intent(inout) :: subdomains_2(num_pe2)
!
      type(mesh_geometry) :: mesh_IO_2
      integer (kind = kint) :: ip, my_rank, ierr
!
!
      do ip = 1, num_pe2
        my_rank = ip - 1
!
        call sel_read_mesh_geometry                                     &
     &     (mesh_file, my_rank, mesh_IO_2, ierr)
        if(ierr .gt. 0) stop 'Error in Mesh data'
!
        call copy_node_geometry_types                                   &
     &     (mesh_IO_2%node, subdomains_2(ip)%node)
        call copy_ele_connect_from_IO                                   &
     &     (mesh_IO_2%ele, subdomains_2(ip)%ele)
!
        call alloc_sph_node_geometry(subdomains_2(ip)%node)
!
        call dealloc_mesh_geometry_base(mesh_IO_2)
      end do
!
      end subroutine set_2nd_mesh_for_single
!
!  ---------------------------------------------------------------------
!
      end module set_2nd_geometry_4_serial
