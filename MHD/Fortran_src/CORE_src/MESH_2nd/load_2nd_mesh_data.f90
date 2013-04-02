!
!      module load_2nd_mesh_data
!
!     Written by H. Matsui on July, 2007
!
!      subroutine input_2nd_mesh(my_rank)
!      subroutine input_2nd_mesh_geometry(my_rank)
!
!      subroutine output_2nd_mesh(my_rank)
!
      module load_2nd_mesh_data
!
      use m_precision
      use m_machine_parameter
!
      use mesh_IO_select
!
      implicit none
!
      private :: set_2nd_mesh_data, set_2nd_mesh_geometry
      private :: set_2nd_mesh_to_IO
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine input_2nd_mesh(my_rank)
!
      use m_2nd_geometry_data
!
      integer(kind = kint), intent(in) :: my_rank
!
!       set second mesh informations
!
      call sel_read_mesh(my_rank)
      call set_2nd_mesh_data
!
      call allocate_2nd_element_data
!
      end subroutine input_2nd_mesh
!
! -----------------------------------------------------------------------
!
      subroutine input_2nd_mesh_geometry(my_rank)
!
      use m_2nd_geometry_data
!
      integer(kind = kint), intent(in) :: my_rank
!
!       set second mesh informations
!
      call sel_read_mesh(my_rank)
      call set_2nd_mesh_geometry
!
      end subroutine input_2nd_mesh_geometry
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_2nd_mesh(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!       save mesh information
!
      call set_2nd_mesh_to_IO(my_rank)
      call sel_write_mesh_file(my_rank)
!
      end subroutine output_2nd_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_2nd_mesh_data
!
      use set_2nd_node_geometry_4_IO
      use set_2nd_ele_connect_4_IO
      use set_2nd_nod_comm_tbl_4_IO
      use set_2nd_group_data_from_IO
!
!
      call copy_2nd_node_geometry_from_IO
      call copy_2nd_ele_connect_from_IO
!
      call copy_2nd_node_comm_tbl_from_IO
!
      call s_set_2nd_group_data_from_IO
!
      end subroutine set_2nd_mesh_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_2nd_mesh_geometry
!
      use m_comm_data_IO
      use m_read_boundary_data
      use set_2nd_node_geometry_4_IO
      use set_2nd_ele_connect_4_IO
      use set_2nd_nod_comm_tbl_4_IO
!
!
      call copy_2nd_node_geometry_from_IO
      call copy_2nd_ele_connect_from_IO
!
      call deallocate_boundary_arrays
      call deallocate_comm_item_IO
!
      end subroutine set_2nd_mesh_geometry
!
!  ---------------------------------------------------------------------
!
      subroutine set_2nd_mesh_to_IO(my_rank)
!
      use set_2nd_nod_comm_tbl_4_IO
      use set_2nd_node_geometry_4_IO
      use set_2nd_ele_connect_4_IO
      use set_2nd_group_data_to_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_2nd_node_comm_tbl_to_IO(my_rank)
      call copy_2nd_node_geometry_to_IO
      call copy_2nd_ele_connect_to_IO
      call s_set_2nd_group_data_to_IO
!
      end subroutine set_2nd_mesh_to_IO
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_2nd_mesh
!
      use m_2nd_geometry_data
      use m_2nd_nod_comm_table
      use m_2nd_group_data
!
      call deallocate_2nd_node_group
      call deallocate_2nd_element_group
      call deallocate_2nd_surface_group
!
      call deallocate_2nd_element_connect
      call deallocate_2nd_node_position
      call deallocate_2nd_nod_export
      call deallocate_2nd_nod_import
!
      call deallocate_2nd_neib_id
!
      end subroutine deallocate_2nd_mesh
!
!   --------------------------------------------------------------------
!
      end module load_2nd_mesh_data
