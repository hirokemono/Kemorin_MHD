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
      call allocate_overlaped_ele_type(ele_2nd)
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
      use m_2nd_geometry_data
      use m_2nd_group_data
      use set_comm_tbl_type_4_IO
      use set_element_types_4_IO
      use set_node_types_4_IO
      use set_group_types_4_IO
!
!
      call copy_node_type_from_IO(node_2nd)
      call copy_ele_connect_type_from_IO(ele_2nd)
      call set_num_nod_4_each_elements_2
!
      call copy_comm_tbl_type_from_IO(comm_2nd)
!
      call set_nod_grp_type_from_IO(nod_grp_2nd)
      call set_ele_grp_type_from_IO(ele_grp_2nd)
      call set_surf_grp_type_from_IO(sf_grp_2nd)
!
      end subroutine set_2nd_mesh_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_2nd_mesh_geometry
!
      use m_2nd_geometry_data
      use m_comm_data_IO
      use m_read_boundary_data
      use set_element_types_4_IO
      use set_2nd_nod_comm_tbl_4_IO
      use set_node_types_4_IO
!
!
      call copy_node_type_from_IO(node_2nd)
      call copy_ele_connect_type_from_IO(ele_2nd)
      call set_num_nod_4_each_elements_2
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
      use m_2nd_geometry_data
      use m_2nd_group_data
      use set_comm_tbl_type_4_IO
      use set_element_types_4_IO
      use set_group_types_4_IO
      use set_node_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_comm_tbl_type_to_IO(my_rank, comm_2nd)
      call copy_node_type_to_IO(node_2nd)
      call copy_ele_connect_type_to_IO(ele_2nd)
!
      call set_node_grp_type_to_IO(nod_grp_2nd)
      call set_ele_grp_type_to_IO(ele_grp_2nd)
      call set_surface_grp_type_to_IO(sf_grp_2nd)
!
      end subroutine set_2nd_mesh_to_IO
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_2nd_mesh
!
      use m_2nd_geometry_data
      use m_2nd_group_data
!
!
      call deallocate_sf_grp_type_num(sf_grp_2nd)
      call deallocate_sf_grp_type_item(sf_grp_2nd)
!
      call deallocate_grp_type_num(ele_grp_2nd)
      call deallocate_grp_type_item(ele_grp_2nd)
!
      call deallocate_grp_type_num(nod_grp_2nd)
      call deallocate_grp_type_item(nod_grp_2nd)
!
!
      call deallocate_ele_connect_type(ele_2nd)
      call deallocate_node_geometry_type(node_2nd)
      call deallocate_type_comm_tbl(comm_2nd)
!
      end subroutine deallocate_2nd_mesh
!
!   --------------------------------------------------------------------
!
      end module load_2nd_mesh_data
