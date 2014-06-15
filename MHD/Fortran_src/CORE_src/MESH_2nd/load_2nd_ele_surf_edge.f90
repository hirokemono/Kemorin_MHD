!load_2nd_ele_surf_edge.f90
!      module  load_2nd_ele_surf_edge
!
!     Written by H. Matsui
!
!      subroutine output_2nd_ele_surf_edge_mesh(my_rank)
!      subroutine dealloc_2nd_ele_surf_edge_mesh
!
!      subroutine output_2nd_element_connect(my_rank)
!      subroutine output_2nd_surface_mesh(my_rank)
!      subroutine output_2nd_edge_mesh(my_rank)
!
!      subroutine deallocate_2nd_ele_comm_table
!      subroutine deallocate_2nd_surf_data
!      subroutine deallocate_2nd_edge_data
!
      module  load_2nd_ele_surf_edge
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine output_2nd_ele_surf_edge_mesh(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call output_2nd_element_connect(my_rank)
      call output_2nd_surface_mesh(my_rank)
      call output_2nd_edge_mesh(my_rank)
!
      end subroutine  output_2nd_ele_surf_edge_mesh
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_2nd_ele_surf_edge_mesh
!
!
      call deallocate_2nd_ele_comm_table
      call deallocate_2nd_surf_data
      call deallocate_2nd_edge_data
!
      end subroutine  dealloc_2nd_ele_surf_edge_mesh
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine output_2nd_element_connect(my_rank)
!
      use set_2nd_ele_comm_tbl_4_IO
      use element_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_2nd_ele_comm_tbl_to_IO(my_rank)
      call sel_output_ele_comm_table(my_rank)
!
      end subroutine output_2nd_element_connect
!
!   --------------------------------------------------------------------
!
      subroutine output_2nd_surface_mesh(my_rank)
!
      use set_2nd_surf_comm_tbl_4_IO
      use set_2nd_surf_geometry_4_IO
      use surface_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_2nd_surf_comm_tbl_to_IO(my_rank)
      call copy_2nd_surf_connect_to_IO
!
      call sel_output_surface_connect(my_rank)
!
      end subroutine output_2nd_surface_mesh
!
!   --------------------------------------------------------------------
!
      subroutine output_2nd_edge_mesh(my_rank)
!
      use set_2nd_edge_comm_tbl_4_IO
      use set_2nd_edge_geometry_4_IO
      use edge_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_2nd_edge_comm_tbl_to_IO(my_rank)
      call copy_2nd_edge_connect_to_IO
!
      call sel_output_edge_connect(my_rank)
!
      end subroutine output_2nd_edge_mesh
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine deallocate_2nd_ele_comm_table
!
      use m_2nd_ele_comm_table
!
!
      call deallocate_2nd_ele_export
      call deallocate_2nd_ele_import
      call deallocate_2nd_ele_neib_id
!
      end subroutine deallocate_2nd_ele_comm_table
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_2nd_surf_data
!
      use m_2nd_geometry_data
!
!
      call deallocate_type_comm_tbl(surf_comm_2nd)
      call deallocate_surface_connect_type(surf_2nd)
!
      end subroutine deallocate_2nd_surf_data
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_2nd_edge_data
!
      use m_2nd_geometry_data
!
!
      call deallocate_type_comm_tbl(edge_comm_2nd)
      call deallocate_edge_connect_type(edge_2nd)
      call deallocate_edge_4_ele_type(edge_2nd)
!
      end subroutine deallocate_2nd_edge_data
!
!   --------------------------------------------------------------------
!
      end module load_2nd_ele_surf_edge
