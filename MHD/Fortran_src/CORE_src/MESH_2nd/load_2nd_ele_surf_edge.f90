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
      use m_2nd_geometry_data
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
      use m_2nd_geometry_data
!
!
      call deallocate_type_comm_tbl(ele_comm_2nd)
!
      call deallocate_type_comm_tbl(surf_comm_2nd)
      call deallocate_surface_connect_type(surf_2nd)
!
      call deallocate_type_comm_tbl(edge_comm_2nd)
      call deallocate_edge_connect_type(edge_2nd)
      call deallocate_edge_4_ele_type(edge_2nd)
!
      end subroutine  dealloc_2nd_ele_surf_edge_mesh
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine output_2nd_element_connect(my_rank)
!
      use m_2nd_geometry_data
      use set_comm_tbl_type_4_IO
      use element_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_comm_tbl_type_to_IO(my_rank, ele_comm_2nd)
      call sel_output_ele_comm_table(my_rank)
!
      end subroutine output_2nd_element_connect
!
!   --------------------------------------------------------------------
!
      subroutine output_2nd_surface_mesh(my_rank)
!
      use m_2nd_geometry_data
      use set_comm_tbl_type_4_IO
      use set_surface_geom_type_IO
      use surface_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_comm_tbl_type_to_IO(my_rank, surf_comm_2nd)
      call copy_surf_conn_type_to_IO(surf_2nd, ele_2nd%numele)
!
      call sel_output_surface_connect(my_rank)
!
      end subroutine output_2nd_surface_mesh
!
!   --------------------------------------------------------------------
!
      subroutine output_2nd_edge_mesh(my_rank)
!
      use m_2nd_geometry_data
      use set_comm_tbl_type_4_IO
      use set_edge_geom_type_IO
      use edge_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_comm_tbl_type_to_IO(my_rank, edge_comm_2nd)
      call copy_edge_conn_type_to_IO                                    &
     &   (edge_2nd, ele_2nd%numele, surf_2nd%numsurf)
!
      call sel_output_edge_connect(my_rank)
!
      end subroutine output_2nd_edge_mesh
!
!   --------------------------------------------------------------------
!
      end module load_2nd_ele_surf_edge
