!load_element_surface_edge.f90
!      module  load_element_surface_edge
!
!     Written by H. Matsui
!
!      subroutine output_ele_surf_edge_mesh(my_rank)
!
!      subroutine output_element_connect(my_rank)
!      subroutine output_surface_mesh(my_rank)
!      subroutine output_edge_mesh(my_rank)
!
      module  load_element_surface_edge
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
      subroutine output_ele_surf_edge_mesh(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call output_element_connect(my_rank)
      call output_surface_mesh(my_rank)
      call output_edge_mesh(my_rank)
!
      end subroutine  output_ele_surf_edge_mesh
!
!   --------------------------------------------------------------------
!
      subroutine output_element_connect(my_rank)
!
      use set_ele_comm_tbl_4_IO
      use element_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if (iflag_debug.gt.0) write(*,*) 'copy_ele_comm_tbl_to_IO'
      call copy_ele_comm_tbl_to_IO(my_rank)
      call sel_output_ele_comm_table(my_rank)
!
      end subroutine output_element_connect
!
!   --------------------------------------------------------------------
!
      subroutine output_surface_mesh(my_rank)
!
      use set_surf_comm_tbl_4_IO
      use set_surface_geometry_4_IO
      use surface_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if (iflag_debug.gt.0) write(*,*) 'copy_surf_comm_tbl_to_IO'
      call copy_surf_comm_tbl_to_IO(my_rank)
      if (iflag_debug.gt.0) write(*,*) 'copy_surf_connect_to_IO'
      call copy_surf_connect_to_IO
!
      call sel_output_surface_connect(my_rank)
!
      end subroutine output_surface_mesh
!
!   --------------------------------------------------------------------
!
      subroutine output_edge_mesh(my_rank)
!
      use set_edge_comm_tbl_4_IO
      use set_edge_geometry_4_IO
      use edge_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if (iflag_debug.gt.0) write(*,*) 'copy_edge_comm_tbl_to_IO'
      call copy_edge_comm_tbl_to_IO(my_rank)
      call copy_edge_connect_to_IO
!
      call sel_output_edge_connect(my_rank)
!
      end subroutine output_edge_mesh
!
!   --------------------------------------------------------------------
!
      end module load_element_surface_edge
