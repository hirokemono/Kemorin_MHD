!load_2nd_ele_surf_edge.f90
!      module  load_2nd_ele_surf_edge
!
!     Written by H. Matsui
!
!      subroutine output_ele_surf_edge_type(my_rank, ele,               &
!     &          ele_mesh, surf_mesh, edge_mesh)
!      subroutine dealloc_ele_surf_edge_type                            &
!     &          (ele_mesh, surf_mesh, edge_mesh)
!
!      subroutine output_element_connect_type(my_rank, ele_mesh)
!      subroutine output_surface_mesh_type(my_rank, numele, surf_mesh)
!      call output_edge_mesh_type                                       &
!     &         (my_rank, numele, surf_mesh, edge_mesh)
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
      subroutine output_ele_surf_edge_type(my_rank, ele,                &
     &          ele_mesh, surf_mesh, edge_mesh)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: my_rank
!
      type(element_data), intent(in) :: ele
      type(element_comms), intent(inout) :: ele_mesh
      type(surface_geometry), intent(inout) :: surf_mesh
      type(edge_geometry), intent(inout) ::    edge_mesh
!
!
      call output_element_connect_type(my_rank, ele_mesh)
      call output_surface_mesh_type(my_rank, ele%numele, surf_mesh)
      call output_edge_mesh_type                                        &
     &         (my_rank,  ele%numele, surf_mesh, edge_mesh)
!
      call dealloc_ele_surf_edge_type                                   &
     &         (ele_mesh, surf_mesh, edge_mesh)
!
      end subroutine  output_ele_surf_edge_type
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_ele_surf_edge_type                             &
     &          (ele_mesh, surf_mesh, edge_mesh)
!
      use t_mesh_data
!
      type(element_comms), intent(inout) :: ele_mesh
      type(surface_geometry), intent(inout) :: surf_mesh
      type(edge_geometry), intent(inout) ::    edge_mesh
!
!
      call deallocate_type_comm_tbl(ele_mesh%ele_comm)
!
      call deallocate_type_comm_tbl(surf_mesh%surf_comm)
      call deallocate_surface_connect_type(surf_mesh%surf)
!
      call deallocate_type_comm_tbl(edge_mesh%edge_comm)
      call deallocate_edge_connect_type(edge_mesh%edge)
      call deallocate_edge_4_ele_type(edge_mesh%edge)
!
      end subroutine  dealloc_ele_surf_edge_type
!
!   --------------------------------------------------------------------
!
      subroutine output_element_connect_type(my_rank, ele_mesh)
!
      use t_mesh_data
      use set_comm_tbl_type_4_IO
      use element_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
      type(element_comms), intent(in) :: ele_mesh
!
!
      call copy_comm_tbl_type_to_IO(my_rank, ele_mesh%ele_comm)
      call sel_output_ele_comm_table(my_rank)
!
      end subroutine output_element_connect_type
!
!   --------------------------------------------------------------------
!
      subroutine output_surface_mesh_type(my_rank, numele, surf_mesh)
!
      use t_mesh_data
      use set_comm_tbl_type_4_IO
      use set_surface_geom_type_IO
      use surface_IO_select
!
      integer(kind = kint), intent(in) :: my_rank, numele
      type(surface_geometry), intent(in) :: surf_mesh
!
!
      call copy_comm_tbl_type_to_IO(my_rank, surf_mesh%surf_comm)
      call copy_surf_conn_type_to_IO(surf_mesh%surf, numele)
!
      call sel_output_surface_connect(my_rank)
!
      end subroutine output_surface_mesh_type
!
!   --------------------------------------------------------------------
!
      subroutine output_edge_mesh_type                                  &
     &         (my_rank, numele, surf_mesh, edge_mesh)
!
      use t_mesh_data
      use set_comm_tbl_type_4_IO
      use set_edge_geom_type_IO
      use edge_IO_select
!
      integer(kind = kint), intent(in) :: my_rank, numele
      type(surface_geometry), intent(in) :: surf_mesh
      type(edge_geometry), intent(in) ::    edge_mesh
!
!
!
      call copy_comm_tbl_type_to_IO(my_rank, edge_mesh%edge_comm)
      call copy_edge_conn_type_to_IO                                    &
     &   (edge_mesh%edge, numele, surf_mesh%surf%numsurf)
!
      call sel_output_edge_connect(my_rank)
!
      end subroutine output_edge_mesh_type
!
!   --------------------------------------------------------------------
!
      end module load_2nd_ele_surf_edge
