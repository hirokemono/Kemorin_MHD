!
!      module load_mesh_type_data
!
!     Written by H. Matsui on July, 2007
!
!      subroutine input_mesh_data_type(my_rank, new_femmesh)
!      subroutine input_mesh_geometry_type(my_rank, newmesh)
!
!      subroutine output_mesh_type(my_rank)
!      subroutine output_element_connect_type(my_rank, ele_mesh)
!      subroutine output_surface_mesh_type(my_rank, numele, surf_mesh)
!      call output_edge_mesh_type                                       &
!     &         (my_rank, numele, numsurf, edge_mesh)
!
      module load_mesh_type_data
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine input_mesh_data_type(my_rank, femmesh)
!
      use t_mesh_data
      use set_mesh_types
      use mesh_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_data), intent(inout) :: femmesh
!
!       set second mesh informations
!
      call sel_read_mesh(my_rank)
      call set_mesh_data_types(femmesh)
!
      call allocate_overlaped_ele_type(femmesh%mesh%ele)
!
      end subroutine input_mesh_data_type
!
! -----------------------------------------------------------------------
!
      subroutine input_mesh_geometry_type(my_rank, mesh)
!
      use t_mesh_data
      use m_read_boundary_data
      use set_mesh_types
      use mesh_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call sel_read_mesh(my_rank)
      call set_geometry_types_data(mesh)
!
      call deallocate_boundary_arrays
!
      end subroutine input_mesh_geometry_type
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_mesh_type(my_rank, femmesh)
!
      use t_mesh_data
      use set_mesh_types
      use mesh_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_data), intent(inout) :: femmesh
!
!       save mesh information
!
      call set_mesh_data_type_to_IO(my_rank, femmesh)
      call sel_write_mesh_file(my_rank)
!
      call dealloc_mesh_data_type(femmesh)
!
      end subroutine output_mesh_type
!
! -----------------------------------------------------------------------
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
      use set_mesh_types
      use surface_IO_select
!
      integer(kind = kint), intent(in) :: my_rank, numele
      type(surface_geometry), intent(in) :: surf_mesh
!
!
      call set_surf_mesh_type_to_IO(my_rank, numele, surf_mesh)
      call sel_output_surface_connect(my_rank)
!
      end subroutine output_surface_mesh_type
!
!   --------------------------------------------------------------------
!
      subroutine output_edge_mesh_type                                  &
     &         (my_rank, numele, numsurf, edge_mesh)
!
      use t_mesh_data
      use set_mesh_types
      use edge_IO_select
!
      integer(kind = kint), intent(in) :: my_rank, numele, numsurf
      type(edge_geometry), intent(in) ::    edge_mesh
!
!
!
      call set_edge_mesh_type_to_IO                                     &
     &   (my_rank, numele, numsurf, edge_mesh)
      call sel_output_edge_connect(my_rank)
!
      end subroutine output_edge_mesh_type
!
!   --------------------------------------------------------------------
!
      end module load_mesh_type_data
