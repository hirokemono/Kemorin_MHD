!>@file  element_file_IO.f90
!!      module element_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine output_element_file(my_rank_IO, ele_mesh_IO)
!!      subroutine output_element_sph_file(my_rank_IO, ele_mesh_IO)
!!      subroutine output_element_cyl_file(my_rank_IO, ele_mesh_IO)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!
!!      subroutine output_surface_file(my_rank_IO, surf_mesh_IO)
!!      subroutine output_surface_sph_file(my_rank_IO, surf_mesh_IO)
!!      subroutine output_surface_cyl_file(my_rank_IO, surf_mesh_IO)
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!
!!      subroutine output_edge_geometries(my_rank_IO, edge_mesh_IO)
!!      subroutine output_edge_geometries_sph(my_rank_IO, edge_mesh_IO)
!!      subroutine output_edge_geometries_cyl(my_rank_IO, edge_mesh_IO)
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!@endverbatim
!!
!!@param my_rank_IO  MPI rank
!
      module element_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use m_read_mesh_data
      use t_read_mesh_data
      use set_mesh_file_names
!
      implicit none
!
      character(len=kchara), private :: file_name
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine output_element_file(my_rank_IO, ele_mesh_IO)
!
      use element_data_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      call set_mesh_file_name(mesh_ele_file_head, id_ascii_file_fmt,    &
     &    my_rank_IO, file_name)
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Write ascii element comm file: ', trim(file_name)
!
      open(input_file_code, file = file_name, form = 'formatted')
      call write_element_comm_table                                     &
     &   (input_file_code, my_rank_IO, ele_mesh_IO%comm)
      call write_element_geometry(input_file_code,                      &
     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      close(input_file_code)
!
      end subroutine output_element_file
!
!------------------------------------------------------------------
!
      subroutine output_element_sph_file(my_rank_IO, ele_mesh_IO)
!
      use element_data_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      call set_mesh_file_name(mesh_ele_file_head, id_ascii_file_fmt,    &
     &    my_rank_IO, file_name)
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Write ascii element comm file: ', trim(file_name)
!
      open(input_file_code, file = file_name, form = 'formatted')
      call write_element_comm_table                                     &
     &   (input_file_code, my_rank_IO, ele_mesh_IO%comm)
      call write_element_geometry_sph(input_file_code,                  &
     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      close(input_file_code)
!
      end subroutine output_element_sph_file
!
!------------------------------------------------------------------
!
      subroutine output_element_cyl_file(my_rank_IO, ele_mesh_IO)
!
      use element_data_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      call set_mesh_file_name(mesh_ele_file_head, id_ascii_file_fmt,    &
     &    my_rank_IO, file_name)
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Write ascii element comm file: ', trim(file_name)
!
      open(input_file_code, file = file_name, form = 'formatted')
      call write_element_comm_table                                     &
     &   (input_file_code, my_rank_IO, ele_mesh_IO%comm)
      call write_element_geometry_cyl(input_file_code,                  &
     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      close(input_file_code)
!
      end subroutine output_element_cyl_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_surface_file(my_rank_IO, surf_mesh_IO)
!
      use surface_data_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
!
      call set_mesh_file_name(mesh_surf_file_head, id_ascii_file_fmt,   &
     &    my_rank_IO, file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
      call write_surface_connection                                     &
     &  (input_file_code, my_rank_IO, surf_mesh_IO%comm,                &
     &   surf_mesh_IO%node, surf_mesh_IO%ele, surf_mesh_IO%sfed)
      call write_surface_geometry(input_file_code,                      &
     &    surf_mesh_IO%node, surf_mesh_IO%sfed)
      close (input_file_code)
!
      end subroutine output_surface_file
!
!------------------------------------------------------------------
!
      subroutine output_surface_sph_file(my_rank_IO, surf_mesh_IO)
!
      use surface_data_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
!
      call set_mesh_file_name(mesh_surf_file_head, id_ascii_file_fmt,   &
     &    my_rank_IO, file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
      call write_surface_connection                                     &
     &  (input_file_code, my_rank_IO, surf_mesh_IO%comm,                &
     &   surf_mesh_IO%node, surf_mesh_IO%ele, surf_mesh_IO%sfed)
      call write_surface_geometry_sph(input_file_code,                  &
     &   surf_mesh_IO%node, surf_mesh_IO%sfed)
      close (input_file_code)
!
      end subroutine output_surface_sph_file
!
!------------------------------------------------------------------
!
      subroutine output_surface_cyl_file(my_rank_IO, surf_mesh_IO)
!
      use surface_data_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
!
      call set_mesh_file_name(mesh_surf_file_head, id_ascii_file_fmt,   &
     &    my_rank_IO, file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
      call write_surface_connection                                     &
     &  (input_file_code, my_rank_IO, surf_mesh_IO%comm,                &
     &   surf_mesh_IO%node, surf_mesh_IO%ele, surf_mesh_IO%sfed)
      call write_surface_geometry_cyl(input_file_code,                  &
     &    surf_mesh_IO%node, surf_mesh_IO%sfed)
      close (input_file_code)
!
      end subroutine output_surface_cyl_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_edge_geometries(my_rank_IO, edge_mesh_IO)
!
      use edge_data_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
!
      call set_mesh_file_name(mesh_edge_file_head, id_ascii_file_fmt,   &
     &    my_rank_IO, file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
      call write_edge_connection                                        &
     &  (input_file_code, my_rank_IO, edge_mesh_IO%comm,                &
     &   edge_mesh_IO%node, edge_mesh_IO%ele, edge_mesh_IO%sfed)
      call write_edge_geometry(input_file_code,                         &
     &   edge_mesh_IO%node, edge_mesh_IO%sfed)
      close (input_file_code)
!
      end subroutine output_edge_geometries
!
!------------------------------------------------------------------
!
      subroutine output_edge_geometries_sph(my_rank_IO, edge_mesh_IO)
!
      use edge_data_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
!
      call set_mesh_file_name(mesh_edge_file_head, id_ascii_file_fmt,   &
     &    my_rank_IO, file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
      call write_edge_connection                                        &
     &  (input_file_code, my_rank_IO, edge_mesh_IO%comm,                &
     &    edge_mesh_IO%node, edge_mesh_IO%ele, edge_mesh_IO%sfed)
      call write_edge_geometry_sph(input_file_code,                     &
     &    edge_mesh_IO%node, edge_mesh_IO%sfed)
      close (input_file_code)
!
      end subroutine output_edge_geometries_sph
!
!------------------------------------------------------------------
!
      subroutine output_edge_geometries_cyl(my_rank_IO, edge_mesh_IO)
!
      use edge_data_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
!
      call set_mesh_file_name(mesh_edge_file_head, id_ascii_file_fmt,   &
     &    my_rank_IO, file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
!
      call write_edge_connection                                        &
     &  (input_file_code, my_rank_IO, edge_mesh_IO%comm,                &
     &   edge_mesh_IO%node, edge_mesh_IO%ele, edge_mesh_IO%sfed)
      call write_edge_geometry_cyl(input_file_code,                     &
     &   edge_mesh_IO%node, edge_mesh_IO%sfed)
      close (input_file_code)
!
      end subroutine output_edge_geometries_cyl
!
!------------------------------------------------------------------
!
      end module element_file_IO
