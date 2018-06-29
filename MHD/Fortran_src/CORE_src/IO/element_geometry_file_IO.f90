!>@file  element_geometry_file_IO.f90
!!      module element_geometry_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine output_element_xyz_file                              &
!!     &         (my_rank_IO, file_prefix, ele_mesh_IO)
!!      subroutine output_element_sph_file                              &
!!     &         (my_rank_IO, ele_mesh_IO)
!!      subroutine output_element_cyl_file                              &
!!     &         (my_rank_IO, ele_mesh_IO)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!
!!      subroutine output_surface_xyz_file                              &
!!     &         (my_rank_IO, file_prefix, surf_mesh_IO)
!!      subroutine output_surface_sph_file                              &
!!     &         (my_rank_IO, file_prefix, surf_mesh_IO)
!!      subroutine output_surface_cyl_file                              &
!!     &         (my_rank_IO, file_prefix, surf_mesh_IO)
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!
!!      subroutine output_edge_xyz_file                                 &
!!     &         (my_rank_IO, file_prefix, edge_mesh_IO)
!!      subroutine output_edge_sph_file                                 &
!!     &         (my_rank_IO, file_prefix, edge_mesh_IO)
!!      subroutine output_edge_cyl_file                                 &
!!     &         (my_rank_IO, file_prefix, edge_mesh_IO)
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!@endverbatim
!!
!!@param my_rank_IO  MPI rank
!
      module element_geometry_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_read_mesh_data
      use set_mesh_file_names
!
      implicit none
!
!   mesh file code
      integer(kind = kint), parameter ::  input_file_code = 14
!
      character(len=kchara) :: file_name
!
      private :: input_file_code, file_name
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine output_element_xyz_file                                &
     &         (my_rank_IO, file_prefix, ele_mesh_IO)
!
      use element_data_IO
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
      character(len=kchara) :: fhead_tmp
!
!
      write(*,*) 'file_prefix: ', file_prefix
      write(fhead_tmp,'(a,a4)') trim(file_prefix), '_xyz'
      call set_ele_comm_file_name(fhead_tmp, id_ascii_file_fmt,         &
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
      call dealloc_comm_table(ele_mesh_IO%comm)
      call dealloc_ele_geometry_data(ele_mesh_IO)
!
      end subroutine output_element_xyz_file
!
!------------------------------------------------------------------
!
      subroutine output_element_sph_file                                &
     &         (my_rank_IO, file_prefix, ele_mesh_IO)
!
      use element_data_IO
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
      character(len=kchara) :: fhead_tmp
!
!
      write(fhead_tmp,'(a,a4)') trim(file_prefix), '_sph'
      call set_ele_comm_file_name(fhead_tmp, id_ascii_file_fmt,       &
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
      call dealloc_comm_table(ele_mesh_IO%comm)
      call dealloc_ele_geometry_data(ele_mesh_IO)
!
      end subroutine output_element_sph_file
!
!------------------------------------------------------------------
!
      subroutine output_element_cyl_file                                &
     &         (my_rank_IO, file_prefix, ele_mesh_IO)
!
      use element_data_IO
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
      character(len=kchara) :: fhead_tmp
!
!
      write(fhead_tmp,'(a,a4)') trim(file_prefix), '_cyl'
      call set_ele_comm_file_name(fhead_tmp, id_ascii_file_fmt,         &
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
      call dealloc_comm_table(ele_mesh_IO%comm)
      call dealloc_ele_geometry_data(ele_mesh_IO)
!
      end subroutine output_element_cyl_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_surface_xyz_file                                &
     &         (my_rank_IO, file_prefix, surf_mesh_IO)
!
      use surface_data_IO
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
      character(len=kchara) :: fhead_tmp
!
!
      write(fhead_tmp,'(a,a4)') trim(file_prefix), '_xyz'
      call set_surf_mesh_file_name(fhead_tmp, id_ascii_file_fmt,        &
     &    my_rank_IO, file_name)
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Write ascii surface mesh file: ', trim(file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
      call write_surface_connection                                     &
     &   (input_file_code, my_rank_IO, surf_mesh_IO%comm,               &
     &    surf_mesh_IO%ele, surf_mesh_IO%sfed)
      call write_surface_geometry(input_file_code,                      &
     &    surf_mesh_IO%node, surf_mesh_IO%sfed)
      close (input_file_code)
!
      call dealloc_surface_mesh_IO(surf_mesh_IO)
      call dealloc_surf_geometry_data(surf_mesh_IO)
!
      end subroutine output_surface_xyz_file
!
!------------------------------------------------------------------
!
      subroutine output_surface_sph_file                                &
     &         (my_rank_IO, file_prefix, surf_mesh_IO)
!
      use surface_data_IO
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
      character(len=kchara) :: fhead_tmp
!
!
      write(fhead_tmp,'(a,a4)') trim(file_prefix), '_sph'
      call set_surf_mesh_file_name(fhead_tmp, id_ascii_file_fmt,        &
     &    my_rank_IO, file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
      call write_surface_connection                                     &
     &   (input_file_code, my_rank_IO, surf_mesh_IO%comm,               &
     &    surf_mesh_IO%ele, surf_mesh_IO%sfed)
      call write_surface_geometry_sph(input_file_code,                  &
     &    surf_mesh_IO%node, surf_mesh_IO%sfed)
      close (input_file_code)
!
      call dealloc_surface_mesh_IO(surf_mesh_IO)
      call dealloc_surf_geometry_data(surf_mesh_IO)
!
      end subroutine output_surface_sph_file
!
!------------------------------------------------------------------
!
      subroutine output_surface_cyl_file                                &
     &         (my_rank_IO, file_prefix, surf_mesh_IO)
!
      use surface_data_IO
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
      character(len=kchara) :: fhead_tmp
!
!
      write(fhead_tmp,'(a,a4)') trim(file_prefix), '_cyl'
      call set_surf_mesh_file_name(fhead_tmp, id_ascii_file_fmt,        &
     &    my_rank_IO, file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
      call write_surface_connection                                     &
     &   (input_file_code, my_rank_IO, surf_mesh_IO%comm,               &
     &    surf_mesh_IO%ele, surf_mesh_IO%sfed)
      call write_surface_geometry_cyl(input_file_code,                  &
     &    surf_mesh_IO%node, surf_mesh_IO%sfed)
      close (input_file_code)
!
      call dealloc_surface_mesh_IO(surf_mesh_IO)
      call dealloc_surf_geometry_data(surf_mesh_IO)
!
      end subroutine output_surface_cyl_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_edge_xyz_file                                   &
     &         (my_rank_IO, file_prefix, edge_mesh_IO)
!
      use edge_data_IO
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
      character(len=kchara) :: fhead_tmp
!
!
      write(fhead_tmp,'(a,a4)') trim(file_prefix), '_xyz'
      call set_edge_mesh_file_name(fhead_tmp, id_ascii_file_fmt,        &
     &    my_rank_IO, file_name)
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Write ascii edge mesh file: ', trim(file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
      call write_edge_connection                                        &
     &   (input_file_code, my_rank_IO, edge_mesh_IO%comm,               &
     &    edge_mesh_IO%ele, edge_mesh_IO%sfed)
      call write_edge_geometry(input_file_code,                         &
     &    edge_mesh_IO%node, edge_mesh_IO%sfed)
      close (input_file_code)
!
      call dealloc_edge_mesh_IO(edge_mesh_IO)
      call dealloc_surf_geometry_data(edge_mesh_IO)
!
      end subroutine output_edge_xyz_file
!
!------------------------------------------------------------------
!
      subroutine output_edge_sph_file                                   &
     &         (my_rank_IO, file_prefix, edge_mesh_IO)
!
      use edge_data_IO
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
      character(len=kchara) :: fhead_tmp
!
!
      write(fhead_tmp,'(a,a4)') trim(file_prefix), '_sph'
      call set_edge_mesh_file_name(fhead_tmp, id_ascii_file_fmt,        &
     &    my_rank_IO, file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
      call write_edge_connection                                        &
     &   (input_file_code, my_rank_IO, edge_mesh_IO%comm,               &
     &    edge_mesh_IO%ele, edge_mesh_IO%sfed)
      call write_edge_geometry_sph(input_file_code,                     &
     &    edge_mesh_IO%node, edge_mesh_IO%sfed)
      close (input_file_code)
!
      call dealloc_edge_mesh_IO(edge_mesh_IO)
      call dealloc_surf_geometry_data(edge_mesh_IO)
!
      end subroutine output_edge_sph_file
!
!------------------------------------------------------------------
!
      subroutine output_edge_cyl_file                                   &
     &         (my_rank_IO, file_prefix, edge_mesh_IO)
!
      use edge_data_IO
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
      character(len=kchara) :: fhead_tmp
!
!
      write(fhead_tmp,'(a,a4)') trim(file_prefix), '_cyl'
      call set_edge_mesh_file_name(fhead_tmp, id_ascii_file_fmt,        &
     &    my_rank_IO, file_name)
!
      open (input_file_code, file = file_name, form = 'formatted')
!
      call write_edge_connection                                        &
     &   (input_file_code, my_rank_IO, edge_mesh_IO%comm,               &
     &    edge_mesh_IO%ele, edge_mesh_IO%sfed)
      call write_edge_geometry_cyl(input_file_code,                     &
     &    edge_mesh_IO%node, edge_mesh_IO%sfed)
      close (input_file_code)
!
      call dealloc_edge_mesh_IO(edge_mesh_IO)
      call dealloc_surf_geometry_data(edge_mesh_IO)
!
      end subroutine output_edge_cyl_file
!
!------------------------------------------------------------------
!
      end module element_geometry_file_IO
