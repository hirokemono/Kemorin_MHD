!>@file   particle_file_IO_select.f90
!!@brief  module particle_file_IO_select
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      subroutine sel_read_particle_file                               &
!!     &         (mesh_file, id_rank, t_IO, particle_IO, ierr)
!!        integer, intent(in) :: id_rank
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(time_data), intent(inout) :: t_IO
!!        type(surf_edge_IO_file), intent(inout) :: particle_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine sel_write_particle_file                              &
!!     &         (mesh_file, id_rank, t_IO, particle_IO)
!!        integer, intent(in) :: id_rank
!!        type(time_data), intent(in) :: t_IO
!!        type(surf_edge_IO_file), intent(in) :: particle_IO
!!@endverbatim
!
      module particle_file_IO_select
!
      use m_precision
!
      use t_file_IO_parameter
      use t_mesh_data
      use m_file_format_switch
!
      use mesh_file_name_by_param
      use particle_file_IO
      use particle_file_IO_b
#ifdef ZLIB_IO
      use gz_particle_file_IO
      use gz_particle_file_IO_b
#endif
!
      implicit none
!
      character(len=kchara), private :: file_name
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_particle_file                                 &
     &         (mesh_file, id_rank, t_IO, particle_IO, ierr)
!
      use set_element_mesh_file_names
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(time_data), intent(inout) :: t_IO
      type(surf_edge_IO_file), intent(inout) :: particle_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      file_name = set_edge_mesh_file_name                               &
     &   (mesh_file%file_prefix, mesh_file%iflag_format, id_rank)
!
      if (mesh_file%iflag_format .eq. id_binary_file_fmt) then
        call read_particle_file_b(id_rank, file_name,                   &
     &                            t_IO, particle_IO, ierr)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_read_particle_file_b(id_rank, file_name,                &
     &                               t_IO, particle_IO, ierr)
      else if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_read_particle_file(id_rank, file_name,                  &
     &                             t_IO, particle_IO, ierr)
#endif
!
      else
        call read_particle_file(id_rank, file_name,                     &
     &                          t_IO, particle_IO, ierr)
      end if 
!
      end subroutine sel_read_particle_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_particle_file                                &
     &         (mesh_file, id_rank, t_IO, particle_IO)
!
      use set_element_mesh_file_names
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(time_data), intent(in) :: t_IO
      type(surf_edge_IO_file), intent(in) :: particle_IO
      integer(kind = kint) :: ierr = 0
!
!
      file_name = set_edge_mesh_file_name                               &
     &   (mesh_file%file_prefix, mesh_file%iflag_format, id_rank)
!
      if (mesh_file%iflag_format .eq. id_binary_file_fmt) then
        call write_particle_file_b(id_rank, file_name,                  &
     &                             t_IO, particle_IO, ierr)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_write_particle_file_b(id_rank, file_name,               &
     &                                t_IO, particle_IO)
      else if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_write_particle_file(id_rank, file_name,                 &
     &                              t_IO, particle_IO)
#endif
!
      else
        call write_particle_file(id_rank, file_name, t_IO, particle_IO)
      end if
!
      end subroutine sel_write_particle_file
!
!  ---------------------------------------------------------------------
!
      end module particle_file_IO_select
