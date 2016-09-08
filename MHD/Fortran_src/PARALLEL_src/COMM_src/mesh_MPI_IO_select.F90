!>@file   mesh_MPI_IO_select.f90
!!@brief  module mesh_MPI_IO_select
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      subroutine sel_mpi_read_mesh
!!      subroutine sel_mpi_read_mesh_geometry
!!
!!      subroutine sel_mpi_read_node_size
!!      subroutine sel_mpi_read_geometry_size
!!
!!      subroutine sel_mpi_write_mesh_file
!!@endverbatim
!
      module mesh_MPI_IO_select
!
      use m_precision
      use calypso_mpi
!
      use m_read_mesh_data
      use m_file_format_switch
!
      use mesh_IO_select
!
      use MPI_mesh_file_IO
      use MPI_mesh_file_IO_b
      use gz_MPI_mesh_file_IO
      use gz_MPI_mesh_file_IO_b
      use set_mesh_file_names
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_mpi_read_mesh
!
!
      call set_mesh_fname(my_rank)
!
      if(iflag_mesh_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_mesh_file_b(nprocs, my_rank)
      else if(iflag_mesh_file_fmt .eq. iflag_single) then
        call mpi_read_mesh_file(my_rank)
        call set_mesh_file_name(mesh_file_head, id_ascii_file_fmt,      &
     &      my_rank, mesh_file_name)
        call read_mesh_file(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_mesh_file_b(nprocs, my_rank)
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_mesh(my_rank)
        call set_mesh_file_name(mesh_file_head, id_gzip_txt_file_fmt,   &
     &      my_rank, mesh_file_name)
        call gz_read_mesh(my_rank)
#endif
!
      else
        call sel_read_mesh(my_rank)
      end if 
!
      end subroutine sel_mpi_read_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine sel_mpi_read_mesh_geometry
!
!
      call set_mesh_fname(my_rank)
!
      if(iflag_mesh_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_mesh_geometry_b(nprocs, my_rank)
      else if(iflag_mesh_file_fmt .eq. iflag_single) then
        call mpi_read_mesh_geometry(my_rank)
        call set_mesh_file_name(mesh_file_head, id_ascii_file_fmt,      &
     &      my_rank, mesh_file_name)
        call read_mesh_geometry(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_mesh_geometry_b(nprocs, my_rank)
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_mesh_geometry(my_rank)
        call set_mesh_file_name(mesh_file_head, id_gzip_txt_file_fmt,   &
     &      my_rank, mesh_file_name)
        call gz_read_mesh_geometry(my_rank)
#endif
!
      else
        call sel_read_mesh_geometry(my_rank)
      end if 
!
      end subroutine sel_mpi_read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
       subroutine sel_mpi_read_node_size
!
!
      call set_mesh_fname(my_rank)
!
      if(iflag_mesh_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_node_size_b(nprocs, my_rank)
      else if(iflag_mesh_file_fmt .eq. iflag_single) then
        call mpi_read_node_size(my_rank)
        call set_mesh_file_name(mesh_file_head, id_ascii_file_fmt,      &
     &      my_rank, mesh_file_name)
        call read_node_size(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_node_size_b(nprocs, my_rank)
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_node_size(my_rank)
        call set_mesh_file_name(mesh_file_head, id_gzip_txt_file_fmt,   &
     &      my_rank, mesh_file_name)
        call gz_read_node_size(my_rank)
#endif
!
      else
        call sel_read_node_size(my_rank)
      end if 
!
      end subroutine sel_mpi_read_node_size
!
!------------------------------------------------------------------
!
       subroutine sel_mpi_read_geometry_size
!
!
      call set_mesh_fname(my_rank)
!
      if(iflag_mesh_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_geometry_size_b(nprocs, my_rank)
      else if(iflag_mesh_file_fmt .eq. iflag_single) then
        call mpi_read_geometry_size(my_rank)
        call set_mesh_file_name(mesh_file_head, id_ascii_file_fmt,      &
     &      my_rank, mesh_file_name)
        call read_geometry_size(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_geometry_size_b(nprocs, my_rank)
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_geometry_size(my_rank)
        call set_mesh_file_name(mesh_file_head, id_gzip_txt_file_fmt,   &
     &      my_rank, mesh_file_name)
        call gz_read_geometry_size(my_rank)
#endif
!
      else
        call sel_read_geometry_size(my_rank)
      end if 
!
      end subroutine sel_mpi_read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_mpi_write_mesh_file
!
!
      call set_mesh_fname(my_rank)
!
      if(iflag_mesh_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_write_mesh_file_b(nprocs, my_rank)
      else if(iflag_mesh_file_fmt .eq. iflag_single) then
        call mpi_write_mesh_file(my_rank)
        call set_mesh_file_name(mesh_file_head, id_ascii_file_fmt,      &
     &      my_rank, mesh_file_name)
        call write_mesh_file(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_write_mesh_file_b(nprocs, my_rank)
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_write_mesh_file(my_rank)
        call set_mesh_file_name(mesh_file_head, id_gzip_txt_file_fmt,   &
     &      my_rank, mesh_file_name)
        call gz_write_mesh_file(my_rank)
#endif
!
      else
        call sel_write_mesh_file(my_rank)
      end if
!
      end subroutine sel_mpi_write_mesh_file
!
!  ---------------------------------------------------------------------
!
      end module mesh_MPI_IO_select
