!>@file   mesh_IO_select.f90
!!@brief  module mesh_IO_select
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      subroutine sel_read_mesh(my_rank)
!!      subroutine sel_read_mesh_geometry(my_rank)
!!
!!      subroutine sel_read_node_size(my_rank)
!!      subroutine sel_read_geometry_size(my_rank)
!!
!!      subroutine sel_write_mesh_file(my_rank)
!!
!!      integer(kind = kint) function check_exist_mesh(my_rank)
!!@endverbatim
!
      module mesh_IO_select
!
      use m_precision
!
      use m_read_mesh_data
      use m_file_format_switch
!
      use mesh_file_IO
      use mesh_file_IO_b
      use gz_mesh_file_IO
      use gz_mesh_file_IO_b
!
      use MPI_mesh_file_IO
      use MPI_mesh_file_IO_b
      use gz_MPI_mesh_file_IO
      use gz_MPI_mesh_file_IO_b
!
      implicit none
!
      private :: set_mesh_fname
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_fname(my_rank)
!
      use m_file_format_switch
      use set_mesh_file_names
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: fname_tmp
!
!
      call set_mesh_file_name(mesh_file_head, iflag_mesh_file_fmt,      &
     &    my_rank, mesh_file_name)
!
      end subroutine set_mesh_fname
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_read_mesh(my_rank)
!
      integer(kind= kint), intent(in) :: my_rank
!
!
      call set_mesh_fname(my_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call read_mesh_file_b(my_rank)
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_binary_file_fmt) then
        call read_mesh_file_mpi_b(my_rank)
        call set_mesh_file_name(mesh_file_head, id_binary_file_fmt,     &
     &      my_rank, mesh_file_name)
        call read_mesh_file_b(my_rank)
      else if(iflag_mesh_file_fmt .eq. iflag_single) then
        call read_mesh_file_mpi(my_rank)
        call set_mesh_file_name(mesh_file_head, iflag_ascii,            &
     &      my_rank, mesh_file_name)
        call read_mesh_file(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_mesh_file_b(my_rank)
        call set_mesh_file_name(mesh_file_head, id_gzip_bin_file_fmt,   &
     &      my_rank, mesh_file_name)
        call gz_read_mesh_file_b(my_rank)
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_mesh(my_rank)
        call set_mesh_file_name(mesh_file_head, id_gzip_txt_file_fmt,   &
     &      my_rank, mesh_file_name)
        call gz_read_mesh(my_rank)
      else if(iflag_mesh_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_mesh_file_b(my_rank)
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_mesh(my_rank)
#endif
!
      else
        call read_mesh_file(my_rank)
      end if 
!
      end subroutine sel_read_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_mesh_geometry(my_rank)
!
      integer(kind= kint), intent(in) :: my_rank
!
!
      call set_mesh_fname(my_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call read_mesh_geometry_b(my_rank)
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_binary_file_fmt) then
        call read_mesh_geometry_mpi_b(my_rank)
        call set_mesh_file_name(mesh_file_head, id_binary_file_fmt,     &
     &      my_rank, mesh_file_name)
        call read_mesh_geometry_b(my_rank)
      else if(iflag_mesh_file_fmt .eq. iflag_single) then
        call read_mesh_geometry_mpi(my_rank)
        call set_mesh_file_name(mesh_file_head, iflag_ascii,            &
     &      my_rank, mesh_file_name)
        call read_mesh_geometry(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_mesh_geometry_b(my_rank)
        call set_mesh_file_name(mesh_file_head, id_gzip_bin_file_fmt,   &
     &      my_rank, mesh_file_name)
        call gz_read_mesh_geometry_b(my_rank)
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_mesh_geometry(my_rank)
        call set_mesh_file_name(mesh_file_head, id_gzip_txt_file_fmt,   &
     &      my_rank, mesh_file_name)
        call gz_read_mesh_geometry(my_rank)
      else if(iflag_mesh_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_mesh_geometry_b(my_rank)
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_mesh_geometry(my_rank)
#endif
!
      else
        call read_mesh_geometry(my_rank)
      end if 
!
      end subroutine sel_read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
       subroutine sel_read_node_size(my_rank)
!
      integer(kind= kint), intent(in) :: my_rank
!
!
      call set_mesh_fname(my_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call read_node_size_b(my_rank)
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_binary_file_fmt) then
        call read_node_size_mpi_b(my_rank)
        call set_mesh_file_name(mesh_file_head, id_binary_file_fmt,     &
     &      my_rank, mesh_file_name)
        call read_node_size_b(my_rank)
      else if(iflag_mesh_file_fmt .eq. iflag_single) then
        call read_node_size_mpi(my_rank)
        call set_mesh_file_name(mesh_file_head, iflag_ascii,            &
     &      my_rank, mesh_file_name)
        call read_node_size(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_node_size_b(my_rank)
        call set_mesh_file_name(mesh_file_head, id_gzip_bin_file_fmt,   &
     &      my_rank, mesh_file_name)
        call gz_read_node_size_b(my_rank)
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_node_size(my_rank)
        call set_mesh_file_name(mesh_file_head, id_gzip_txt_file_fmt,   &
     &      my_rank, mesh_file_name)
        call gz_read_node_size(my_rank)
      else if(iflag_mesh_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_node_size_b(my_rank)
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_node_size(my_rank)
#endif
!
      else
        call read_node_size(my_rank)
      end if 
!
      end subroutine sel_read_node_size
!
!------------------------------------------------------------------
!
       subroutine sel_read_geometry_size(my_rank)
!
      integer(kind= kint), intent(in) :: my_rank
!
!
      call set_mesh_fname(my_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call read_geometry_size_b(my_rank)
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_binary_file_fmt) then
        call read_geometry_size_mpi_b(my_rank)
        call set_mesh_file_name(mesh_file_head, id_binary_file_fmt,     &
     &      my_rank, mesh_file_name)
        call read_geometry_size_b(my_rank)
      else if(iflag_mesh_file_fmt .eq. iflag_single) then
        call read_geometry_size_mpi(my_rank)
        call set_mesh_file_name(mesh_file_head, iflag_ascii,            &
     &      my_rank, mesh_file_name)
        call read_geometry_size(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_geometry_size_b(my_rank)
        call set_mesh_file_name(mesh_file_head, id_gzip_bin_file_fmt,   &
     &      my_rank, mesh_file_name)
        call gz_read_geometry_size_b(my_rank)
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_geometry_size(my_rank)
        call set_mesh_file_name(mesh_file_head, id_gzip_txt_file_fmt,   &
     &      my_rank, mesh_file_name)
        call gz_read_geometry_size(my_rank)
      else if(iflag_mesh_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_geometry_size_b(my_rank)
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_geometry_size(my_rank)
#endif
!
      else
        call read_geometry_size(my_rank)
      end if 
!
      end subroutine sel_read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_mesh_file(my_rank)
!
      integer(kind= kint), intent(in) :: my_rank
!
!
      call set_mesh_fname(my_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call write_mesh_file_b(my_rank)
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_binary_file_fmt) then
        call write_mesh_file_mpi_b(my_rank)
        call set_mesh_file_name(mesh_file_head, id_binary_file_fmt,     &
     &      my_rank, mesh_file_name)
        call write_mesh_file_b(my_rank)
      else if(iflag_mesh_file_fmt .eq. iflag_single) then
        call write_mesh_file_mpi(my_rank)
        call set_mesh_file_name(mesh_file_head, iflag_ascii,            &
     &      my_rank, mesh_file_name)
        call write_mesh_file(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_write_mesh_file_b(my_rank)
        call set_mesh_file_name(mesh_file_head, id_gzip_bin_file_fmt,   &
     &      my_rank, mesh_file_name)
        call gz_write_mesh_file_b(my_rank)
      else if(iflag_mesh_file_fmt                                       &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_write_mesh_file(my_rank)
        call set_mesh_file_name(mesh_file_head, id_gzip_txt_file_fmt,   &
     &      my_rank, mesh_file_name)
        call gz_write_mesh_file(my_rank)
      else if(iflag_mesh_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_write_mesh_file_b(my_rank)
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_write_mesh_file(my_rank)
#endif
!
      else
        call write_mesh_file(my_rank)
      end if
!
      end subroutine sel_write_mesh_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_exist_mesh(my_rank)
!
      use delete_data_files
!
      integer(kind= kint), intent(in) :: my_rank
!
!
      call set_mesh_fname(my_rank)
!
      check_exist_mesh = check_file_exist(mesh_file_name)
!
      return
      end function check_exist_mesh
!
!  ---------------------------------------------------------------------
!
      end module mesh_IO_select
