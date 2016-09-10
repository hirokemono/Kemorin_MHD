!>@file   mesh_IO_select.f90
!!@brief  module mesh_IO_select
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      subroutine set_mesh_fname(id_rank)
!!
!!      subroutine sel_read_mesh(id_rank)
!!      subroutine sel_read_mesh_geometry(id_rank)
!!
!!      subroutine sel_read_node_size(id_rank)
!!      subroutine sel_read_geometry_size(id_rank)
!!
!!      subroutine sel_write_mesh_file(id_rank)
!!
!!      integer(kind = kint) function check_exist_mesh(id_rank)
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
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_fname(id_rank)
!
      use m_file_format_switch
      use set_mesh_file_names
!
      integer(kind = kint), intent(in) :: id_rank
      character(len=kchara) :: fname_tmp
!
!
      call set_mesh_file_name(mesh_file_head, iflag_mesh_file_fmt,      &
     &    id_rank, mesh_file_name)
!
      end subroutine set_mesh_fname
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_read_mesh(id_rank)
!
      integer(kind= kint), intent(in) :: id_rank
!
!
      call set_mesh_fname(id_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call read_mesh_file_b(id_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_mesh_file_b(id_rank)
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_mesh(id_rank)
#endif
!
      else
        call read_mesh_file(id_rank)
      end if 
!
      end subroutine sel_read_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_mesh_geometry(id_rank)
!
      integer(kind= kint), intent(in) :: id_rank
!
!
      call set_mesh_fname(id_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call read_mesh_geometry_b(id_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_mesh_geometry_b(id_rank)
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_mesh_geometry(id_rank)
#endif
!
      else
        call read_mesh_geometry(id_rank)
      end if 
!
      end subroutine sel_read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
       subroutine sel_read_node_size(id_rank)
!
      integer(kind= kint), intent(in) :: id_rank
!
!
      call set_mesh_fname(id_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call read_node_size_b(id_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_node_size_b(id_rank)
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_node_size(id_rank)
#endif
!
      else
        call read_node_size(id_rank)
      end if 
!
      end subroutine sel_read_node_size
!
!------------------------------------------------------------------
!
       subroutine sel_read_geometry_size(id_rank)
!
      integer(kind= kint), intent(in) :: id_rank
!
!
      call set_mesh_fname(id_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call read_geometry_size_b(id_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_geometry_size_b(id_rank)
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_geometry_size(id_rank)
#endif
!
      else
        call read_geometry_size(id_rank)
      end if 
!
      end subroutine sel_read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_mesh_file(id_rank)
!
      integer(kind= kint), intent(in) :: id_rank
!
!
      call set_mesh_fname(id_rank)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call write_mesh_file_b(id_rank)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_write_mesh_file_b(id_rank)
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_write_mesh_file(id_rank)
#endif
!
      else
        call write_mesh_file(id_rank)
      end if
!
      end subroutine sel_write_mesh_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_exist_mesh(id_rank)
!
      use delete_data_files
!
      integer(kind= kint), intent(in) :: id_rank
!
!
      call set_mesh_fname(id_rank)
!
      check_exist_mesh = check_file_exist(mesh_file_name)
!
      return
      end function check_exist_mesh
!
!  ---------------------------------------------------------------------
!
      end module mesh_IO_select
