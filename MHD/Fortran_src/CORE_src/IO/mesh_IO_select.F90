!>@file   mesh_IO_select.f90
!!@brief  module mesh_IO_select
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      subroutine set_mesh_fname(my_rank_IO)
!!
!!      subroutine sel_read_mesh(my_rank_IO, fem_IO, ierr)
!!        type(mesh_data), intent(inout) :: fem_IO
!!
!!      subroutine sel_read_mesh_geometry(my_rank_IO, mesh_IO, ierr)
!!      subroutine sel_read_node_size(my_rank_IO, mesh_IO, ierr)
!!      subroutine sel_read_geometry_size(my_rank_IO, mesh_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine sel_write_mesh_file(my_rank_IO, fem_IO)
!!        type(mesh_data), intent(inout) :: fem_IO
!!
!!      integer(kind = kint) function check_exist_mesh(my_rank_IO)
!!@endverbatim
!
      module mesh_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use t_mesh_data
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
      subroutine set_mesh_fname(my_rank_IO)
!
      use m_file_format_switch
      use set_mesh_file_names
!
      integer(kind = kint), intent(in) :: my_rank_IO
!
!
      call set_mesh_file_name(mesh_file_head, iflag_mesh_file_fmt,      &
     &    my_rank_IO, mesh_file_name)
!
      end subroutine set_mesh_fname
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_read_mesh(my_rank_IO, fem_IO, ierr)
!
      integer(kind= kint), intent(in) :: my_rank_IO
!
      type(mesh_data), intent(inout) :: fem_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!
      call set_mesh_fname(my_rank_IO)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call read_mesh_file_b(my_rank_IO, fem_IO, ierr)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_mesh_file_b(my_rank_IO, fem_IO, ierr)
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_mesh(my_rank_IO, fem_IO, ierr)
#endif
!
      else
        call read_mesh_file(my_rank_IO, fem_IO, ierr)
      end if 
!
      end subroutine sel_read_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_mesh_geometry(my_rank_IO, mesh_IO, ierr)
!
      integer(kind= kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call set_mesh_fname(my_rank_IO)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call read_mesh_geometry_b(my_rank_IO, mesh_IO, ierr)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_mesh_geometry_b(my_rank_IO, mesh_IO, ierr)
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_mesh_geometry(my_rank_IO, mesh_IO, ierr)
#endif
!
      else
        call read_mesh_geometry(my_rank_IO, mesh_IO, ierr)
      end if 
!
      end subroutine sel_read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
       subroutine sel_read_node_size(my_rank_IO, mesh_IO, ierr)
!
      integer(kind= kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call set_mesh_fname(my_rank_IO)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call read_node_size_b(my_rank_IO, mesh_IO, ierr)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_node_size_b(my_rank_IO, mesh_IO, ierr)
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_node_size(my_rank_IO, mesh_IO, ierr)
#endif
!
      else
        call read_node_size(my_rank_IO, mesh_IO, ierr)
      end if 
!
      end subroutine sel_read_node_size
!
!------------------------------------------------------------------
!
       subroutine sel_read_geometry_size(my_rank_IO, mesh_IO, ierr)
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call set_mesh_fname(my_rank_IO)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call read_geometry_size_b(my_rank_IO, mesh_IO, ierr)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_geometry_size_b(my_rank_IO, mesh_IO, ierr)
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_read_geometry_size(my_rank_IO, mesh_IO, ierr)
#endif
!
      else
        call read_geometry_size(my_rank_IO, mesh_IO, ierr)
      end if 
!
      end subroutine sel_read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_mesh_file(my_rank_IO, fem_IO)
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(mesh_data), intent(inout) :: fem_IO
!
!
      call set_mesh_fname(my_rank_IO)
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call write_mesh_file_b(my_rank_IO, fem_IO)
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_write_mesh_file_b(my_rank_IO, fem_IO)
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call gz_write_mesh_file(my_rank_IO, fem_IO)
#endif
!
      else
        call write_mesh_file(my_rank_IO, fem_IO)
      end if
!
      end subroutine sel_write_mesh_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_exist_mesh(my_rank_IO)
!
      use delete_data_files
!
      integer(kind= kint), intent(in) :: my_rank_IO
!
!
      call set_mesh_fname(my_rank_IO)
!
      check_exist_mesh = check_file_exist(mesh_file_name)
!
      return
      end function check_exist_mesh
!
!  ---------------------------------------------------------------------
!
      end module mesh_IO_select
