!mesh_IO_select.F90
!      module mesh_IO_select
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine sel_read_mesh(my_rank)
!      subroutine sel_read_mesh_geometry(my_rank)
!
!      subroutine sel_read_node_size(my_rank)
!      subroutine sel_read_geometry_size(my_rank)
!
!      subroutine sel_write_mesh_file(my_rank)
!
      module mesh_IO_select
!
      use m_precision
!
      use m_read_mesh_data
      use m_file_format_switch
      use mesh_file_IO
      use mesh_file_IO_b
      use gz_mesh_file_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
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
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_mesh_gz(my_rank)
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
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_mesh_geometry_gz(my_rank)
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
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_node_size_gz(my_rank)
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
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_geometry_size_gz(my_rank)
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
!
#ifdef ZLIB_IO
      else if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call write_mesh_file_gz(my_rank)
#endif
!
      else
        call write_mesh_file(my_rank)
      end if
!
      end subroutine sel_write_mesh_file
!
!  ---------------------------------------------------------------------
!
      end module mesh_IO_select
