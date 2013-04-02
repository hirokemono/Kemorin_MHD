!viewer_IO_select_4_zlib.F90
!      module viewer_IO_select_4_zlib
!
      module viewer_IO_select_4_zlib
!
!      Written by H. Matsui
!
      use m_precision
!
      use m_file_format_switch
!
      implicit none
!
!      subroutine sel_output_surface_grid(ifmt_file)
!      subroutine sel_read_surface_grid(ifmt_file)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_output_surface_grid(ifmt_file)
!
      use viewer_file_IO
      use gz_viewer_file_IO
!
      integer(kind = kint), intent(in) :: ifmt_file
!
!
#ifdef ZLIB_IO
      if(ifmt_file .eq. id_gzip_txt_file_fmt) then
        call output_surface_grid_gz
        return
      end if
#endif
!
      call output_surface_grid
!
      end subroutine sel_output_surface_grid
!
!------------------------------------------------------------------
!
      subroutine sel_read_surface_grid(ifmt_file)
!
      use viewer_file_IO
      use gz_viewer_file_IO
!
      integer(kind = kint), intent(in) :: ifmt_file
!
!
#ifdef ZLIB_IO
      if(ifmt_file .eq. id_gzip_txt_file_fmt) then
        call read_surface_grid_gz
        return
      end if
#endif
!
      call read_surface_grid
!
      end subroutine sel_read_surface_grid
!
!------------------------------------------------------------------
!
      end module viewer_IO_select_4_zlib
