!viewer_IO_select_4_zlib.F90
!      module viewer_IO_select_4_zlib
!
!      Written by H. Matsui
!
!!      subroutine sel_output_surface_grid                              &
!!     &         (ifmt_file, nnod_4_surf, nnod_4_edge)
!!      subroutine sel_read_surface_grid                                &
!!     &          (ifmt_file, nnod_4_ele, nnod_4_surf, nnod_4_edge)
!
      module viewer_IO_select_4_zlib
!
      use m_precision
!
      use m_file_format_switch
!
      use viewer_file_IO
!
#ifdef ZLIB_IO
      use gz_viewer_file_IO
#endif
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_output_surface_grid                                &
     &         (ifmt_file, nnod_4_surf, nnod_4_edge)
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: nnod_4_edge
!
      integer(kind = kint), intent(in) :: ifmt_file
!
!
#ifdef ZLIB_IO
      if(ifmt_file .eq. id_gzip_txt_file_fmt) then
        call output_surface_grid_gz(nnod_4_surf, nnod_4_edge)
        return
      end if
#endif
!
      call output_surface_grid(nnod_4_surf, nnod_4_edge)
!
      end subroutine sel_output_surface_grid
!
!------------------------------------------------------------------
!
      subroutine sel_read_surface_grid                                  &
     &          (ifmt_file, nnod_4_ele, nnod_4_surf, nnod_4_edge)
!
      integer(kind = kint), intent(in) :: ifmt_file
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(inout) :: nnod_4_surf
      integer(kind = kint), intent(inout) :: nnod_4_edge
!
!
!
#ifdef ZLIB_IO
      if(ifmt_file .eq. id_gzip_txt_file_fmt) then
        call read_surface_grid_gz(nnod_4_ele, nnod_4_surf, nnod_4_edge)
        return
      end if
#endif
!
      call read_surface_grid(nnod_4_ele, nnod_4_surf, nnod_4_edge)
!
      end subroutine sel_read_surface_grid
!
!------------------------------------------------------------------
!
      end module viewer_IO_select_4_zlib
