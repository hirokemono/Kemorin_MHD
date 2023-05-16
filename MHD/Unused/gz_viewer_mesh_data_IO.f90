!gz_viewer_mesh_data_IO.f90
!      module gz_viewer_mesh_data_IO
!
!      Written by Kemorin on Jan., 2007
!
!!      subroutine write_domain_data_viewer_gz                          &
!!     &         (FPz_f, mgd_view_mesh, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine read_domain_data_viewer_gz                           &
!!     &         (FPz_f, mgd_view_mesh, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine write_node_data_viewer_gz(FPz_f, view_mesh, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine read_node_data_viewer_gz(FPz_f, view_mesh, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine write_surf_connect_viewer_gz                         &
!!     &         (FPz_f, num_pe, isurf_sf_stack, view_mesh, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine read_surf_connect_viewer_gz                          &
!!     &         (FPz_f, nnod_4_ele, nnod_4_edge, view_mesh, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine write_edge_connect_viewer_gz(FPz_f, view_mesh, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine read_edge_connect_viewer_gz                          &
!!     &         (FPz_f, nnod_4_edge, view_mesh, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!
      module gz_viewer_mesh_data_IO
!
      use m_precision
!
      use t_viewer_mesh
      use t_merged_viewer_mesh
      use t_buffer_4_gzip
!
      use m_viewer_mesh_labels
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_domain_data_viewer_gz                            &
     &         (FPz_f, mgd_view_mesh, zbuf)
!
      use gzip_file_access
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: num_pe_write
!
!
      zbuf%fixbuf(1) = hd_ndomain_viewer() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      num_pe_write = int(mgd_view_mesh%num_pe_sf,KIND(num_pe_write))
      write(zbuf%fixbuf(1),'(i16,2a1)')                                 &
     &                     num_pe_write, char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call write_gz_multi_int_8i16                                      &
     &   (FPz_f, num_pe_write, mgd_view_mesh%inod_sf_stack(1), zbuf)
      call write_gz_multi_int_8i16                                      &
     &   (FPz_f, num_pe_write, mgd_view_mesh%isurf_sf_stack(1), zbuf)
      call write_gz_multi_int_8i16                                      &
     &   (FPz_f, num_pe_write, mgd_view_mesh%iedge_sf_stack(1), zbuf)
!
      end subroutine write_domain_data_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_domain_data_viewer_gz                             &
     &         (FPz_f, mgd_view_mesh, zbuf)
!
      use gz_data_IO
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: num_pe_read
!
!
      call skip_gz_comment_int(FPz_f, num_pe_read, zbuf)
!
      call alloc_num_mesh_sf(int(num_pe_read), mgd_view_mesh)
!
      call read_gz_multi_int                                            &
     &   (FPz_f, num_pe_read, mgd_view_mesh%inod_sf_stack, zbuf)
      call read_gz_multi_int                                            &
     &   (FPz_f, num_pe_read, mgd_view_mesh%isurf_sf_stack, zbuf)
      call read_gz_multi_int                                            &
     &   (FPz_f, num_pe_read, mgd_view_mesh%iedge_sf_stack, zbuf)
!
      end subroutine read_domain_data_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_node_data_viewer_gz(FPz_f, view_mesh, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      type(viewer_mesh_data), intent(in) :: view_mesh
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i
!
!
      zbuf%fixbuf(1) = hd_node_viewer() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      write(zbuf%fixbuf(1),'(i16,2a1)') view_mesh%nnod_viewer,          &
     &                                  char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      do i = 1, view_mesh%nnod_viewer
        write(zbuf%fixbuf(1),'(i16,1p3E25.15e3,2a1)')                   &
     &     view_mesh%inod_gl_view(i), view_mesh%xx_view(i,1:3),         &
     &     char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end do
!
      end subroutine write_node_data_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_node_data_viewer_gz(FPz_f, view_mesh, zbuf)
!
      use gzip_file_access
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      type(viewer_mesh_data), intent(inout) :: view_mesh
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i, itmp
!
!
      call skip_gz_comment_int(FPz_f, itmp, zbuf)
!
      call alloc_nod_position_viewer(view_mesh)
!
      do i = 1, view_mesh%nnod_viewer
        call get_one_line_text_from_gz(FPz_f, zbuf)
        read(zbuf%fixbuf(1),*) itmp, view_mesh%xx_view(i,1:3)
      end do
!
      end subroutine read_node_data_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surf_connect_viewer_gz                           &
     &         (FPz_f, num_pe, isurf_sf_stack, view_mesh, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: isurf_sf_stack(0:num_pe)
      type(viewer_mesh_data), intent(in) :: view_mesh
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i, ist, num
      character(len=kchara) :: fmt_txt
!
!
      zbuf%fixbuf(1) = hd_surf_viewer() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      write(zbuf%fixbuf(1),'(i16,2a1)') view_mesh%nsurf_viewer,         &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
      do i = 1, num_pe
        ist = isurf_sf_stack(i-1)
        num = isurf_sf_stack(i) - isurf_sf_stack(i-1)
        if(num .gt. 0)  then
          call write_gz_multi_int_10i8(FPz_f, num,                      &
     &        view_mesh%surftyp_viewer(ist+1), zbuf)
        end if
      end do
!
      write(fmt_txt,'(a5,i2,a10)')                                      &
     &                '(i16,', view_mesh%nnod_v_surf, '(i16),2a1)'
      do i = 1, view_mesh%nsurf_viewer
        write(zbuf%fixbuf(1),fmt_txt)                                   &
     &      i, view_mesh%ie_sf_viewer(i,1:view_mesh%nnod_v_surf),       &
     &      char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end do
!
      end subroutine write_surf_connect_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_surf_connect_viewer_gz                            &
     &         (FPz_f, nnod_4_ele, nnod_4_edge, view_mesh, zbuf)
!
      use m_geometry_constants
      use m_node_quad_2_linear_sf
      use set_nnod_4_ele_by_type
      use gzip_file_access
      use gz_data_IO
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(inout) :: nnod_4_edge
      type(viewer_mesh_data), intent(inout) :: view_mesh
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i, itmp
      integer(kind = kint) :: nnod_4_surf
!
!
      call skip_gz_comment_int(FPz_f, itmp, zbuf)
!
      call alloc_surf_type_viewer(view_mesh)
!
      call read_gz_multi_int(FPz_f, view_mesh%nsurf_viewer,             &
     &                       view_mesh%surftyp_viewer, zbuf)
!
      call set_3D_nnod_4_sfed_by_ele                                    &
     &   (nnod_4_ele, nnod_4_surf, nnod_4_edge)
      call allocate_quad4_2_linear(nnod_4_ele)
!
      call alloc_surf_connect_viewer(nnod_4_surf, view_mesh)
!
      do i = 1, view_mesh%nsurf_viewer
        call get_one_line_text_from_gz(FPz_f, zbuf)
        read(zbuf%fixbuf(1),*)                                          &
     &            itmp, view_mesh%ie_sf_viewer(i,1:nnod_4_surf)
      end do
!
      end subroutine read_surf_connect_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_edge_connect_viewer_gz(FPz_f, view_mesh, zbuf)
!
      use m_geometry_constants
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      type(viewer_mesh_data), intent(in) :: view_mesh
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i
      character(len=kchara) :: fmt_txt
!
!
      zbuf%fixbuf(1) = hd_edge_viewer() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      write(zbuf%fixbuf(1),'(i16,2a1)') view_mesh%nedge_viewer,         &
     &                                  char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
!
      write(fmt_txt,'(a5,i2,a10)')                                      &
     &           '(i16,', view_mesh%nnod_v_edge, '(i16),2a1)'
      do i = 1, view_mesh%nedge_viewer
        write(zbuf%fixbuf(1),fmt_txt)                                   &
     &       i, view_mesh%ie_edge_viewer(i,1:view_mesh%nnod_v_edge),    &
     &       char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end do
!
      zbuf%fixbuf(1) = hd_edge_on_sf_viewer() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
      write(zbuf%fixbuf(1),'(i16,2a1)') view_mesh%nsurf_viewer,         &
     &                                  char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      write(fmt_txt,'(a5,i2,a10)')                                      &
     &                '(i16,', nedge_4_surf, '(i16),2a1)'
      do i = 1, view_mesh%nsurf_viewer
        write(zbuf%fixbuf(1),fmt_txt)                                   &
     &      i, view_mesh%iedge_sf_viewer(i,1:nedge_4_surf),             &
     &      char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end do
!
      end subroutine write_edge_connect_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_edge_connect_viewer_gz                            &
     &         (FPz_f, nnod_4_edge, view_mesh, zbuf)
!
      use m_geometry_constants
      use m_node_quad_2_linear_sf
      use gzip_file_access
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: nnod_4_edge
      type(viewer_mesh_data), intent(inout) :: view_mesh
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i, itmp
!
!
      call skip_gz_comment_int(FPz_f, itmp, zbuf)
!
      call alloc_edge_data_4_sf(nnod_4_edge, view_mesh)
!
      do i = 1, view_mesh%nedge_viewer
        call get_one_line_text_from_gz(FPz_f, zbuf)
        read(zbuf%fixbuf(1),*)                                          &
     &                  itmp, view_mesh%ie_edge_viewer(i,1:nnod_4_edge)
      end do
!
      call skip_gz_comment_int(FPz_f, itmp, zbuf)
!
      do i = 1, view_mesh%nsurf_viewer
        call get_one_line_text_from_gz(FPz_f, zbuf)
        read(zbuf%fixbuf(1),*)                                          &
     &        itmp, view_mesh%iedge_sf_viewer(i,1:nedge_4_surf)
      end do
!
      end subroutine read_edge_connect_viewer_gz
!
!------------------------------------------------------------------
!
      end module gz_viewer_mesh_data_IO
