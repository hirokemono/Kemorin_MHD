!gz_viewer_mesh_data_IO.f90
!      module gz_viewer_mesh_data_IO
!
!      Written by Kemorin on Jan., 2007
!
!!      subroutine write_domain_data_viewer_gz
!!        type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!!      subroutine read_domain_data_viewer_gz
!!        type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!!
!!      subroutine write_node_data_viewer_gz(view_mesh)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!      subroutine read_node_data_viewer_gz(view_mesh)
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!
!!      subroutine write_surf_connect_viewer_gz                         &
!!     &         (num_pe, isurf_sf_stack, view_mesh)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!      subroutine read_surf_connect_viewer_gz                          &
!!     &         (nnod_4_ele, nnod_4_edge, view_mesh)
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!
!!      subroutine write_edge_connect_viewer_gz(view_mesh)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!      subroutine read_edge_connect_viewer_gz(nnod_4_edge, view_mesh)
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!
      module gz_viewer_mesh_data_IO
!
      use m_precision
!
      use t_viewer_mesh
      use t_merged_viewer_mesh
!
      use skip_gz_comment
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
      subroutine write_domain_data_viewer_gz(mgd_view_mesh)
!
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!
      integer(kind = kint) :: num_pe_write
!
!
      textbuf = hd_ndomain_viewer() // char(0)
      call gz_write_textbuf_no_lf
!
      num_pe_write = int(mgd_view_mesh%num_pe_sf,KIND(num_pe_write))
      write(textbuf,'(i16,a1)') num_pe_write, char(0)
      call gz_write_textbuf_w_lf
!
      call write_gz_multi_int_8i10                                      &
     &   (num_pe_write, mgd_view_mesh%inod_sf_stack(1))
      call write_gz_multi_int_8i10                                      &
     &   (num_pe_write, mgd_view_mesh%isurf_sf_stack(1))
      call write_gz_multi_int_8i10                                      &
     &   (num_pe_write, mgd_view_mesh%iedge_sf_stack(1))
!
      end subroutine write_domain_data_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_domain_data_viewer_gz(mgd_view_mesh)
!
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
      integer(kind = kint) :: num_pe_read
!
!
      call skip_gz_comment_int(num_pe_read)
!
      call alloc_num_mesh_sf(int(num_pe_read), mgd_view_mesh)
!
      call read_gz_multi_int                                            &
     &   (num_pe_read, mgd_view_mesh%inod_sf_stack)
      call read_gz_multi_int                                            &
     &   (num_pe_read, mgd_view_mesh%isurf_sf_stack)
      call read_gz_multi_int                                            &
     &   (num_pe_read, mgd_view_mesh%iedge_sf_stack)
!
      end subroutine read_domain_data_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_node_data_viewer_gz(view_mesh)
!
      type(viewer_mesh_data), intent(in) :: view_mesh
!
      integer(kind = kint) :: i
!
!
      textbuf = hd_node_viewer() // char(0)
      call gz_write_textbuf_no_lf
!
      write(textbuf,'(i16,a1)') view_mesh%nnod_viewer, char(0)
      call gz_write_textbuf_w_lf
!
      do i = 1, view_mesh%nnod_viewer
        write(textbuf,'(i16,1p3E25.15e3,a1)')                           &
     &     view_mesh%inod_gl_view(i), view_mesh%xx_view(i,1:3), char(0)
        call gz_write_textbuf_w_lf
      end do
!
      end subroutine write_node_data_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_node_data_viewer_gz(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: i, itmp
!
!
      call skip_gz_comment_int(itmp)
!
      call alloc_nod_position_viewer(view_mesh)
!
      do i = 1, view_mesh%nnod_viewer
        call get_one_line_from_gz_f
        read(textbuf,*) itmp, view_mesh%xx_view(i,1:3)
      end do
!
      end subroutine read_node_data_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surf_connect_viewer_gz                           &
     &         (num_pe, isurf_sf_stack, view_mesh)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: isurf_sf_stack(0:num_pe)
      type(viewer_mesh_data), intent(in) :: view_mesh
!
      integer(kind = kint) :: i, ist, num
      character(len=kchara) :: fmt_txt
!
!
      textbuf = hd_surf_viewer() // char(0)
      call gz_write_textbuf_no_lf
!
      write(textbuf,'(i16,a1)') view_mesh%nsurf_viewer, char(0)
      call gz_write_textbuf_w_lf
      do i = 1, num_pe
        ist = isurf_sf_stack(i-1)
        num = isurf_sf_stack(i) - isurf_sf_stack(i-1)
        if(num .gt. 0)  call write_gz_multi_int_10i8                    &
     &                     (num, view_mesh%surftyp_viewer(ist+1))
      end do
!
      write(fmt_txt,'(a5,i2,a9)')                                       &
     &                '(i16,', view_mesh%nnod_v_surf, '(i16),a1)'
      do i = 1, view_mesh%nsurf_viewer
        write(textbuf,fmt_txt)                                          &
     &      i, view_mesh%ie_sf_viewer(i,1:view_mesh%nnod_v_surf),       &
     &      char(0)
        call gz_write_textbuf_w_lf
      end do
!
end subroutine write_surf_connect_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_surf_connect_viewer_gz                            &
     &         (nnod_4_ele, nnod_4_edge, view_mesh)
!
      use m_geometry_constants
      use m_node_quad_2_linear_sf
      use set_nnod_4_ele_by_type
!
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(inout) :: nnod_4_edge
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: i, itmp
      integer(kind = kint) :: nnod_4_surf
!
!
      call skip_gz_comment_int(itmp)
!
      call alloc_surf_type_viewer(view_mesh)
!
      call read_gz_multi_int                                            &
     &   (view_mesh%nsurf_viewer, view_mesh%surftyp_viewer)
!
      call set_3D_nnod_4_sfed_by_ele                                    &
     &   (nnod_4_ele, nnod_4_surf, nnod_4_edge)
      call allocate_quad4_2_linear(nnod_4_ele)
!
      call alloc_surf_connect_viewer(nnod_4_surf, view_mesh)
!
      do i = 1, view_mesh%nsurf_viewer
        call get_one_line_from_gz_f
        read(textbuf,*) itmp, view_mesh%ie_sf_viewer(i,1:nnod_4_surf)
      end do
!
      end subroutine read_surf_connect_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_edge_connect_viewer_gz(view_mesh)
!
      use m_geometry_constants
!
      type(viewer_mesh_data), intent(in) :: view_mesh
!
      integer(kind = kint) :: i
      character(len=kchara) :: fmt_txt
!
!
      textbuf = hd_edge_viewer() // char(0)
      call gz_write_textbuf_no_lf
!
      write(textbuf,'(i16,a1)') view_mesh%nedge_viewer, char(0)
      call gz_write_textbuf_w_lf
!
!
      write(fmt_txt,'(a5,i2,a9)')                                       &
     &           '(i16,', view_mesh%nnod_v_edge, '(i16),a1)'
      do i = 1, view_mesh%nedge_viewer
        write(textbuf,fmt_txt)                                          &
     &       i, view_mesh%ie_edge_viewer(i,1:view_mesh%nnod_v_edge),    &
     &       char(0)
        call gz_write_textbuf_w_lf
      end do
!
      textbuf = hd_edge_on_sf_viewer() // char(0)
      call gz_write_textbuf_no_lf
      write(textbuf,'(i16,a1)') view_mesh%nsurf_viewer, char(0)
      call gz_write_textbuf_w_lf
!
      write(fmt_txt,'(a5,i2,a9)')                                       &
     &                '(i16,', nedge_4_surf, '(i16),a1)'
      do i = 1, view_mesh%nsurf_viewer
        write(textbuf,fmt_txt)                                          &
     & i, view_mesh%iedge_sf_viewer(i,1:nedge_4_surf), char(0)
        call gz_write_textbuf_w_lf
      end do
!
      end subroutine write_edge_connect_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_edge_connect_viewer_gz(nnod_4_edge, view_mesh)
!
      use m_geometry_constants
      use m_node_quad_2_linear_sf
!
      integer(kind = kint), intent(in) :: nnod_4_edge
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: i, itmp
!
!
      call skip_gz_comment_int(itmp)
!
      call alloc_edge_data_4_sf(nnod_4_edge, view_mesh)
!
      do i = 1, view_mesh%nedge_viewer
        call get_one_line_from_gz_f
        read(textbuf,*) itmp, view_mesh%ie_edge_viewer(i,1:nnod_4_edge)
      end do
!
      call skip_gz_comment_int(itmp)
!
      do i = 1, view_mesh%nsurf_viewer
        call get_one_line_from_gz_f
        read(textbuf,*)                                                 &
     &        itmp, view_mesh%iedge_sf_viewer(i,1:nedge_4_surf)
      end do
!
      end subroutine read_edge_connect_viewer_gz
!
!------------------------------------------------------------------
!
      end module gz_viewer_mesh_data_IO
