!gz_viewer_mesh_data_IO.f90
!      module gz_viewer_mesh_data_IO
!
!      Written by Kemorin on Jan., 2007
!
!      subroutine write_domain_data_viewer_gz
!      subroutine read_domain_data_viewer_gz
!
!      subroutine write_node_data_viewer_gz
!      subroutine read_node_data_viewer_gz
!
!      subroutine write_surf_connect_viewer_gz
!      subroutine read_surf_connect_viewer_gz
!
!      subroutine write_edge_connect_viewer_gz
!      subroutine read_edge_connect_viewer_gz
!
!      subroutine write_domain_center_viewer_gz
!      subroutine read_domain_center_viewer_gz
!
      module gz_viewer_mesh_data_IO
!
      use m_precision
!
      use m_surface_mesh_4_merge
      use skip_gz_comment
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_domain_data_viewer_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! number of domain ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)')                                           &
     &                 '!   stack of node for domain ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)')                                           &
     &                 '!   stack of surface for domain ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)')                                           &
     &                 '!   stack of edge for domain ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(i10,a1)') num_pe_sf, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_gz_multi_int_8i10(num_pe_sf, inod_sf_stack(1))
      call write_gz_multi_int_8i10(num_pe_sf, isurf_sf_stack(1))
      call write_gz_multi_int_8i10(num_pe_sf, iedge_sf_stack(1))
!
      end subroutine write_domain_data_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_domain_data_viewer_gz
!
!
      call skip_gz_comment_int(num_pe_sf)
!
      call allocate_num_mesh_sf
!
      call read_gz_multi_int(num_pe_sf, inod_sf_stack)
      call read_gz_multi_int(num_pe_sf, isurf_sf_stack)
      call read_gz_multi_int(num_pe_sf, iedge_sf_stack)
!
      nodpetot_viewer =  inod_sf_stack(num_pe_sf)
      surfpetot_viewer = isurf_sf_stack(num_pe_sf)
      edgepetot_viewer = iedge_sf_stack(num_pe_sf)
!
      end subroutine read_domain_data_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_node_data_viewer_gz
!
      integer(kind = kint) :: i
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 1. node information', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)')                                           &
     &                 '! number_of node, intenal_node', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! Global ID, x, y, z', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(i10,a1)') nodpetot_viewer, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      do i = 1, nodpetot_viewer
        write(textbuf,1002) i, xx_view(i,1:3), char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
 1002 format(i10, 1p3e23.12,a1)
!
      end subroutine write_node_data_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_node_data_viewer_gz
!
      integer(kind = kint) :: i, itmp
      integer(kind = kint) :: nchara
!
!
      call skip_gz_comment_int(itmp)
!
      call allocate_nod_position_viewer
!
      do i = 1, nodpetot_viewer
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) itmp, xx_view(i,1:3)
      end do
!
      end subroutine read_node_data_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surf_connect_viewer_gz
!
      use m_geometry_parameter
!
      integer(kind = kint) :: i
      character(len=kchara) :: fmt_txt
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 2. element information', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! element type', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! Global ID, connectivity', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(i10,a1)') surfpetot_viewer, char(0)
      call write_compress_txt(nbuf, textbuf)
      call write_gz_multi_int_10i8(surfpetot_viewer, surftyp_viewer)
!
      write(fmt_txt,'(a5,i2,a9)')                                       &
     &                '(i10,', nnod_4_surf, '(i10),a1)'
      do i = 1, surfpetot_viewer
        write(textbuf,fmt_txt) i, ie_sf_viewer(i,1:nnod_4_surf),        &
     &                        char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      end subroutine write_surf_connect_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_surf_connect_viewer_gz
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_node_quad_2_linear_sf
!
      integer(kind = kint) :: i, itmp
      integer(kind = kint) :: nchara
!
!
      call skip_gz_comment_int(itmp)
!
      call allocate_surf_type_viewer
!
      call read_gz_multi_int(surfpetot_viewer, surftyp_viewer)
!
      if ( surftyp_viewer(1).eq.223) then
        nnod_4_surf = num_lag_sf
        nnod_4_edge = num_quad_edge
        call allocate_quad4_2_linear_tri
      else if ( surftyp_viewer(1).eq.222) then
        nnod_4_surf = num_quad_sf
        nnod_4_edge = num_quad_edge
        call allocate_quad8_2_linear_tri
      else if ( surftyp_viewer(1).eq.221) then
        nnod_4_surf = num_linear_sf
        nnod_4_edge = num_linear_edge
        call allocate_quad9_2_linear_tri
      end if
!
      call allocate_surf_connect_viewer
!
      do i = 1, surfpetot_viewer
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) itmp, ie_sf_viewer(i,1:nnod_4_surf)
      end do
!
      end subroutine read_surf_connect_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_edge_connect_viewer_gz
!
      use m_geometry_constants
      use m_geometry_parameter
!
      integer(kind = kint) :: i
      character(len=kchara) :: fmt_txt
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  edge information', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  edge type', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  global ID, connectivity', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(i10,a1)') edgepetot_viewer, char(0)
      call write_compress_txt(nbuf, textbuf)
!
!
      write(fmt_txt,'(a5,i2,a9)')                                       &
     &                '(i10,', nnod_4_edge, '(i10),a1)'
      do i = 1, edgepetot_viewer
        write(textbuf,fmt_txt) i, ie_edge_viewer(i,1:nnod_4_edge),      &
     &                         char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  edge ID for surfaces', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(i10,a1)') surfpetot_viewer, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(fmt_txt,'(a5,i2,a9)')                                       &
     &                '(i10,', nedge_4_surf, '(i10),a1)'
      do i = 1, surfpetot_viewer
        write(textbuf,fmt_txt) i, iedge_sf_viewer(i,1:nedge_4_surf),    &
     &                           char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      end subroutine write_edge_connect_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_edge_connect_viewer_gz
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_node_quad_2_linear_sf
!
      integer(kind = kint) :: i, itmp
      integer(kind = kint) :: nchara
!
!
      call skip_gz_comment_int(itmp)
!
      call allocate_edge_data_4_sf
!
      do i = 1, edgepetot_viewer
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) itmp, ie_edge_viewer(i,1:nnod_4_edge)
      end do
!
      call skip_gz_comment_int(itmp)
!
      do i = 1, surfpetot_viewer
       call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
       read(textbuf,*) itmp, iedge_sf_viewer(i,1:nedge_4_surf)
      end do
!
      end subroutine read_edge_connect_viewer_gz
!
!------------------------------------------------------------------
!
      end module gz_viewer_mesh_data_IO
