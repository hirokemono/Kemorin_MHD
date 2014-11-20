!gz_element_connect_IO.f90
!      module gz_element_connect_IO
!
!     Written by H. Matsui on Oct., 2006
!
!
!
!      subroutine write_element_info_gz
!      subroutine write_surface_4_element_gz
!      subroutine write_edge_4_element_gz
!
!      subroutine read_number_of_element_gz
!      subroutine read_element_info_gz
!      subroutine read_surface_4_element_gz
!      subroutine read_edge_4_element_gz
!
      module gz_element_connect_IO
!
      use m_precision
!
      use m_read_mesh_data
      use skip_gz_comment
!
      implicit none
!
      character(len=kchara), private :: fmt_txt
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine write_element_info_gz
!
      integer (kind = kint) :: i
!
!
      write(textbuf,'(i16,a1)') numele_dummy, char(0)
      call gz_write_textbuf_f
      call write_gz_multi_int_10i8(numele_dummy, i_ele_dummy)
!
      do i=1, numele_dummy
        write(fmt_txt,'(a5,i3,a7)') '(i16,', nodelm_dummy(i), 'i16,a1)'
        write(textbuf,fmt_txt) globalelmid_dummy(i),                    &
     &         ie_dummy(i,1:nodelm_dummy(i)), char(0)
        call gz_write_textbuf_f
      end do
!
      call deallocate_ele_info_dummy
!
      end subroutine write_element_info_gz
!
!------------------------------------------------------------------
!
      subroutine write_surface_4_element_gz
!
      integer(kind = kint) :: i
!
      write(textbuf,'(2i16,a1)') nsf_4_ele_IO, nsurf_in_ele_IO, char(0)
      call gz_write_textbuf_f
!
      write(fmt_txt,'(a5,i3,a7)') '(i16,', nsurf_in_ele_IO, 'i16,a1)'
      do i = 1, nsf_4_ele_IO
        write(textbuf,fmt_txt)                                          &
    &         i, isf_4_ele_IO(i,1:nsurf_in_ele_IO), char(0)
        call gz_write_textbuf_f
      end do
!
      call deallocate_surface_connect_IO
!
      end subroutine write_surface_4_element_gz
!
!------------------------------------------------------------------
!
      subroutine write_edge_4_element_gz
!
      integer(kind = kint) :: i
!
!
      write(textbuf,'(2i16,a1)') ned_4_ele_IO, nedge_in_ele_IO, char(0)
      call gz_write_textbuf_f
!
      write(fmt_txt,'(a5,i3,a7)') '(i16,', nedge_in_ele_IO, 'i16,a1)'
      do i = 1, ned_4_ele_IO
        write(textbuf,fmt_txt)                                          &
     &        i, iedge_4_ele_IO(i,1:nedge_in_ele_IO), char(0)
        call gz_write_textbuf_f
      end do
!
      call deallocate_edge_connect_IO
!
      end subroutine write_edge_4_element_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_number_of_element_gz
!
!
      call skip_gz_comment_int(numele_dummy)
!       write(*,*) numele_dummy
!
      end subroutine read_number_of_element_gz
!
!------------------------------------------------------------------
!
       subroutine read_element_info_gz
!
       use set_nnod_4_ele_by_type
!
       integer (kind = kint) :: i
!
!
       call allocate_ele_info_dummy
       call read_gz_multi_int(numele_dummy, i_ele_dummy)
!
       nnod_4_ele_dummy = 0
       do i = 1, numele_dummy
         call s_set_nnod_4_ele_by_type(nodelm_dummy(i), i_ele_dummy(i))
         nnod_4_ele_dummy = max(nnod_4_ele_dummy,nodelm_dummy(i))
       end do
!
       call allocate_connect_dummy
!
       do i=1, numele_dummy
        call get_one_line_from_gz_f
        read(textbuf,*) globalelmid_dummy(i),                           &
     &                 ie_dummy(i,1:nodelm_dummy(i))
       end do
!
       end subroutine read_element_info_gz
!
!------------------------------------------------------------------
!
      subroutine read_surface_4_element_gz
!
      integer (kind = kint) :: i, itmp
!
!
      call skip_gz_comment_int(nsf_4_ele_IO)
      read(textbuf,*) nsf_4_ele_IO, nsurf_in_ele_IO
!
      call allocate_surface_connect_IO
!
      do i = 1, nsf_4_ele_IO
        call get_one_line_from_gz_f
        read(textbuf,*) itmp, isf_4_ele_IO(i,1:nsurf_in_ele_IO)
      end do
!
      end subroutine read_surface_4_element_gz
!
!------------------------------------------------------------------
!
      subroutine read_edge_4_element_gz
!
      integer(kind = kint) :: i, itmp
!
      call skip_gz_comment_int(ned_4_ele_IO)
      read(textbuf,*) ned_4_ele_IO, nedge_in_ele_IO
!
!
      call allocate_edge_connect_IO
!
      do i = 1, ned_4_ele_IO
        call get_one_line_from_gz_f
        read(textbuf,*) itmp, iedge_4_ele_IO(i,1:nedge_in_ele_IO)
      end do
!
      end subroutine read_edge_4_element_gz
!
!------------------------------------------------------------------
!
      end module gz_element_connect_IO
