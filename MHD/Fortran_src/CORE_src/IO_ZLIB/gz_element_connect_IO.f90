!gz_element_connect_IO.f90
!      module gz_element_connect_IO
!
!     Written by H. Matsui on Oct., 2006
!
!
!      subroutine write_element_info_gz
!
!      subroutine read_number_of_element_gz
!      subroutine read_element_info_gz
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
      write(textbuf,'(i16,a1)') ele_IO%numele, char(0)
      call gz_write_textbuf_w_lf
      call write_gz_multi_int_10i8(ele_IO%numele, ele_IO%elmtyp)
!
      do i=1, ele_IO%numele
        write(fmt_txt,'(a5,i3,a7)')                                     &
     &         '(i16,', ele_IO%nodelm(i), 'i16,a1)'
        write(textbuf,fmt_txt) ele_IO%iele_global(i),                   &
     &         ie_dummy(i,1:ele_IO%nodelm(i)), char(0)
        call gz_write_textbuf_w_lf
      end do
!
      call deallocate_ele_info_dummy
!
      end subroutine write_element_info_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_number_of_element_gz
!
!
      call skip_gz_comment_int(ele_IO%numele)
!       write(*,*) ele_IO%numele
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
       call read_gz_multi_int(ele_IO%numele, ele_IO%elmtyp)
!
       ele_IO%nnod_4_ele = 0
       do i = 1, ele_IO%numele
         call s_set_nnod_4_ele_by_type                                  &
     &      (ele_IO%elmtyp(i), ele_IO%nodelm(i))
         ele_IO%nnod_4_ele = max(ele_IO%nnod_4_ele,ele_IO%nodelm(i))
       end do
!
       call allocate_connect_dummy
!
       do i=1, ele_IO%numele
        call get_one_line_from_gz_f
        read(textbuf,*) ele_IO%iele_global(i),                          &
     &                 ie_dummy(i,1:ele_IO%nodelm(i))
       end do
!
       end subroutine read_element_info_gz
!
!------------------------------------------------------------------
!
      end module gz_element_connect_IO
