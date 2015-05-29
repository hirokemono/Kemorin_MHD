!
!      module gz_node_geometry_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine write_geometry_info_gz
!      subroutine write_scalar_in_element_gz
!      subroutine write_vector_in_element_gz
!
!      subroutine read_number_of_node_gz
!      subroutine read_geometry_info_gz
!      subroutine read_scalar_in_element_gz
!      subroutine read_vector_in_element_gz
!
      module gz_node_geometry_IO
!
      use m_precision
!
      use m_read_mesh_data
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
      subroutine write_geometry_info_gz
!
      integer (kind = kint) :: i
!
!
      write(textbuf,'(2i16,a1)') numnod_dummy, internal_node_dummy,     &
     &      char(0)
      call gz_write_textbuf_w_lf
!
      do i=1, numnod_dummy
        write(textbuf,'(i16,1p3E25.15e3,a1)')  globalnodid_dummy(i),    &
     &        xx_dummy(i,1:3), char(0)
        call gz_write_textbuf_w_lf
      end do
!
      call deallocate_node_data_dummy
!
      end subroutine write_geometry_info_gz
!
!------------------------------------------------------------------
!
      subroutine write_scalar_in_element_gz
!
      integer(kind = kint) :: i
!
      write(textbuf,'(2i16,a1)') numnod_dummy, internal_node_dummy,     &
     &      char(0)
      call gz_write_textbuf_w_lf
      do i = 1, numnod_dummy
        write(textbuf,'(i16,1p3E25.15e3,a1)') i, ele_scalar_IO(i),      &
     &      char(0)
        call gz_write_textbuf_w_lf
      end do
!
      call deallocate_ele_scalar_IO
!
      end subroutine write_scalar_in_element_gz
!
!------------------------------------------------------------------
!
      subroutine write_vector_in_element_gz
!
      integer(kind = kint) :: i
!
      write(textbuf,'(2i16,a1)') numnod_dummy, internal_node_dummy,     &
     &      char(0)
      call gz_write_textbuf_w_lf
      do i = 1, numnod_dummy
        write(textbuf,'(i16,1p3E25.15e3,a1)') i, ele_vector_IO(i,1:3),  &
     &         char(0)
        call gz_write_textbuf_w_lf
      end do
!
      call deallocate_ele_vector_IO
!
      end subroutine write_vector_in_element_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_number_of_node_gz
!
!
      call skip_gz_comment_int(numnod_dummy)
      read(textbuf,*) numnod_dummy, internal_node_dummy
!
      end subroutine read_number_of_node_gz
!
!------------------------------------------------------------------
!
      subroutine read_geometry_info_gz
!
      integer (kind = kint) :: i, k
!
!
      call allocate_node_data_dummy
!
      do i=1, numnod_dummy
        call get_one_line_from_gz_f
        read(textbuf,*)  globalnodid_dummy(i), (xx_dummy(i,k),k=1,3)
      end do
!
      end subroutine read_geometry_info_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_scalar_in_element_gz
!
      integer(kind = kint) :: i, itmp
!
!
      call read_number_of_node_gz
!
      call allocate_ele_scalar_IO
!
      do i = 1, numnod_dummy
        call get_one_line_from_gz_f
        read(textbuf,*) itmp, ele_scalar_IO(i)
      end do
!
      end subroutine read_scalar_in_element_gz
!
!------------------------------------------------------------------
!
      subroutine read_vector_in_element_gz
!
      integer(kind = kint) :: i, itmp
!
!
      call read_number_of_node_gz
!
      call allocate_ele_vector_IO
!
      do i = 1, numnod_dummy
        call get_one_line_from_gz_f
        read(textbuf,*) itmp, ele_vector_IO(i,1:3)
      end do
!
      end subroutine read_vector_in_element_gz
!
!------------------------------------------------------------------
!
      end module gz_node_geometry_IO
