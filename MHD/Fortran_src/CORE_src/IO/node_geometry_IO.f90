!>@file   node_geometry_IO.f90
!!@brief  module node_geometry_IO
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on  Aug., 2006
!
!>@brief  routines for ASCII data IO for mesh geometry
!!
!!@verbatim
!!      function each_node_data_buffer(inod_gl, xx)
!!      subroutine read_each_node_data_buffer(textbuf, inod_gl, vect)
!!
!!      subroutine write_geometry_info(id_file)
!!      subroutine write_scalar_in_element(id_file)
!!      subroutine write_vector_in_element(id_file)
!!
!!      subroutine read_number_of_node(id_file)
!!      subroutine read_geometry_info(id_file)
!!@endverbatim
!
      module node_geometry_IO
!
      use m_precision
!
      use m_read_mesh_data
!
      implicit none
!
      integer(kind = kint), parameter                                   &
     &             :: len_each_node_data_buf = 16 + 3*25 + 1
!
      character(len=255) :: character_4_read
      private :: character_4_read
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      function each_node_data_buffer(inod_gl, xx)
!
      integer(kind = kint_gl), intent(inout) :: inod_gl
      real(kind = kreal), intent(in) :: xx(3)
!
      character(len_each_node_data_buf) :: each_node_data_buffer
!
!
      write(each_node_data_buffer,'(i16,1p3E25.15e3,a1)')               &
     &      inod_gl, xx(1:3), char(10)
!
      end function each_node_data_buffer
!
! -------------------------------------------------------------------
!
      subroutine read_each_node_data_buffer(textbuf, inod_gl, vect)
!
      character(len=len_each_node_data_buf), intent(in) :: textbuf
      integer(kind = kint_gl), intent(inout) :: inod_gl
      real(kind = kreal), intent(inout) :: vect(3)
!
      read(textbuf,*) inod_gl, vect(1:3)
!
      end subroutine read_each_node_data_buffer
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine write_geometry_info(id_file)
!
      integer (kind = kint), intent(in) :: id_file
      integer (kind = kint) :: i
!
!
      write(id_file,'(2i16)') nod_IO%numnod, nod_IO%internal_node
!
      do i=1, nod_IO%numnod
        write(id_file,'(i16,1p3E25.15e3)')  nod_IO%inod_global(i),      &
     &        nod_IO%xx(i,1:3)
      end do
!
      call dealloc_node_geometry_base(nod_IO)
!
      end subroutine write_geometry_info
!
!------------------------------------------------------------------
!
      subroutine write_scalar_in_element(id_file)
!
      integer (kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
      write(id_file,'(2i16)') nod_IO%numnod, nod_IO%internal_node
      do i = 1, nod_IO%numnod
        write(id_file,'(i16, 1p3e23.15)') i, sfed_IO%ele_scalar(i)
      end do
!
      call dealloc_ele_scalar_IO(sfed_IO)
!
      end subroutine write_scalar_in_element
!
!------------------------------------------------------------------
!
      subroutine write_vector_in_element(id_file)
!
      integer (kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
      write(id_file,'(2i16)') nod_IO%numnod, nod_IO%internal_node
      do i = 1, nod_IO%numnod
        write(id_file,'(i16,1p3e23.15)') i, sfed_IO%ele_vector(i,1:3)
      end do
!
      call dealloc_ele_vector_IO(sfed_IO)
!
      end subroutine write_vector_in_element
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_number_of_node(id_file)
!
      use skip_comment_f
      integer (kind = kint), intent(in) :: id_file
!
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) nod_IO%numnod, nod_IO%internal_node
!      write(*,*) nod_IO%numnod, nod_IO%internal_node
!
      end subroutine read_number_of_node
!
!------------------------------------------------------------------
!
      subroutine read_geometry_info(id_file)
!
      integer (kind = kint), intent(in) :: id_file
      integer (kind = kint) :: i, k
!
!
      call alloc_node_geometry_base(nod_IO)
!
      do i=1, nod_IO%numnod
        read(id_file,*)  nod_IO%inod_global(i), (nod_IO%xx(i,k),k=1,3)
      end do
!
      end subroutine read_geometry_info
!
!------------------------------------------------------------------
!
      end module node_geometry_IO
