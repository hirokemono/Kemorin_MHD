!
!      module copy_1darray_2_2darray
!
!        programmed by H. Matsui on June, 2007
!
!     subroutine s_copy_1darray_2_2darray(n1, n2, x2, x1)
!
      module copy_1darray_2_2darray
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_copy_1darray_2_2darray(n1, n2, x2, x1)
!
      integer(kind = kint) :: n1, n2
      real(kind = kreal), dimension(n1,n2) :: x2
      real(kind = kreal), dimension(n1*n2) :: x1
!
      integer(kind = kint) :: i, j
!
      do j = 1, n2
        do i = 1, n1
          x2(i,j) = x1( n1*(j-1)+i )
        end do
      end do
!
!
      end subroutine s_copy_1darray_2_2darray
!
!-----------------------------------------------------------------------
!
      end module copy_1darray_2_2darray
