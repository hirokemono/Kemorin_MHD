!
!      module modify_matrix_and_rhs
!
!        programmed by H.Matsui on Nov., 2006
!
!      subroutine swap_matrix_by_diag_size(n, nmat, b_vec, a_mat)
!      subroutine check_diagonal(n, nmat, a_mat, ierr)
!         if there is 0 in diagonal, ierr = 1
!      subroutine scaling_by_diagonal(n, nmat, b_vec, a_mat)
!
      module modify_matrix_and_rhs
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
      subroutine swap_matrix_by_diag_size(n, nmat, b_vec, a_mat)
!
      integer(kind = kint), intent(in) :: n, nmat
!
      real(kind = kreal), intent(inout) :: b_vec(n)
      real(kind = kreal), intent(inout) :: a_mat(nmat,nmat)
!
      integer(kind = kint) :: i, j, i_max
      real(kind= kreal) :: work
!
      do i = 1, n-1
        i_max = i
        do j = i, n
          if ( abs(a_mat(j,i)) .gt. abs(a_mat(i_max,i)) ) i_max = j
        end do
!
        work =     b_vec(i)
        b_vec(i) = b_vec(i_max)
        b_vec(i_max) = work
!
        do j = 1, n
          work =           a_mat(i,j)
          a_mat(i,j) =     a_mat(i_max,j)
          a_mat(i_max,j) = work
        end do
!
      end do
!
      end subroutine swap_matrix_by_diag_size
!
!-----------------------------------------------------------------------
!
      subroutine check_diagonal(n, nmat, a_mat, ierr)
!
      integer(kind = kint), intent(in) :: n, nmat
!
      integer(kind = kint), intent(inout) :: ierr
      real(kind = kreal), intent(inout) :: a_mat(nmat,nmat)
!
      integer(kind = kint) :: i
!
      ierr = 0
      do i = 1, n
        if ( a_mat(i,i) .eq. 0.0d0) ierr = 1
      end do
!
      end subroutine check_diagonal
!
!-----------------------------------------------------------------------
!
      subroutine scaling_by_diagonal(n, nmat, b_vec, a_mat)
!
      integer(kind = kint), intent(in) :: n, nmat
!
      real(kind = kreal), intent(inout) :: b_vec(n)
      real(kind = kreal), intent(inout) :: a_mat(nmat,nmat)
!
      integer(kind = kint) :: i
      real(kind= kreal) :: diag
!
!
      do i = 1, n
        diag = a_mat(i,i)
        b_vec(i) = b_vec(i) / diag
        a_mat(i,1:n) = a_mat(i,1:n) / diag
      end do
!
      end subroutine scaling_by_diagonal
!
!-----------------------------------------------------------------------
!
      end module modify_matrix_and_rhs
