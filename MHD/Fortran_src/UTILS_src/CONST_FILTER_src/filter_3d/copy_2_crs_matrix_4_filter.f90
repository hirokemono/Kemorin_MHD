!
!      module copy_2_crs_matrix_4_filter
!
      module copy_2_crs_matrix_4_filter
!
!     Written by H. Matsui on Aug., 2006
!
      use m_precision
!
      implicit none
!
!
!      subroutine s_copy_2_crs_matrix_4_filter
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_copy_2_crs_matrix_4_filter
!
      use m_crs_matrix_4_filter
      use m_matrix_4_filter
!
      integer(kind = kint) :: i, j ,inum
!
!
      n_crs =       mat_size
      n_inter_crs = mat_size
      npl_crs = mat_size * (mat_size - 1) / 2
      npu_crs = mat_size * (mat_size - 1) / 2
!
      istack_l_crs = 0
      istack_u_crs = 0
      do i = 1, mat_size
        istack_l_crs(i) = istack_l_crs(i-1) + i - 1
        istack_u_crs(i) = istack_u_crs(i-1) + mat_size - i
      end do
!
      item_l_crs = 0
      item_u_crs = 0
      al_mat = 0.0d0
      au_mat = 0.0d0
      diag_mat = 0.0d0
!
      do i = 1, mat_size
        do j = 1, i-1
          inum = istack_l_crs(i-1) + j
          item_l_crs(inum) = j
          al_mat(inum) = a_mat(i,j)
        end do
        do j = 1, mat_size - i
          inum = istack_u_crs(i-1) + j
          item_u_crs(inum) = i + j
          au_mat(inum) = a_mat(i,i+j)
        end do
      end do
!
      do i = 1, mat_size
        diag_mat(i) = a_mat(i,i)
      end do
!
      end subroutine s_copy_2_crs_matrix_4_filter
!
!-----------------------------------------------------------------------
!
      end module copy_2_crs_matrix_4_filter
