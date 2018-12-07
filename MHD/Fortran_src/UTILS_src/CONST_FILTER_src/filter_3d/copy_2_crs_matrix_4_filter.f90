!
!      module copy_2_crs_matrix_4_filter
!     Written by H. Matsui on Aug., 2006
!
!!      subroutine s_copy_2_crs_matrix_4_filter                         &
!!     &         (fil_tbl_crs, fil_mat_crs)
!
!
      module copy_2_crs_matrix_4_filter
!
      use m_precision
!
      use t_crs_connect
      use t_crs_matrix
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_copy_2_crs_matrix_4_filter                           &
     &         (fil_tbl_crs, fil_mat_crs)
!
      use m_matrix_4_filter
!
      type(CRS_matrix_connect), intent(inout) :: fil_tbl_crs
      type(CRS_matrix), intent(inout) :: fil_mat_crs
!
      integer(kind = kint) :: i, j ,inum
!
!
      fil_tbl_crs%ntot_d = mat_size
      fil_tbl_crs%ntot_l = mat_size * (mat_size - 1) / 2
      fil_tbl_crs%ntot_u = mat_size * (mat_size - 1) / 2
!
      fil_tbl_crs%istack_l(0) = 0
      fil_tbl_crs%istack_u(0) = 0
      do i = 1, mat_size
        fil_tbl_crs%nitem_l(i) = i - 1
        fil_tbl_crs%nitem_u(i) = mat_size - i
        fil_tbl_crs%istack_l(i) = fil_tbl_crs%istack_l(i-1) + i - 1
        fil_tbl_crs%istack_u(i) = fil_tbl_crs%istack_u(i-1)             &
      &                           + mat_size - i
      end do
!
      fil_tbl_crs%item_l = 0
      fil_tbl_crs%item_u = 0
      fil_mat_crs%AL_crs = 0.0d0
      fil_mat_crs%AU_crs = 0.0d0
      fil_mat_crs%D_crs =  0.0d0
!
      do i = 1, mat_size
        do j = 1, i-1
          inum = fil_tbl_crs%istack_l(i-1) + j
          fil_tbl_crs%item_l(inum) = j
          fil_mat_crs%AL_crs(inum) = a_mat(i,j)
        end do
        do j = 1, mat_size - i
          inum = fil_tbl_crs%istack_u(i-1) + j
          fil_tbl_crs%item_u(inum) = i + j
          fil_mat_crs%AU_crs(inum) = a_mat(i,i+j)
        end do
      end do
!
      do i = 1, mat_size
        fil_mat_crs%D_crs(i) = a_mat(i,i)
      end do
!
      end subroutine s_copy_2_crs_matrix_4_filter
!
!-----------------------------------------------------------------------
!
      end module copy_2_crs_matrix_4_filter
