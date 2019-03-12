!
!      module m_matrix_4_filter
!
      module m_matrix_4_filter
!
!     Written by H. Matsui on Aug., 2006
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: max_mat_size
      integer(kind = kint) :: mat_size
!
      real(kind = kreal), allocatable :: a_mat(:,:)
!
      real(kind = kreal), allocatable :: vec_mat(:)
!
      real(kind = kreal), allocatable :: x_sol(:)
      real(kind = kreal) :: det_mat, vec_norm, ratio_vec_mat
!
      integer(kind = kint), allocatable :: indx_mat(:)
      real(kind = kreal), allocatable :: mat_work(:)
!
!      subroutine allocate_matrix_4_filter
!      subroutine deallocate_matrix_4_filter
!
!      subroutine copy_rhs_2_solution
!
!      subroutine check_matrix_4_filter(id_rank)
!      subroutine check_rhs_4_filter(id_rank)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_matrix_4_filter
!
      allocate(a_mat(max_mat_size, max_mat_size))
      allocate(vec_mat(max_mat_size))
      allocate(x_sol(max_mat_size))
      allocate(indx_mat(max_mat_size))
!
      allocate( mat_work(max_mat_size*(max_mat_size+1)) )
!
      a_mat = 0.0d0
      vec_mat = 0.0d0
      x_sol = 0.0d0
!
      indx_mat = 0
      mat_work = 0.0d0
!
      end subroutine allocate_matrix_4_filter
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_matrix_4_filter
!
      deallocate(a_mat)
      deallocate(vec_mat)
      deallocate(x_sol)
      deallocate(indx_mat)
      deallocate( mat_work )
!
      end subroutine deallocate_matrix_4_filter
!
!-----------------------------------------------------------------------
!
      subroutine copy_rhs_2_solution
!
      x_sol(1:max_mat_size) = vec_mat(1:max_mat_size)
!
      end subroutine copy_rhs_2_solution
!
!-----------------------------------------------------------------------
!
      subroutine check_matrix_4_filter(id_rank)
!
      integer(kind = kint), intent(in) :: id_rank
      integer(kind = kint) :: i, j
!
      write(50+id_rank,*) 'size of matrix', mat_size
      do i = 1, mat_size
        do j = 1, mat_size
          write(50+id_rank,*) i, j, a_mat(i,j)
        end do
      end do
!
      end subroutine check_matrix_4_filter
!
!-----------------------------------------------------------------------
!
      subroutine check_rhs_4_filter(id_rank)
!
      integer(kind = kint), intent(in) :: id_rank
      integer(kind = kint) :: i
!
      write(50+id_rank,*) 'size of RHS vector', mat_size
      do i = 1, mat_size
          write(50+id_rank,*) i, vec_mat(i)
      end do
!
      end subroutine check_rhs_4_filter
!
!-----------------------------------------------------------------------
!
      end module m_matrix_4_filter
