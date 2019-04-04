!
!      module t_matrix_4_filter
!
!     Written by H. Matsui on Aug., 2006
!
!!      subroutine alloc_matrix_4_filter(fil_mat)
!!      subroutine dealloc_matrix_4_filter(fil_mat)
!!        type(matrix_4_filter), intent(inout) :: fil_mat
!!
!!      subroutine copy_rhs_2_solution(fil_mat)
!!        type(matrix_4_filter), intent(inout) :: fil_mat
!!
!!      subroutine check_matrix_4_filter(id_rank, fil_mat)
!!      subroutine check_rhs_4_filter(id_rank, fil_mat)
!!        type(matrix_4_filter), intent(in) :: fil_mat
!
      module t_matrix_4_filter
!
      use m_precision
      use t_crs_connect
      use t_crs_matrix
!
      implicit none
!
      type matrix_4_filter
        integer(kind = kint) :: max_mat_size
        integer(kind = kint) :: mat_size
!
        real(kind = kreal), allocatable :: a_mat(:,:)
!
        real(kind = kreal), allocatable :: vec_mat(:)
!
        real(kind = kreal), allocatable :: x_sol(:)
        real(kind = kreal) :: det_mat
        real(kind = kreal) :: vec_norm
        real(kind = kreal) :: ratio_vec_mat
!
        integer(kind = kint), allocatable :: indx_mat(:)
!
        integer(kind = kint) :: num_work
        real(kind = kreal), allocatable :: mat_work(:)
      end type matrix_4_filter
!
      type matrices_4_filter
        type(matrix_4_filter) :: fil_mat
!
        type(CRS_matrix_connect) :: fil_tbl_crs
        type(CRS_matrix) :: fil_mat_crs
      end type matrices_4_filter
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_matrix_4_filter(fil_mat)
!
      type(matrix_4_filter), intent(inout) :: fil_mat
!
      integer(kind = kint) :: num
!
      num = fil_mat%max_mat_size
      allocate(fil_mat%a_mat(num, num))
      allocate(fil_mat%vec_mat(num))
      allocate(fil_mat%x_sol(num))
      allocate(fil_mat%indx_mat(num))
!
      fil_mat%num_work = num * (num + 1)
      allocate(fil_mat%mat_work(fil_mat%num_work))
!
      fil_mat%a_mat = 0.0d0
      fil_mat%vec_mat = 0.0d0
      fil_mat%x_sol = 0.0d0
!
      fil_mat%indx_mat = 0
      fil_mat%mat_work = 0.0d0
!
      end subroutine alloc_matrix_4_filter
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_matrix_4_filter(fil_mat)
!
      type(matrix_4_filter), intent(inout) :: fil_mat
!
      deallocate(fil_mat%a_mat)
      deallocate(fil_mat%vec_mat)
      deallocate(fil_mat%x_sol)
      deallocate(fil_mat%indx_mat)
      deallocate(fil_mat%mat_work)
!
      end subroutine dealloc_matrix_4_filter
!
!-----------------------------------------------------------------------
!
      subroutine copy_rhs_2_solution(fil_mat)
!
      type(matrix_4_filter), intent(inout) :: fil_mat
!
      fil_mat%x_sol(1:fil_mat%max_mat_size)                             &
     &         = fil_mat%vec_mat(1:fil_mat%max_mat_size)
!
      end subroutine copy_rhs_2_solution
!
!-----------------------------------------------------------------------
!
      subroutine check_matrix_4_filter(id_rank, fil_mat)
!
      integer, intent(in) :: id_rank
      type(matrix_4_filter), intent(in) :: fil_mat
!
      integer(kind = kint) :: i, j
!
      write(50+id_rank,*) 'size of matrix', fil_mat%mat_size
      do i = 1, fil_mat%mat_size
        do j = 1, fil_mat%mat_size
          write(50+id_rank,*) i, j, fil_mat%a_mat(i,j)
        end do
      end do
!
      end subroutine check_matrix_4_filter
!
!-----------------------------------------------------------------------
!
      subroutine check_rhs_4_filter(id_rank, fil_mat)
!
      integer, intent(in) :: id_rank
      type(matrix_4_filter), intent(in) :: fil_mat
!
      integer(kind = kint) :: i
!
      write(50+id_rank,*) 'size of RHS vector', fil_mat%mat_size
      do i = 1, fil_mat%mat_size
          write(50+id_rank,*) i, fil_mat%vec_mat(i)
      end do
!
      end subroutine check_rhs_4_filter
!
!-----------------------------------------------------------------------
!
      end module t_matrix_4_filter
