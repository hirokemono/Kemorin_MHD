!>@file   t_matrix_4_LU.f90
!!@brief  module t_matrix_4_LU
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief  Structure for linear solver by LU decomposition
!!
!!@verbatim
!!      subroutine alloc_matrix_4_LU(num, LU_mat)
!       subroutine dealloc_matrix_4_LU(LU_mat)
!!        integer(kind = kint), intent(in) :: num
!!        type(matrix_4_LU), intent(inout) :: LU_mat
!!@endverbatim
!
      module t_matrix_4_LU
!
      use m_precision
!
      implicit none
!
      type matrix_4_LU
!>         Matrix A
        real(kind = kreal), allocatable ::  a_nod(:,:)
!>         Matrix A
        real(kind = kreal), allocatable ::  b_nod(:)
!>         detamenant of the matrix
        real(kind = kreal) :: d_nod
!
!>         Matrix A
        integer(kind = kint) :: ncomp_lu
!>         Pibots data for LU decomposition
        integer(kind = kint), allocatable ::  indx(:)
      end type matrix_4_LU
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_matrix_4_LU(num, LU_mat)
!
      integer(kind = kint), intent(in) :: num
      type(matrix_4_LU), intent(inout) :: LU_mat
!
!
      LU_mat%ncomp_lu = num
      allocate(LU_mat%a_nod(LU_mat%ncomp_lu, LU_mat%ncomp_lu))
      allocate(LU_mat%b_nod(LU_mat%ncomp_lu))
      allocate(LU_mat%indx(LU_mat%ncomp_lu))
!
      if(LU_mat%ncomp_lu .le. 0) return
      LU_mat%a_nod(1:LU_mat%ncomp_lu, 1:LU_mat%ncomp_lu) = 0.0d0
      LU_mat%b_nod(1:LU_mat%ncomp_lu) = 0.0d0
      LU_mat%indx(1:LU_mat%ncomp_lu) =  0
!
      end subroutine alloc_matrix_4_LU
!
!  ---------------------------------------------------------------------
!
       subroutine dealloc_matrix_4_LU(LU_mat)
!
      type(matrix_4_LU), intent(inout) :: LU_mat
!
        deallocate(LU_mat%a_nod)
        deallocate(LU_mat%b_nod)
        deallocate(LU_mat%indx)
!
      end subroutine dealloc_matrix_4_LU
!
!  ---------------------------------------------------------------------
!
      subroutine solve_by_LU_decomp(NB, A_in, RHS_in, X_lu, LU_mat)
!
      use m_ludcmp
!
      integer(kind = kint), intent(in) :: NB
      real(kind = kreal), intent(in) :: A_in(NB,NB)
      real(kind = kreal), intent(in) :: RHS_in(NB)
!
      real(kind = kreal), intent(inout) :: X_lu(NB)
      type(matrix_4_LU), intent(inout) :: LU_mat
!
      integer(kind = kint) :: i, j
!
!
!$omp parallel do private(i,j)
      do i = 1, NB
        do j = 1, NB
          LU_mat%a_nod(j,i) = A_in(i,j)
        end do
      end do
!$omp end parallel do
!$omp parallel workshare
      LU_mat%b_nod(1:NB) = RHS_in(1:NB)
!$omp end parallel workshare
!
!c decompose A = LU
      call ludcmp(LU_mat%a_nod, LU_mat%ncomp_lu, LU_mat%ncomp_lu,       &
     &            LU_mat%indx, LU_mat%d_nod)
!
      LU_mat%d_nod = LU_mat%a_nod(1,1)
      do i = 2, NB
        LU_mat%d_nod = LU_mat%d_nod * LU_mat%a_nod(i,i)
      end do
      write(*,*) 'det A', LU_mat%d_nod
!
!c solve Ax=LUx=b
      call lubksb(LU_mat%a_nod, LU_mat%ncomp_lu, LU_mat%ncomp_lu,       &
     &            LU_mat%indx, LU_mat%b_nod)
!
!$omp parallel workshare
      X_lu(1:NB) = LU_mat%b_nod(1:NB)
!$omp end parallel workshare
!
      end subroutine solve_by_LU_decomp
!
!  ---------------------------------------------------------------------
!
      end module t_matrix_4_LU
