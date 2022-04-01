!>@file   t_fdm_coefs.f90
!!@brief  module t_fdm_coefs
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  Coefficients to obtain radial derivatives
!!        by finite difference method
!!
!!@verbatim
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!     define of elemental field
!!       r_ele(k) = half *(r_nod(k-1) + r_nod(k))
!!       d_ele(k) = half *(d_nod(k-1) + d_nod(k))
!!
!!    2nd order derivatives on node by nodal field
!!      dfdr =    fdmn_nod%fdm(1)%dmat(k,-1) * d_nod(k-1)
!!              + fdmn_nod%fdm(1)%dmat(k, 0) * d_nod(k  )
!!              + fdmn_nod%fdm(1)%dmat(k, 1) * d_nod(k+1)
!!      d2fdr2 =  fdmn_nod%fdm(2)%dmat(k,-1) * d_nod(k-1)
!!              + fdmn_nod%fdm(2)%dmat(k, 0) * d_nod(k  )
!!              + fdmn_nod%fdm(2)%dmat(k, 1) * d_nod(k+1)
!! ----------------------------------------------------------------------
!!
!!      subroutine alloc_nod_fdm_matrices                               &
!!     &         (nri, num_order, n_minus, n_plus, fdmn_nod)
!!      subroutine alloc_fdm_work(nri, fdmn_nod)
!!      subroutine dealloc_nod_fdm_matrices(fdmn_nod)
!!      subroutine dealloc_fdm_work(fdmn_nod)
!!        integer(kind = kint), intent(in) :: nri, num_order
!!        integer(kind = kint), intent(in) :: n_minus, n_plus
!!        type(fdm_matrices), intent(inout) :: fdmn_nod
!!
!!      subroutine copy_fdm2_nod_coefs_from_mat(nri, fdm2_nod)
!!      subroutine copy_fdm4_nod_coefs_from_mat(nri, fdm4_nod)
!!      subroutine copy_fdm2_ele_coefs_from_mat(nri, fdm2_ele)
!!
!!      subroutine check_fdm_coefs(nri, r, fdmn_nod)
!!      subroutine check_fdm_mat(nri, r, fdmn_nod)
!!@endverbatim
!!
!!@n @param nri    number of radial grid points
!!@n @param r(nri) radius
!
      module t_fdm_coefs
!
      use m_precision
!
      implicit none
!
!
!>        Structure of FDM matrix
      type fdm_matrix
!>        Number of radial points
        integer(kind = kint) :: nri_mat
!>        Width of matrix (positive side)
        integer(kind = kint) :: n_plus
!>        Width of matrix (negative side)
        integer(kind = kint) :: n_minus
!>        Coefficients to evaluate radial derivative
!!        from nodal field by FDM
        real(kind = kreal), allocatable :: dmat(:,:)
      end type fdm_matrix
!
!>        Structure of FDM matrices
      type fdm_matrices
!>        Coefficients to evaluate first radial derivative
        integer(kind = kint) :: n_order
!>        Structure of FDM matrix
        type(fdm_matrix), allocatable :: fdm(:)
!
!>      Work matrix to construct radial derivatives 
!!      from nodal field by FDM
        real(kind = kreal), allocatable :: wk_mat(:,:,:)
      end type fdm_matrices
!
      private :: alloc_fdm_matrix, dealloc_fdm_matrix
      private :: check_fdm_coef
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_nod_fdm_matrices                                 &
     &         (nri, num_order, n_minus, n_plus, fdmn_nod)
!
      integer(kind = kint), intent(in) :: nri, num_order
      integer(kind = kint), intent(in) :: n_minus, n_plus
      type(fdm_matrices), intent(inout) :: fdmn_nod
!
      integer(kind = kint) :: i
!
!
      fdmn_nod%n_order = num_order
      allocate( fdmn_nod%fdm(fdmn_nod%n_order) )
!
      do i = 1, fdmn_nod%n_order
        call alloc_fdm_matrix(nri, n_minus, n_plus, fdmn_nod%fdm(i))
      end do
!
      end subroutine alloc_nod_fdm_matrices
!
! -----------------------------------------------------------------------
!
      subroutine alloc_fdm_work(nri, fdmn_nod)
!
      integer(kind = kint), intent(in) :: nri
      type(fdm_matrices), intent(inout) :: fdmn_nod
!
      integer(kind = kint) :: nband
!
!
      nband = fdmn_nod%n_order + 1
      allocate( fdmn_nod%wk_mat(nband,nband,nri) )
      if(nri .gt. 0)  fdmn_nod%wk_mat =  0.0d0
!
      end subroutine alloc_fdm_work
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_nod_fdm_matrices(fdmn_nod)
!
      type(fdm_matrices), intent(inout) :: fdmn_nod
!
      integer(kind = kint) :: i
!
!
      do i = 1, fdmn_nod%n_order
        call dealloc_fdm_matrix(fdmn_nod%fdm(i))
      end do
!
      deallocate(fdmn_nod%fdm)
!
      end subroutine dealloc_nod_fdm_matrices
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_fdm_work(fdmn_nod)
!
      type(fdm_matrices), intent(inout) :: fdmn_nod
!
!
      deallocate( fdmn_nod%wk_mat )
!
      end subroutine dealloc_fdm_work
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_fdm2_nod_coefs_from_mat(nri, fdm2_nod)
!
      integer(kind = kint), intent(in) :: nri
      type(fdm_matrices), intent(inout) :: fdm2_nod
!
      integer(kind= kint) :: k, i
!
!
!$omp parallel private(i)
      do i = 1, fdm2_nod%n_order
!$omp do private (k)
        do k = 1, nri
          fdm2_nod%fdm(i)%dmat(k,-1) = fdm2_nod%wk_mat(i+1,3,k)
          fdm2_nod%fdm(i)%dmat(k, 0) = fdm2_nod%wk_mat(i+1,1,k)
          fdm2_nod%fdm(i)%dmat(k, 1) = fdm2_nod%wk_mat(i+1,2,k)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_fdm2_nod_coefs_from_mat
!
! -----------------------------------------------------------------------
!
      subroutine copy_fdm4_nod_coefs_from_mat(nri, fdm4_nod)
!
      integer(kind = kint), intent(in) :: nri
      type(fdm_matrices), intent(inout) :: fdm4_nod
      integer(kind= kint) :: i, k
!
!
!$omp parallel private (i)
      do i = 1, 4
!$omp do private (k)
        do k = 1, nri
          fdm4_nod%fdm(i)%dmat(k,-2) = fdm4_nod%wk_mat(i+1,5,k)
          fdm4_nod%fdm(i)%dmat(k,-1) = fdm4_nod%wk_mat(i+1,3,k)
          fdm4_nod%fdm(i)%dmat(k, 0) = fdm4_nod%wk_mat(i+1,1,k)
          fdm4_nod%fdm(i)%dmat(k, 1) = fdm4_nod%wk_mat(i+1,2,k)
          fdm4_nod%fdm(i)%dmat(k, 2) = fdm4_nod%wk_mat(i+1,4,k)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_fdm4_nod_coefs_from_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_fdm_coefs(nri, r, fdmn_nod)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      type(fdm_matrices), intent(in) :: fdmn_nod
!
      integer(kind = kint) :: i
!
!
      do i = 1, fdmn_nod%n_order
        write(50,*) 'Matrix for differences: ', i
        call check_fdm_coef(nri, r, fdmn_nod%fdm(i))
      end do
!
      end subroutine check_fdm_coefs
!
! -----------------------------------------------------------------------
!
      subroutine check_fdm_mat(nri, r, fdmn_nod)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      type(fdm_matrices), intent(in) :: fdmn_nod
!
      integer(kind = kint) :: i, kr
!
      do i = 2, fdmn_nod%n_order
        write(50,*) 'kr, r, matrix'
        do kr = 1, nri
          write(50,'(i5,1p4e20.12)') kr, r(kr),                         &
     &                   fdmn_nod%wk_mat(i,1:fdmn_nod%n_order+1,kr)
        end do
      end do
!
      end subroutine check_fdm_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_fdm_matrix(nri, n_minus, n_plus, fdm)
!
      integer(kind = kint), intent(in) :: nri, n_minus, n_plus
      type(fdm_matrix), intent(inout) :: fdm
!
!
      fdm%nri_mat = nri
      fdm%n_plus =  n_plus
      fdm%n_minus = n_minus
      allocate( fdm%dmat(fdm%nri_mat,-fdm%n_minus:fdm%n_plus) )
!
      if(nri .gt. 0) fdm%dmat = 0.0d0
!
      end subroutine alloc_fdm_matrix
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_fdm_matrix(fdm)
!
      type(fdm_matrix), intent(inout) :: fdm
!
      deallocate(fdm%dmat)
!
      end subroutine dealloc_fdm_matrix
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_fdm_coef(nri, r, fdm)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      type(fdm_matrix), intent(in) :: fdm
!
      integer(kind = kint) :: kr
!
      write(50,*) 'kr, r, coefficients'
      do kr = 1, nri
        write(50,'(i5,1p40e20.12)')                                     &
     &       kr, r(kr), fdm%dmat(kr,-fdm%n_minus:fdm%n_plus)
      end do
!
      end subroutine check_fdm_coef
!
! -----------------------------------------------------------------------
!
      end module t_fdm_coefs
