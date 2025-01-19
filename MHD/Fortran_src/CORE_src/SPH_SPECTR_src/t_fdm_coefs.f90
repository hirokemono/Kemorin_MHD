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
!!      dfdr =    fdm_nod%r_fdm%dmat(-1,k,1) * d_nod(k-1)
!!              + fdm_nod%r_fdm%dmat( 0,k,1) * d_nod(k  )
!!              + fdm_nod%r_fdm%dmat( 1,k,1) * d_nod(k+1)
!!      d2fdr2 =  fdm_nod%r_fdm%dmat(-1,k,2) * d_nod(k-1)
!!              + fdm_nod%r_fdm%dmat( 0,k,2) * d_nod(k  )
!!              + fdm_nod%r_fdm%dmat( 1,k,2) * d_nod(k+1)
!! ----------------------------------------------------------------------
!!
!!      subroutine alloc_nod_fdm_matrices                               &
!!     &         (nri, ist_order, num_order, n_minus, n_plus, fdm_nod)
!!      subroutine dealloc_nod_fdm_matrices(fdm_nod)
!!        integer(kind = kint), intent(in) :: nri, num_order
!!        integer(kind = kint), intent(in) :: n_minus, n_plus
!!        type(fdm_matrices), intent(inout) :: fdm_nod
!!
!!      subroutine check_fdm_coefs(id_file, nri, r, fdm_nod)
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: nri
!!        real(kind = kreal), intent(in) :: r(nri)
!!        type(fdm_matrices), intent(in) :: fdm_nod
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
      type fdm_r_matrix
!>        Maximum order for derivative
        integer(kind = kint) :: n_order
!>        Number of radial points
        integer(kind = kint) :: nri_mat
!>        Width of matrix (positive side)
        integer(kind = kint) :: n_plus
!>        Width of matrix (negative side)
        integer(kind = kint) :: n_minus
!>        Coefficients to evaluate radial derivative
!!        from nodal field by FDM
        real(kind = kreal), allocatable :: dmat(:,:,:)
      end type fdm_r_matrix
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
!>        Minimum order for derivative
        integer(kind = kint) :: ist_order
!>        Maximum order for derivative
        integer(kind = kint) :: n_order
!>        Structure of FDM matrix
        type(fdm_r_matrix) :: r_fdm
!>        Structure of FDM matrix
        type(fdm_matrix), allocatable :: fdm(:)
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
     &         (nri, ist_order, num_order, n_minus, n_plus, fdm_nod)
!
      integer(kind = kint), intent(in) :: nri, ist_order, num_order
      integer(kind = kint), intent(in) :: n_minus, n_plus
      type(fdm_matrices), intent(inout) :: fdm_nod
!
      integer(kind = kint) :: i
!
!
      fdm_nod%ist_order = ist_order
      fdm_nod%n_order =   num_order
      allocate(fdm_nod%fdm(ist_order:num_order))
      do i = fdm_nod%ist_order, fdm_nod%n_order
        call alloc_fdm_matrix(nri, n_minus, n_plus, fdm_nod%fdm(i))
      end do
!
      call alloc_fdm_r_matrix(num_order, nri, n_minus, n_plus,          &
     &                        fdm_nod%r_fdm)
!
      end subroutine alloc_nod_fdm_matrices
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_nod_fdm_matrices(fdm_nod)
!
      type(fdm_matrices), intent(inout) :: fdm_nod
!
      integer(kind = kint) :: i
!
!
      call dealloc_fdm_r_matrix(fdm_nod%r_fdm)
      do i = fdm_nod%ist_order, fdm_nod%n_order
        call dealloc_fdm_matrix(fdm_nod%fdm(i))
      end do
!
      deallocate(fdm_nod%fdm)
!
      end subroutine dealloc_nod_fdm_matrices
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_fdm_coefs(id_file, nri, r, fdm_nod)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      type(fdm_matrices), intent(in) :: fdm_nod
!
      integer(kind = kint) :: i
!
!
      do i = 0, fdm_nod%n_order
        write(id_file,*) 'Matrix for differences: ', i
        call check_r_fdm_coef(id_file, i, nri, r, fdm_nod%r_fdm)
      end do
!
      do i = fdm_nod%ist_order, fdm_nod%n_order
        write(id_file,*) 'Matrix for differences: ', i
        call check_fdm_coef(id_file, nri, r, fdm_nod%fdm(i))
      end do
!
      end subroutine check_fdm_coefs
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_fdm_r_matrix(n_order, nri,                       &
     &                              n_minus, n_plus, r_fdm)
!
      integer(kind = kint), intent(in) :: n_order, nri
      integer(kind = kint), intent(in) :: n_minus, n_plus
      type(fdm_r_matrix), intent(inout) :: r_fdm
!
!
      r_fdm%n_order =  n_order
      r_fdm%nri_mat =  nri
      r_fdm%n_plus =   n_plus
      r_fdm%n_minus = -n_minus
      allocate(r_fdm%dmat(-n_minus:n_plus,1:nri,0:n_order))
!
      if(size(r_fdm%dmat) .gt. 0) then
!$omp parallel workshare
        r_fdm%dmat(-n_minus:n_plus,1:nri,0:n_order) = 0.0d0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_fdm_r_matrix
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_fdm_r_matrix(r_fdm)
!
      type(fdm_r_matrix), intent(inout) :: r_fdm
!
      deallocate(r_fdm%dmat)
!
      end subroutine dealloc_fdm_r_matrix
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_r_fdm_coef(id_file, i_order, nri, r, r_fdm)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: i_order, nri
      real(kind = kreal), intent(in) :: r(nri)
      type(fdm_r_matrix), intent(in) :: r_fdm
!
      integer(kind = kint) :: kr
!
      write(id_file,*) 'r, kr, coefficients'
      do kr = 1, nri
        write(id_file,'(1pe20.12,i5,1p40e20.12)') r(kr), kr,            &
     &          r_fdm%dmat(r_fdm%n_minus:r_fdm%n_plus,kr,i_order)
      end do
!
      end subroutine check_r_fdm_coef
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
      fdm%n_minus = -n_minus
      allocate( fdm%dmat(fdm%nri_mat,fdm%n_minus:fdm%n_plus) )
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
      subroutine check_fdm_coef(id_file, nri, r, fdm)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      type(fdm_matrix), intent(in) :: fdm
!
      integer(kind = kint) :: kr
!
      write(id_file,*) 'r, kr, coefficients'
      do kr = 1, nri
        write(id_file,'(1pe20.12,i5,1p40e20.12)')                       &
     &       r(kr), kr, fdm%dmat(kr,fdm%n_minus:fdm%n_plus)
      end do
!
      end subroutine check_fdm_coef
!
! -----------------------------------------------------------------------
!
      end module t_fdm_coefs
