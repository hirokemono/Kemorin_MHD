!>@file   m_fdm_4th_coefs.f90
!!@brief  module m_fdm_4th_coefs
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  Coefficients to obtain radial derivatives
!!        by forth order finite difference method
!!
!!@verbatim
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!     define of elemental field
!!       r_ele(k) = half *(r_nod(k-1) + r_nod(k))
!!       d_ele(k) = half *(d_nod(k-1) + d_nod(k))
!!
!!    4th order derivatives on node by nodal field
!!      dfdr =    r_4th%fdm(1)%dmat(-2,1) *  d_nod(k-2)
!!              + r_4th%fdm(1)%dmat(-1,2) *  d_nod(k-1)
!!              + r_4th%fdm(1)%dmat( 0,3) *  d_nod(k  )
!!              + r_4th%fdm(1)%dmat( 1,3) *  d_nod(k+1)
!!              + r_4th%fdm(1)%dmat( 2,4) *  d_nod(k+2)
!!      d2fdr2 =  r_4th%fdm(2)%dmat(-2,1) *  d_nod(k-2)
!!              + r_4th%fdm(2)%dmat(-1,2) *  d_nod(k-1)
!!              + r_4th%fdm(2)%dmat( 1,3) *  d_nod(k  )
!!              + r_4th%fdm(2)%dmat( 0,3) *  d_nod(k+1)
!!              + r_4th%fdm(2)%dmat( 2,4) *  d_nod(k+2)
!!      d3fdr3 =  r_4th%fdm(3)%dmat(-2,1) *  d_nod(k-2)
!!              + r_4th%fdm(3)%dmat(-1,2) *  d_nod(k-1)
!!              + r_4th%fdm(3)%dmat( 0,3) *  d_nod(k  )
!!              + r_4th%fdm(3)%dmat( 1,3) *  d_nod(k+1)
!!              + r_4th%fdm(3)%dmat( 2,4) *  d_nod(k+2)
!!      d4fdr4 =  r_4th%fdm(4)%dmat(-2,1) *  d_nod(k-2)
!!              + r_4th%fdm(4)%dmat(-1,2) *  d_nod(k-1)
!!              + r_4th%fdm(4)%dmat( 0,3) *  d_nod(k  )
!!              + r_4th%fdm(4)%dmat( 1,3) *  d_nod(k+1)
!!              + r_4th%fdm(4)%dmat( 2,4) *  d_nod(k+2)
!!
!!       r_4th%fdm(1)%dmat = d1nod_mat_fdm_4
!!       r_4th%fdm(2)%dmat = d2nod_mat_fdm_4
!!       r_4th%fdm(3)%dmat = d3nod_mat_fdm_4
!!       r_4th%fdm(4)%dmat = d4nod_mat_fdm_4
!!
!! ----------------------------------------------------------------------
!!      Work array to obtain 1d FDM
!!
!!    derivatives on node by nodal field
!!      dfdr =    mat_fdm_4(2,1,k) * d_nod(k  )
!!              + mat_fdm_4(2,2,k) * d_nod(k+1)
!!              + mat_fdm_4(2,3,k) * d_nod(k-1)
!!              + mat_fdm_4(2,4,k) * d_nod(k+2)
!!              + mat_fdm_4(2,5,k) * d_nod(k-2)
!!      d2fdr2 =  mat_fdm_4(3,1,k) * d_nod(k  )
!!              + mat_fdm_4(3,2,k) * d_nod(k+1)
!!              + mat_fdm_4(3,3,k) * d_nod(k-1)
!!              + mat_fdm_4(3,4,k) * d_nod(k+2)
!!              + mat_fdm_4(3,5,k) * d_nod(k-2)
!!      d3fdr3 =  mat_fdm_4(4,1,k) * d_nod(k  )
!!              + mat_fdm_4(4,2,k) * d_nod(k+1)
!!              + mat_fdm_4(4,3,k) * d_nod(k-1)
!!              + mat_fdm_4(4,4,k) * d_nod(k+2)
!!              + mat_fdm_4(4,5,k) * d_nod(k-2)
!!      d4fdr4 =  mat_fdm_4(5,1,k) * d_nod(k  )
!!              + mat_fdm_4(5,2,k) * d_nod(k+1)
!!              + mat_fdm_4(5,3,k) * d_nod(k-1)
!!              + mat_fdm_4(5,4,k) * d_nod(k+2)
!!              + mat_fdm_4(5,5,k) * d_nod(k-2)
!! ----------------------------------------------------------------------
!!
!!      subroutine allocate_fdm4_coefs(nri)
!!      subroutine allocate_fdm4_matrices(nri)
!!      subroutine deallocate_fdm4_coefs
!!      subroutine deallocate_fdm4_matrices
!!
!!      subroutine copy_fdm4_nod_coefs_from_mat(nri)
!!
!!      subroutine check_fdm_4_coefs(nri, r)
!!      subroutine check_fdm_4_mat(nri, r)
!!@endverbatim
!!
!!@n @param nri    number of radial grid points
!!@n @param r(nri) radius
!
      module m_fdm_4th_coefs
!
      use m_precision
      use m_constants
      use t_fdm_coefs
!
      implicit none
!
!
!>        Structure of FDM matrices
      type(fdm_matrices), save :: r_4th
!r_4th%fdm(1)%dmat
!
!>      Work matrix to construct radial derivatives with 5 points
      real(kind = kreal), allocatable :: mat_fdm_4(:,:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_fdm4_coefs(nri)
!
      integer(kind = kint), intent(in) :: nri
!
!
      call alloc_nod_fdm_matrices(nri, ifour, r_4th)
!
      end subroutine allocate_fdm4_coefs
!
! -----------------------------------------------------------------------
!
      subroutine allocate_fdm4_matrices(nri)
!
      integer(kind = kint), intent(in) :: nri
!
!
      allocate( mat_fdm_4(5,5,nri) )
      if(nri .gt. 0) mat_fdm_4 =  0.0d0
!
      end subroutine allocate_fdm4_matrices
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_fdm4_coefs
!
!
      call dealloc_nod_fdm_matrices(r_4th)
!
      end subroutine deallocate_fdm4_coefs
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_fdm4_matrices
!
!
      deallocate(mat_fdm_4)
!
      end subroutine deallocate_fdm4_matrices
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_fdm4_nod_coefs_from_mat(nri)
!
      integer(kind = kint), intent(in) :: nri
      integer(kind= kint) :: i, k
!
!
!$omp parallel private (i)
      do i = 1, 4
!$omp do private (k)
        do k = 1, nri
          r_4th%fdm(i)%dmat(k,-2) = mat_fdm_4(i+1,5,k)
          r_4th%fdm(i)%dmat(k,-1) = mat_fdm_4(i+1,3,k)
          r_4th%fdm(i)%dmat(k, 0) = mat_fdm_4(i+1,1,k)
          r_4th%fdm(i)%dmat(k, 1) = mat_fdm_4(i+1,2,k)
          r_4th%fdm(i)%dmat(k, 2) = mat_fdm_4(i+1,4,k)
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
      subroutine check_fdm_4_coefs(nri, r)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
!
      call check_fdm_coefs(nri, r, r_4th)
!
      end subroutine check_fdm_4_coefs
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_fdm_4_mat(nri, r)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      integer(kind = kint) :: kr
!
      write(50,*) 'kr, r, matrix for field'
      do kr = 1, nri
        write(50,'(i5,1p3e20.12)') kr, r(kr), mat_fdm_4(1,1:5,kr)
      end do
      write(50,*) 'kr, r, matrix for dfdr'
      do kr = 1, nri
        write(50,'(i5,1p3e20.12)') kr, r(kr), mat_fdm_4(2,1:5,kr)
      end do
      write(50,*) 'kr, r, matrix for d2fdr2'
      do kr = 1, nri
        write(50,'(i5,1p3e20.12)') kr, r(kr), mat_fdm_4(3,1:5,kr)
      end do
      write(50,*) 'kr, r, matrix for d3fdr3'
      do kr = 1, nri
        write(50,'(i5,1p3e20.12)') kr, r(kr), mat_fdm_4(4,1:5,kr)
      end do
      write(50,*) 'kr, r, matrix for d4fdr4'
      do kr = 1, nri
        write(50,'(i5,1p3e20.12)') kr, r(kr), mat_fdm_4(5,1:5,kr)
      end do
!
      end subroutine check_fdm_4_mat
!
! -----------------------------------------------------------------------
!
      end module m_fdm_4th_coefs
