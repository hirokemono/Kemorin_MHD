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
!!      dfdr =    d1nod_mat_fdm_4(-2,1) *  d_nod(k-2)
!!              + d1nod_mat_fdm_4(-1,2) *  d_nod(k-1)
!!              + d1nod_mat_fdm_4( 0,3) *  d_nod(k  )
!!              + d1nod_mat_fdm_4( 1,3) *  d_nod(k+1)
!!              + d1nod_mat_fdm_4( 2,4) *  d_nod(k+2)
!!      d2fdr2 =  d2nod_mat_fdm_4(-2,1) *  d_nod(k-2)
!!              + d2nod_mat_fdm_4(-1,2) *  d_nod(k-1)
!!              + d2nod_mat_fdm_4( 1,3) *  d_nod(k  )
!!              + d2nod_mat_fdm_4( 0,3) *  d_nod(k+1)
!!              + d2nod_mat_fdm_4( 2,4) *  d_nod(k+2)
!!      d3fdr3 =  d3nod_mat_fdm_4(-2,1) *  d_nod(k-2)
!!              + d3nod_mat_fdm_4(-1,2) *  d_nod(k-1)
!!              + d3nod_mat_fdm_4( 0,3) *  d_nod(k  )
!!              + d3nod_mat_fdm_4( 1,3) *  d_nod(k+1)
!!              + d3nod_mat_fdm_4( 2,4) *  d_nod(k+2)
!!      d4fdr4 =  d4nod_mat_fdm_4(-2,1) *  d_nod(k-2)
!!              + d4nod_mat_fdm_4(-1,2) *  d_nod(k-1)
!!              + d4nod_mat_fdm_4( 0,3) *  d_nod(k  )
!!              + d4nod_mat_fdm_4( 1,3) *  d_nod(k+1)
!!              + d4nod_mat_fdm_4( 2,4) *  d_nod(k+2)
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
!
      implicit none
!
!
!>      Coefficients to evaluate first radial derivative
!!      from nodal field by FDM
      real(kind = kreal), allocatable :: d1nod_mat_fdm_4(:,:)
!>      Coefficients to evaluate second radial derivative
!!      from nodal field by FDM
      real(kind = kreal), allocatable :: d2nod_mat_fdm_4(:,:)
!>      Coefficients to evaluate third radial derivative
!!      from nodal field by FDM
      real(kind = kreal), allocatable :: d3nod_mat_fdm_4(:,:)
!>      Coefficients to evaluate forth radial derivative
!!      from nodal field by FDM
      real(kind = kreal), allocatable :: d4nod_mat_fdm_4(:,:)
!
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
      allocate( d1nod_mat_fdm_4(nri,-2:2) )
      allocate( d2nod_mat_fdm_4(nri,-2:2) )
      allocate( d3nod_mat_fdm_4(nri,-2:2) )
      allocate( d4nod_mat_fdm_4(nri,-2:2) )
!
      if(nri .gt. 0) then
        d1nod_mat_fdm_4 = 0.0d0
        d2nod_mat_fdm_4 = 0.0d0
        d3nod_mat_fdm_4 = 0.0d0
        d4nod_mat_fdm_4 = 0.0d0
      end if
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
      deallocate( d1nod_mat_fdm_4, d2nod_mat_fdm_4 )
      deallocate( d3nod_mat_fdm_4, d4nod_mat_fdm_4 )
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
      integer(kind= kint) :: k
!
!
!$omp parallel do private (k)
      do k = 1, nri
        d1nod_mat_fdm_4(k,-2) = mat_fdm_4(2,5,k)
        d1nod_mat_fdm_4(k,-1) = mat_fdm_4(2,3,k)
        d1nod_mat_fdm_4(k, 0) = mat_fdm_4(2,1,k)
        d1nod_mat_fdm_4(k, 1) = mat_fdm_4(2,2,k)
        d1nod_mat_fdm_4(k, 2) = mat_fdm_4(2,4,k)
!
        d2nod_mat_fdm_4(k,-2) = mat_fdm_4(3,5,k)
        d2nod_mat_fdm_4(k,-1) = mat_fdm_4(3,3,k)
        d2nod_mat_fdm_4(k, 0) = mat_fdm_4(3,1,k)
        d2nod_mat_fdm_4(k, 1) = mat_fdm_4(3,2,k)
        d2nod_mat_fdm_4(k, 2) = mat_fdm_4(3,4,k)
!
        d3nod_mat_fdm_4(k,-2) = mat_fdm_4(4,5,k)
        d3nod_mat_fdm_4(k,-1) = mat_fdm_4(4,3,k)
        d3nod_mat_fdm_4(k, 0) = mat_fdm_4(4,1,k)
        d3nod_mat_fdm_4(k, 1) = mat_fdm_4(4,2,k)
        d3nod_mat_fdm_4(k, 2) = mat_fdm_4(4,4,k)
!
        d4nod_mat_fdm_4(k,-2) = mat_fdm_4(5,5,k)
        d4nod_mat_fdm_4(k,-1) = mat_fdm_4(5,3,k)
        d4nod_mat_fdm_4(k, 0) = mat_fdm_4(5,1,k)
        d4nod_mat_fdm_4(k, 1) = mat_fdm_4(5,2,k)
        d4nod_mat_fdm_4(k, 2) = mat_fdm_4(5,4,k)
      end do
!$omp end parallel do
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
      integer(kind = kint) :: kr
!
      write(50,*) 'kr, r, d1f_n2, d1f_n1, d1f_0, d1f_p1, d2f_p2'
      do kr = 1, nri
        write(50,'(i5,1p10e20.12)') kr, r(kr), d1nod_mat_fdm_4(kr,-2:2)
      end do
      write(50,*) 'kr, r, d2f_n2, d2f_n1, d2f_0, d2f_p1, d2f_p2'
      do kr = 1, nri
        write(50,'(i5,1p10e20.12)') kr, r(kr), d2nod_mat_fdm_4(kr,-2:2)
      end do
      write(50,*) 'kr, r, d3f_n2, d3f_n1, d3f_0, d3f_p1, d3f_p2'
      do kr = 1, nri
        write(50,'(i5,1p10e20.12)') kr, r(kr), d3nod_mat_fdm_4(kr,-2:2)
      end do
      write(50,*) 'kr, r, d4f_n2, d4f_n1, d4f_0, d4f_p1, d4f_p2'
      do kr = 1, nri
        write(50,'(i5,1p10e20.12)') kr, r(kr), d4nod_mat_fdm_4(kr,-2:2)
      end do
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
