!>@file   m_fdm_2e_coefs.f90
!!@brief  module m_fdm_2e_coefs
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
!!    derivatives on node by element field
!!      d_nod =   d_nod_mat_fdm_2e(0) * d_ele(k  )
!!              + d_nod_mat_fdm_2e(1) * d_ele(k+1)
!!      dfdr =    d1nod_mat_fdm_2e(0) * d_ele(k  )
!!              + d1nod_mat_fdm_2e(1) * d_ele(k+1)
!!
!! ----------------------------------------------------------------------
!!      Work array to obtain 1d FDM
!!
!!    derivatives on node by nodal field
!!      dfdr =    mat_fdm_2(2,1,k) * d_nod(k  )
!!              + mat_fdm_2(2,2,k) * d_nod(k+1)
!!              + mat_fdm_2(2,3,k) * d_nod(k-1)
!!      d2fdr2 =  mat_fdm_2(3,1,k) * d_nod(k  )
!!              + mat_fdm_2(3,2,k) * d_nod(k+1)
!!              + mat_fdm_2(3,3,k) * d_nod(k-1)
!!
!!    derivatives on node by element field
!!      d_nod(k) =   mat_fdm_2e(1,1) * d_ele(k+1)
!!                 + mat_fdm_2e(1,2) * d_ele(k  )
!!      dfdr =       mat_fdm_2e(2,1) * d_ele(k+1)
!!                 + mat_fdm_2e(2,2) * d_ele(k  )
!! ----------------------------------------------------------------------
!!
!!      subroutine allocate_fdm_2e_coefs(nri)
!!      subroutine deallocate_fdm_2e_coefs
!!
!!      subroutine cal_2nd_ele_r_fdm_coefs(nlayer_ICB, nri, r)
!!
!!      subroutine check_fdm_2e_coefs(nri, r)
!!      subroutine check_fdm_2e_mat(nri, r)
!!@endverbatim
!!
!!@n @param nri           number of radial grid points
!!@n @param nlayer_ICB    Address of inner core boundary
!!@n @param r(nri) radius
!
      module m_fdm_2e_coefs
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Coefficients to evaluate first radial derivative
!!      from element field by FDM
      real(kind = kreal), allocatable :: d_nod_mat_fdm_2e(:,:)
!>      Coefficients to evaluate second radial derivative
!!      from element field by FDM
      real(kind = kreal), allocatable :: d1nod_mat_fdm_2e(:,:)
!
!>      Work matrix to construct radial derivatives
!!      from elemental field by FDM
      real(kind = kreal), allocatable :: mat_fdm_2e(:,:,:)
!
      private :: mat_fdm_2e
      private :: allocate_fdm_2e_matrices, deallocate_fdm_2e_matrices
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_fdm_2e_coefs(nri)
!
      integer(kind = kint), intent(in) :: nri
!
!
      allocate( d_nod_mat_fdm_2e(nri,0:1) )
      allocate( d1nod_mat_fdm_2e(nri,0:1) )
!
      if(nri .gt. 0) then
        d_nod_mat_fdm_2e = 0.0d0
        d1nod_mat_fdm_2e = 0.0d0
      end if
!
      end subroutine allocate_fdm_2e_coefs
!
! -----------------------------------------------------------------------
!
      subroutine allocate_fdm_2e_matrices(nri)
!
      integer(kind = kint), intent(in) :: nri
!
!
      allocate( mat_fdm_2e(2,2,nri) )
      if(nri .gt. 0) mat_fdm_2e = 0.0d0
!
      end subroutine allocate_fdm_2e_matrices
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_fdm_2e_coefs
!
!
      deallocate( d_nod_mat_fdm_2e, d1nod_mat_fdm_2e )
!
      end subroutine deallocate_fdm_2e_coefs
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_fdm_2e_matrices
!
!
      deallocate(mat_fdm_2e)
!
      end subroutine deallocate_fdm_2e_matrices
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_2nd_ele_r_fdm_coefs(nlayer_ICB, nri, r)
!
      use cal_inverse_small_matrix
!
      integer(kind = kint), intent(in) :: nri, nlayer_ICB
      real(kind = kreal), intent(in) :: r(nri)
!
      integer(kind = kint) :: ierr
      integer(kind = kint) :: kr
!
      real(kind = kreal) :: dr_p1, dr_n1
      real(kind = kreal) :: mat_taylor_2(2,2)
!
!
      call allocate_fdm_2e_matrices(nri)
!
      do kr = 1, nri
!
        dr_p1 = (r(kr+1) - r(kr)) * half
        if (kr.eq.1) then
          if(nlayer_ICB.gt.1) then
            dr_n1 = r(1) * half
          else
            dr_n1 = (r(2) - r(1)) * half
          end if
        else
          dr_n1 = (r(2) - r(1)) * half
        end if
!
        mat_taylor_2(1,1) = one
        mat_taylor_2(1,2) = dr_p1
!
        mat_taylor_2(2,1) = one
        mat_taylor_2(2,2) =-dr_n1
!
        call cal_inverse_22_matrix(mat_taylor_2, mat_fdm_2e(1,1,kr),    &
     &      ierr)
      end do
!
!$omp parallel do private (kr)
      do kr = 1, nri
        d_nod_mat_fdm_2e(kr,0) = mat_fdm_2e(1,2,kr)
        d_nod_mat_fdm_2e(kr,1) = mat_fdm_2e(1,1,kr)
!
        d1nod_mat_fdm_2e(kr,0) = mat_fdm_2e(2,2,kr)
        d1nod_mat_fdm_2e(kr,1) = mat_fdm_2e(2,1,kr)
      end do
!$omp end parallel do
!
      call deallocate_fdm_2e_matrices
!
      end subroutine cal_2nd_ele_r_fdm_coefs
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_fdm_2e_coefs(nri, r)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      integer(kind = kint) :: kr
!
      write(50,*) 'kr, r, dnod_n1, dnod_p1'
      do kr = 1, nri
        write(50,'(i5,1p3e20.12)') kr, r(kr),                           &
     &           d_nod_mat_fdm_2e(kr,0:1)
      end do
!
      write(50,*) 'kr, r, dfe_n1, dfe_p1'
      do kr = 1, nri
        write(50,'(i5,1p3e20.12)') kr, r(kr),                           &
     &           d1nod_mat_fdm_2e(kr,0:1)
      end do
!
      end subroutine check_fdm_2e_coefs
!
! -----------------------------------------------------------------------
!
      subroutine check_fdm_2e_mat(nri, r)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      integer(kind = kint) :: kr
!
      write(50,*) 'kr, r, cnod_p, cnod_n'
      do kr = 1, nri
        write(50,'(i5,1p3e20.12)') kr, r(kr), mat_fdm_2e(1,1:2,kr)
      end do
      write(50,*) 'kr, r, dfe_p, d2fe_n'
      do kr = 1, nri
        write(50,'(i5,1p3e20.12)') kr, r(kr), mat_fdm_2e(2,1:2,kr)
      end do
!
      end subroutine check_fdm_2e_mat
!
! -----------------------------------------------------------------------
!
      end module m_fdm_2e_coefs
