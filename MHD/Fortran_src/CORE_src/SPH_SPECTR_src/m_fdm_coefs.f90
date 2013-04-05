!>@file   m_fdm_coefs.f90
!!@brief  module m_fdm_coefs
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
!!    derivatives on node by nodal field
!!      dfdr =    d1nod_mat_fdm_2(-1) * d_nod(k-1)
!!              + d1nod_mat_fdm_2( 0) * d_nod(k  )
!!              + d1nod_mat_fdm_2( 1) * d_nod(k+1)
!!      d2fdr2 =  d2nod_mat_fdm_2(-1) * d_nod(k-1)
!!              + d2nod_mat_fdm_2( 0) * d_nod(k  )
!!              + d2nod_mat_fdm_2( 1) * d_nod(k+1)
!!
!!    derivatives on node by element field
!!      d_nod =   d_nod_mat_fdm_2e(0) * d_ele(k  )
!!              + d_nod_mat_fdm_2e(1) * d_ele(k+1)
!!      dfdr =    d1nod_mat_fdm_2e(0) * d_ele(k  )
!!              + d1nod_mat_fdm_2e(1) * d_ele(k+1)
!! ----------------------------------------------------------------------!!      Work array to obtain 1d FDM
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
!!
!! ----------------------------------------------------------------------
!!
!!      subroutine allocate_fdm_coefs(nri)
!!      subroutine allocate_fdm_matrices(nri)
!!
!!      subroutine deallocate_fdm_coefs
!!      subroutine deallocate_fdm_matrices
!!
!!      subroutine check_fdm_2_coefs(nri, r)
!!      subroutine check_fdm_2e_coefs(nri, r)
!!
!!      subroutine check_fdm_2_mat(nri, r)
!!      subroutine check_fdm_2e_mat(nri, r)
!!@endverbatim
!!
!!@n @param nri    number of radial grid points
!!@n @param r(nri) radius
!
      module m_fdm_coefs
!
      use m_precision
!
      implicit none
!
!
!>      Coefficients to evaluate first radial derivative
!!      from nodal field by FDM
      real(kind = kreal), allocatable :: d1nod_mat_fdm_2(:,:)
!>      Coefficients to evaluate second radial derivative
!!      from nodal field by FDM
      real(kind = kreal), allocatable :: d2nod_mat_fdm_2(:,:)
!
!>      Coefficients to evaluate first radial derivative
!!      from element field by FDM
      real(kind = kreal), allocatable :: d_nod_mat_fdm_2e(:,:)
!>      Coefficients to evaluate second radial derivative
!!      from element field by FDM
      real(kind = kreal), allocatable :: d1nod_mat_fdm_2e(:,:)
!
!>      Work matrix to construct radial derivatives 
!!      from nodal field by FDM
      real(kind = kreal), allocatable :: mat_fdm_2(:,:,:)
!>      Work matrix to construct radial derivatives
!!      from elemental field by FDM
      real(kind = kreal), allocatable :: mat_fdm_2e(:,:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_fdm_coefs(nri)
!
      integer(kind = kint), intent(in) :: nri
!
!
      allocate( d1nod_mat_fdm_2(nri,-1:1) )
      allocate( d2nod_mat_fdm_2(nri,-1:1) )
!
      allocate( d_nod_mat_fdm_2e(nri,0:1) )
      allocate( d1nod_mat_fdm_2e(nri,0:1) )
!
      d1nod_mat_fdm_2 = 0.0d0
      d2nod_mat_fdm_2 = 0.0d0
!
      d_nod_mat_fdm_2e = 0.0d0
      d1nod_mat_fdm_2e = 0.0d0
!
      end subroutine allocate_fdm_coefs
!
! -----------------------------------------------------------------------
!
      subroutine allocate_fdm_matrices(nri)
!
      integer(kind = kint), intent(in) :: nri
!
!
      allocate( mat_fdm_2(3,3,nri) )
      allocate( mat_fdm_2e(2,2,nri) )
!
      mat_fdm_2 = 0.0d0
      mat_fdm_2e = 0.0d0
!
      end subroutine allocate_fdm_matrices
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_fdm_coefs
!
!
      deallocate( d1nod_mat_fdm_2, d2nod_mat_fdm_2 )
      deallocate( d_nod_mat_fdm_2e, d1nod_mat_fdm_2e )
!
      end subroutine deallocate_fdm_coefs
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_fdm_matrices
!
!
      deallocate( mat_fdm_2, mat_fdm_2e )
!
      end subroutine deallocate_fdm_matrices
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_fdm_2_coefs(nri, r)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      integer(kind = kint) :: kr
!
      write(50,*) 'kr, r, df_n1, df_0, df_p1'
      do kr = 1, nri
        write(50,'(i5,1p4e20.12)') kr, r(kr), d1nod_mat_fdm_2(kr,-1:1)
      end do
      write(50,*) 'kr, r, d2f_n1, d2f_0, d2f_p1'
      do kr = 1, nri
        write(50,'(i5,1p4e20.12)') kr, r(kr), d2nod_mat_fdm_2(kr,-1:1)
      end do
!
      end subroutine check_fdm_2_coefs
!
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
      subroutine check_fdm_2_mat(nri, r)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      integer(kind = kint) :: kr
!
      write(50,*) 'kr, r, df_0, df_p, df_n'
      do kr = 1, nri
        write(50,'(i5,1p4e20.12)') kr, r(kr), mat_fdm_2(2,1:3,kr)
      end do
      write(50,*) 'kr, r, d2f_0, d2f_p, d2f_n'
      do kr = 1, nri
        write(50,'(i5,1p4e20.12)') kr, r(kr), mat_fdm_2(3,1:3,kr)
      end do
!
      end subroutine check_fdm_2_mat
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
      end module m_fdm_coefs
