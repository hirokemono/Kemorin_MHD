!>@file   m_schmidt_legendre_each_m.f90
!!@brief  module m_schmidt_legendre_each_m
!!
!!@author H. Matsui
!!@date Programmed in 1993
!!@n    Modified in 2009
!
!> @brief Data array for Legendre polyonomials with
!!        Schmidt normalization
!!
!!@verbatim
!!      subroutine allocate_schmidt_legendre_m(ltr)
!!      subroutine deallocate_schmidt_legendre_m
!!
!!      subroutine dlad(m,theta)
!!      subroutine dschmidt_m(m, theta)
!!@endverbatim
!
      module m_schmidt_legendre_each_m
!
      use m_precision
!
      implicit  none
!
!
!>      Truncation level
      integer(kind = kint) :: nth_m = 10
!
!>      Schmidt quasi-normalized Legendre polynomials
      real(kind = kreal), allocatable :: p_m(:)
!>      Schmidt quasi-normalized Legendre polynomials
      real(kind = kreal), allocatable :: pmp1(:)
!>      Schmidt quasi-normalized Legendre polynomials
      real(kind = kreal), allocatable :: pmn1(:)
!
!>      diffrence of Schmidt quasi-normalized Legendre polynomials
      real(kind = kreal), allocatable :: dp_m(:)
!
!>      work area
      real(kind = kreal), allocatable :: df_m(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_schmidt_legendre_m(ltr)
!
      integer(kind = kint), intent(in) :: ltr
!
!
      nth_m = ltr
      allocate ( p_m(0:nth_m), pmp1(0:nth_m), pmn1(0:nth_m) )
      allocate ( dp_m(0:nth_m) )
      allocate ( df_m(0:nth_m+2) )
!
      p_m = 0.0d0
      pmp1 = 0.0d0
      pmn1 = 0.0d0
      dp_m = 0.0d0
      df_m = 0.0d0
!
      end subroutine allocate_schmidt_legendre_m
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_schmidt_legendre_m
!
        deallocate (p_m, pmp1, pmn1, dp_m, df_m)
!
       end subroutine deallocate_schmidt_legendre_m
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dschmidt_m(m, theta)
!
      use schmidt_fix_m
!
      integer(kind = kint), intent(in) :: m
      real(kind = kreal), intent(in) :: theta
!
!
      call schmidt_legendres_m                                          &
     &   (nth_m, m, theta, p_m, dp_m, pmn1, pmp1, df_m)
!
      end subroutine dschmidt_m
!
! -----------------------------------------------------------------------
!
      subroutine full_norm_legendre(m, theta)
!
      use schmidt_fix_m
!
      integer(kind = kint), intent(in) :: m
      real(kind = kreal), intent(in) :: theta
!
!
!*   ++++++++++  lead adjoint Legendre Polynomial  ++++++
!*
      call dschmidt_m(m, theta)
!
!*   ++++++++++  Full normalizationl  ++++++
!*
      call full_normalize_from_smdt_m(nth_m, m, p_m, dp_m)
!
      end subroutine full_norm_legendre
!
! -----------------------------------------------------------------------
!
      end module m_schmidt_legendre_each_m
