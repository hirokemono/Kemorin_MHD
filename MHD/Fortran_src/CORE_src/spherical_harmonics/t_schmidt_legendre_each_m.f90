!>@file   t_schmidt_legendre_each_m.f90
!!@brief  module t_schmidt_legendre_each_m
!!
!!@author H. Matsui
!!@date Programmed in 1993
!!@n    Modified in 2009
!
!> @brief Data array for Legendre polyonomials with
!!        Schmidt normalization
!!
!!@verbatim
!!      subroutine allocate_schmidt_legendre_m(ltr, leg_m)
!!      subroutine deallocate_schmidt_legendre_m(leg_m)
!!
!!      subroutine dlad(m,theta, leg_m)
!!      subroutine dschmidt_m(m, theta, leg_m)
!!        type(legendre_polynomials_each_m), intent(inout) :: leg_m
!!@endverbatim
!
      module t_schmidt_legendre_each_m
!
      use m_precision
!
      implicit  none
!
!
      type legendre_polynomials_each_m
!>        Truncation level
        integer(kind = kint) :: nth_m = 10
!
!>        Schmidt quasi-normalized Legendre polynomials
        real(kind = kreal), allocatable :: p_m(:)
!>        Schmidt quasi-normalized Legendre polynomials
        real(kind = kreal), allocatable :: pmp1(:)
!>        Schmidt quasi-normalized Legendre polynomials
        real(kind = kreal), allocatable :: pmn1(:)
!
!>        diffrence of Schmidt quasi-normalized Legendre polynomials
        real(kind = kreal), allocatable :: dp_m(:)
!
!>        work area
        real(kind = kreal), allocatable :: df_m(:)
      end type legendre_polynomials_each_m
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_schmidt_legendre_m(ltr, leg_m)
!
      integer(kind = kint), intent(in) :: ltr
      type(legendre_polynomials_each_m), intent(inout) :: leg_m
!
!
      leg_m%nth_m = ltr
      allocate ( leg_m%p_m(0:leg_m%nth_m) )
      allocate ( leg_m%pmp1(0:leg_m%nth_m) )
      allocate ( leg_m%pmn1(0:leg_m%nth_m) )
      allocate ( leg_m%dp_m(0:leg_m%nth_m) )
      allocate ( leg_m%df_m(0:leg_m%nth_m+2) )
!
      if(leg_m%nth_m .le. 0) return
      leg_m%p_m = 0.0d0
      leg_m%pmp1 = 0.0d0
      leg_m%pmn1 = 0.0d0
      leg_m%dp_m = 0.0d0
      leg_m%df_m = 0.0d0
!
      end subroutine allocate_schmidt_legendre_m
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_schmidt_legendre_m(leg_m)
!
      type(legendre_polynomials_each_m), intent(inout) :: leg_m
!
       deallocate(leg_m%p_m, leg_m%pmp1, leg_m%pmn1)
       deallocate(leg_m%dp_m, leg_m%df_m)
!
       end subroutine deallocate_schmidt_legendre_m
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dschmidt_m(m, theta, leg_m)
!
      use schmidt_fix_m
!
      integer(kind = kint), intent(in) :: m
      real(kind = kreal), intent(in) :: theta
!
      type(legendre_polynomials_each_m), intent(inout) :: leg_m
!
!
      call schmidt_legendres_m                                          &
     &   (leg_m%nth_m, m, theta, leg_m%p_m, leg_m%dp_m, leg_m%pmn1,     &
     &    leg_m%pmp1, leg_m%df_m)
!
      end subroutine dschmidt_m
!
! -----------------------------------------------------------------------
!
      subroutine full_norm_legendre(m, theta, leg_m)
!
      use schmidt_fix_m
!
      integer(kind = kint), intent(in) :: m
      real(kind = kreal), intent(in) :: theta
!
      type(legendre_polynomials_each_m), intent(inout) :: leg_m
!
!
!*   ++++++++++  lead adjoint Legendre Polynomial  ++++++
!*
      call dschmidt_m(m, theta, leg_m)
!
!*   ++++++++++  Full normalizationl  ++++++
!*
      call full_normalize_from_smdt_m                                   &
     &   (leg_m%nth_m, m, leg_m%p_m, leg_m%dp_m)
!
      end subroutine full_norm_legendre
!
! -----------------------------------------------------------------------
!
      end module t_schmidt_legendre_each_m
