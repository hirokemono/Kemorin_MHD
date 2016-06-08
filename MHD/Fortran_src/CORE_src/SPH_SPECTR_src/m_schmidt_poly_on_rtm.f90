!>@file   m_schmidt_poly_on_rtm.f90
!!@brief  module m_schmidt_poly_on_rtm
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief Parameters for LEgendre transforms
!!
!!@verbatim
!!      subroutine allocate_gauss_colat_rtm(nth_rtm)
!!      subroutine allocate_schmidt_poly_rtm(nth_rtm, jmax_rlm, jmax_rj)
!!      subroutine allocate_hemi_schmidt_rtm(nth_rtm, jmax_rlm)
!!      subroutine allocate_trans_schmidt_rtm(nth_rtm, jmax_rlm)
!!      subroutine allocate_schmidt_p_rtm_pole(jmax_rlm)
!!
!!      subroutine deallocate_gauss_colat_rtm
!!      subroutine deallocate_schmidt_poly_rtm
!!      subroutine deallocate_hemi_schmidt_rtm
!!      subroutine deallocate_trans_schmidt_rtm
!!      subroutine deallocate_schmidt_p_rtm_pole
!!
!!      subroutine check_schmidt_p_rtm_pole(my_rank, sph_rlm)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!@endverbatim
!
      module m_schmidt_poly_on_rtm
!
      use m_precision
      use t_schmidt_poly_on_rtm
!
      implicit none
!
!>      Structures for Legendre polynomials for spherical transform
      type(legendre_4_sph_trans), save :: leg1
!leg1%P_rtm
!!
      real(kind = kreal), allocatable :: g_point_rtm(:)
      real(kind = kreal), allocatable :: g_colat_rtm(:)
      real(kind = kreal), allocatable :: weight_rtm(:)
!
!>        @$f P_{l}{m} @$f at gouss points
!      real(kind = kreal), allocatable :: P_rtm(:,:)
!>        @$f dP_{l}{m}/d\theta @$f at gouss points
!      real(kind = kreal), allocatable :: dPdt_rtm(:,:)
!
!>        Normalization constants for spherical harmonics in (r,l,m)
      real(kind = kreal), allocatable:: g_sph_rlm(:,:)
!>        Normalization constants for spherical harmonics in (r,j)
      real(kind = kreal), allocatable:: g_sph_rj(:,:)
!
!>        @$f P_{l}{m} @$f at poles
!      real(kind = kreal), allocatable :: P_pole_rtm(:,:)
!>        @$f dP_{l}{m}/d\theta @$f at poles
!      real(kind = kreal), allocatable :: dPdt_pole_rtm(:,:)
!
!
!>        @$f P_{l}{m} @$f with A(j,theta)
!      real(kind = kreal), allocatable :: P_jl(:,:)
!>        @$f dP_{l}{m}/d\theta @$f with A(j,theta)
!      real(kind = kreal), allocatable :: dPdt_jl(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_gauss_colat_rtm(nth_rtm)
!
      integer(kind = kint), intent(in) :: nth_rtm
!
      allocate( g_point_rtm(nth_rtm) )
      allocate( g_colat_rtm(nth_rtm) )
      allocate( weight_rtm(nth_rtm) )
!
      g_point_rtm = 0.0d0
      g_colat_rtm = 0.0d0
      weight_rtm = 0.0d0
!
      end subroutine allocate_gauss_colat_rtm
!
! -----------------------------------------------------------------------
!
      subroutine allocate_schmidt_poly_rtm(nth_rtm, jmax_rlm, jmax_rj)
!
      integer(kind = kint), intent(in) :: nth_rtm, jmax_rlm, jmax_rj
!
!
      allocate( leg1%P_rtm(nth_rtm,jmax_rlm) )
      allocate( leg1%dPdt_rtm(nth_rtm,jmax_rlm) )
!
      allocate( g_sph_rlm(jmax_rlm,17) )
      allocate( g_sph_rj(jmax_rj,13) )
!
      leg1%P_rtm = 0.0d0
      leg1%dPdt_rtm = 0.0d0
!
      g_sph_rlm = 0.0d0
      g_sph_rj =  0.0d0
!
      end subroutine allocate_schmidt_poly_rtm
!
! -----------------------------------------------------------------------
!
      subroutine allocate_trans_schmidt_rtm(nth_rtm, jmax_rlm)
!
      integer(kind = kint), intent(in) :: nth_rtm, jmax_rlm
!
!
      call alloc_trans_schmidt_rtm(nth_rtm, jmax_rlm, leg1)
!
      end subroutine allocate_trans_schmidt_rtm
!
! -----------------------------------------------------------------------
!
      subroutine allocate_schmidt_p_rtm_pole(jmax_rlm)
!
      integer(kind = kint), intent(in) :: jmax_rlm
!
!
      call alloc_schmidt_p_rtm_pole(jmax_rlm, leg1)
!
      end subroutine allocate_schmidt_p_rtm_pole
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_gauss_colat_rtm
!
      deallocate( g_point_rtm )
      deallocate( g_colat_rtm )
      deallocate( weight_rtm )
!
      end subroutine deallocate_gauss_colat_rtm
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_schmidt_poly_rtm
!
      deallocate( leg1%P_rtm, leg1%dPdt_rtm)
      deallocate( g_sph_rlm, g_sph_rj)
!
      end subroutine deallocate_schmidt_poly_rtm
!
! -----------------------------------------------------------------------
!
      end module m_schmidt_poly_on_rtm
