!
!      module full_legendre_on_meridian
!
!      Written by H. Matsui on June, 2007
!
!!      subroutine cal_full_legendre_on_med(nth, lst, led, ltr, leg_d)
!!        type(gauss_legendre_data), intent(inout) :: leg_d
!
      module full_legendre_on_meridian
!
      use m_precision
      use t_schmidt_polynomial
      use t_schmidt_poly_on_rtm
      use t_spherical_harmonics
      use t_gauss_points
!
      implicit none
!
      type gauss_legendre_data
!>      Legendre polynomials with no normalization
        real(kind = kreal), allocatable :: P_org(:,:)
      end type gauss_legendre_data
!
      real(kind = kreal), allocatable :: dint_p(:)
!
      type(legendre_polynomials), save, private :: leg_test
      type(sph_1point_type),  save, private :: sph_1pt
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_full_legendre_on_med(nth, lst, led, ltr)
!
      use gauss_integration
      use spherical_harmonics
      use schmidt_poly_on_rtm_grid
!
      integer(kind = kint), intent(in) :: nth, lst, led, ltr
!
      type(gauss_points) :: gauss
      type(legendre_4_sph_trans) :: leg
      type(gauss_legendre_data) :: leg_d
      integer(kind = kint) :: i, j, m, l
!
!
      call alloc_gauss_colat_rtm(nth, leg)
!
!     set gauss colatitudes
!
      call construct_gauss_coefs(leg%nth_P_tm, gauss)
      call set_gauss_colatitude(gauss)
      call set_gauss_points_rtm(gauss, leg)
!
!     set Legendre polynomials
!
      call alloc_schmidt_polynomial(ltr, leg_test)
!
      call alloc_index_4_sph(ltr, sph_1pt)
      call idx28(ltr_tri_sph, jmax_tri_sph, idx, g)
!
      call alloc_schmidt_normalize                                      &
     &   (sph_1pt%jmax_tri, sph_1pt%jmax_tri, leg)
      call alloc_schmidt_poly_rtm(leg)
      call alloc_legendre_med(leg, leg_d)
!
      allocate(dint_p(0:leg%jmax_P_lm))
      dint_p = 0.0d0
!
      do i = lst, led
        call full_norm_legendre(leg%g_colat_rtm(i), leg_test)
!
        do j = 0, leg%jmax_P_lm
          l = sph_1pt%idx(j,1)
          m = abs( sph_1pt%idx(j,2) )
          leg%P_rtm(i,j) =    leg_test%p(m,l)
          leg%dPdt_rtm(i,j) = leg_test%dp(m,l)
        end do
!
        call dlad(leg%g_colat_rtm(i), leg_test)
        do j = 0, leg%jmax_P_lm
          l = sph_1pt%idx(j,1)
          m = abs( sph_1pt%idx(j,2) )
          leg_d%P_org(i,j) = leg_test%dplm(m,l)
        end do
      end do
!
      dint_p = 0.0d0
      do j = 0, leg%jmax_P_lm
        l = sph_1pt%idx(j,1)
        m = abs( sph_1pt%idx(j,2) )
        do i = 1, gauss%n_point
          dint_p(j) = dint_p(j) + gauss%weight(i) * leg%P_rtm(i,j)**2
        end do
      end do
!
      call dealloc_gauss_colatitude(gauss)
      call dealloc_gauss_points(gauss)
      call dealloc_schmidt_polynomial(leg_test)
!
!      call check_gauss_colat_rtm(izero, leg)
!
      call check_normalized_legendre(ltr, lst, led, leg, leg_d)
      call dealloc_legendre_med(leg_d)
!
      end subroutine cal_full_legendre_on_med
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_legendre_med(leg, leg_d)
!
      type(legendre_4_sph_trans), intent(in) :: leg
      type(gauss_legendre_data), intent(inout) :: leg_d
!
      allocate( leg_d%P_org(leg%nth_P_tm, 0:leg%jmax_P_lm) )
      if(leg%nth_P_tm .gt. 0) leg_d%P_org = 0.0d0
!
      end subroutine alloc_legendre_med
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_legendre_med(leg_d)
!
      type(gauss_legendre_data), intent(inout) :: leg_d
!
      deallocate( leg_d%P_org )
!
      end subroutine dealloc_legendre_med
!
! -----------------------------------------------------------------------
!
      subroutine check_normalized_legendre(ltr, lst, led, leg, leg_d)
!
      integer(kind = kint) :: i, l, m, j
!
      integer(kind = kint), intent(in) :: ltr, lst, led
      type(legendre_4_sph_trans), intent(in) :: leg
      type(gauss_legendre_data), intent(in) :: leg_d
!
!
      open (50,file='schmidt_polynomials.dat')
      write(50,'(a)') 'j, l, m, i, theta, P_lm, dPdr_lm'
      l = ltr
      do m = 0, l
        j = l*(l+1) + m
        do i = lst, led
          write(50,'(4i16,1p4E25.15e3)') j, l, m, i,                    &
     &      leg%g_colat_rtm(i), leg_d%P_org(i,j),                       &
     &      leg%P_rtm(i,j), leg%dPdt_rtm(i,j)
        end do
      end do
      close(50)
!
      end subroutine check_normalized_legendre
!
! -----------------------------------------------------------------------
!
      end module full_legendre_on_meridian
