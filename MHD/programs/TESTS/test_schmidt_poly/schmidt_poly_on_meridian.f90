!
!      module schmidt_poly_on_meridian
!
!      Written by H. Matsui on June, 2007
!
!!      subroutine cal_full_legendre_on_med(lst, led, leg_d)
!!        type(gauss_legendre_data), intent(inout) :: leg_d
!
      module schmidt_poly_on_meridian
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable :: dint_p(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_full_legendre_on_med(lst, led, leg_d)
!
      use t_schmidt_poly_on_gauss
      use t_schmidt_polynomial
      use t_spherical_harmonics
      use m_gauss_points
      use spherical_harmonics
!
      integer(kind = kint), intent(in) :: lst, led
      type(gauss_legendre_data), intent(inout) :: leg_d
!
      integer(kind = kint) :: i, j, m, l
      type(legendre_polynomials) :: leg_1pt
      type(sph_1point_type) :: sph_1pt
!
!
      call alloc_gauss_colat_med(leg_d)
!
!     set gauss colatitudes
!
      call construct_gauss_coefs(leg_d%nth_g, gauss1)
      call set_gauss_colatitude(gauss1)
!
      leg_d%g_point_med(1:leg_d%nth_g) = gauss1%point(1:leg_d%nth_g)
      leg_d%g_colat_med(1:leg_d%nth_g) = gauss1%colat(1:leg_d%nth_g)
      leg_d%weight_med(1:leg_d%nth_g) =  gauss1%weight(1:leg_d%nth_g)
!
!
!     set Legendre polynomials
!
      call init_sph_indices(leg_d%ltr_g, leg_1pt, sph_1pt)
!
      leg_d%jmax_g = sph_1pt%jmax_tri
!
      call alloc_schmidt_poly_med(leg_d)
      call alloc_legendre_med(leg_d)
!
      allocate(dint_p(0:leg_d%jmax_g))
      dint_p = 0.0d0
!
      do i = lst, led
        call full_norm_legendre(leg_d%g_colat_med(i), leg_1pt)
!
        do j = 0, leg_d%jmax_g
          l = sph_1pt%idx(j,1)
          m = abs( sph_1pt%idx(j,2) )
          leg_d%P_smdt(i,j) =    leg_1pt%p(m,l)
          leg_d%dPdt_smdt(i,j) = leg_1pt%dp(m,l)
        end do
!
        call dlad(leg_d%g_colat_med(i), leg_1pt)
        do j = 0, leg_d%jmax_g
          l = sph_1pt%idx(j,1)
          m = abs( sph_1pt%idx(j,2) )
          leg_d%P_org(i,j) = leg_1pt%dplm(m,l)
        end do
      end do
!
      dint_p = 0.0d0
      do j = 0, leg_d%jmax_g
        l = sph_1pt%idx(j,1)
        m = abs( sph_1pt%idx(j,2) )
        do i = 1, gauss1%n_point
          dint_p(j) = dint_p(j)                                         &
     &               + gauss1%weight(i) * leg_d%P_smdt(i,j)**2
        end do
      end do
!
      call dealloc_gauss_colatitude(gauss1)
      call dealloc_gauss_points(gauss1)
      call finalize_sph_indices(leg_1pt, sph_1pt)
!
      end subroutine cal_full_legendre_on_med
!
! -----------------------------------------------------------------------
!
      end module schmidt_poly_on_meridian
