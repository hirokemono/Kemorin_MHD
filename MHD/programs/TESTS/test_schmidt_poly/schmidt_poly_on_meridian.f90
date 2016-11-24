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
      use m_gauss_points
      use m_schmidt_polynomial
      use m_spherical_harmonics
      use spherical_harmonics
!
      integer(kind = kint), intent(in) :: lst, led
      type(gauss_legendre_data), intent(inout) :: leg_d
!
      integer(kind = kint) :: i, j, m, l
!
!
      call alloc_gauss_colat_med(leg_d)
!
!     set gauss colatitudes
!
      call allocate_gauss_points(leg_d%nth_g)
      call construct_gauss_coefs
!
      call allocate_gauss_colatitude
      call set_gauss_colatitude
!
      leg_d%g_point_med(1:leg_d%nth_g) = w_point(1:leg_d%nth_g)
      leg_d%g_colat_med(1:leg_d%nth_g) = w_colat(1:leg_d%nth_g)
      leg_d%weight_med(1:leg_d%nth_g) =  w_coefs(1:leg_d%nth_g)
!
!
!     set Legendre polynomials
!
      nth = leg_d%ltr_g
      call allocate_index_4_sph(nth)
      call allocate_schmidt_polynomial
!
      call idx28(ltr_tri_sph, jmax_tri_sph, idx, g)
      leg_d%jmax_g = jmax_tri_sph
!
      call alloc_schmidt_poly_med(leg_d)
      call alloc_legendre_med(leg_d)
!
      allocate(dint_p(0:leg_d%jmax_g))
      dint_p = 0.0d0
!
      do i = lst, led
        call full_norm_legendre(leg_d%g_colat_med(i))
!
        do j = 0, leg_d%jmax_g
          l = idx(j,1)
          m = abs( idx(j,2) )
          leg_d%P_smdt(i,j) =    p(m,l)
          leg_d%dPdt_smdt(i,j) = dp(m,l)
        end do
!
        call dlad(leg_d%g_colat_med(i))
        do j = 0, leg_d%jmax_g
          l = idx(j,1)
          m = abs( idx(j,2) )
          leg_d%P_org(i,j) = dplm(m,l)
        end do
      end do
!
      dint_p = 0.0d0
      do j = 0, leg_d%jmax_g
        l = idx(j,1)
        m = abs( idx(j,2) )
        do i = 1, n_point
          dint_p(j) = dint_p(j) + w_coefs(i) * leg_d%P_smdt(i,j)**2
        end do
      end do
!
      call deallocate_gauss_colatitude
      call deallocate_gauss_points
      call deallocate_schmidt_polynomial
!
      end subroutine cal_full_legendre_on_med
!
! -----------------------------------------------------------------------
!
      end module schmidt_poly_on_meridian
