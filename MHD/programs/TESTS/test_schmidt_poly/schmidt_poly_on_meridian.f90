!
!      module schmidt_poly_on_meridian
!
      module schmidt_poly_on_meridian
!
!      Written by H. Matsui on June, 2007
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable :: dint_p(:)
!
!
!      subroutine cal_full_legendre_on_med
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_full_legendre_on_med
!
      use m_schmidt_poly_on_gauss
      use m_gauss_points
      use m_schmidt_polynomial
      use m_spherical_harmonics
      use spherical_harmonics
!
      integer(kind = kint) :: i, j, m, l
!
!
      call allocate_gauss_colat_med
!
!     set gauss colatitudes
!
      n_point = nth_g
      call allocate_gauss_points
      call construct_gauss_coefs
!
      call allocate_gauss_colatitude
      call set_gauss_colatitude
!
      g_point_med(1:nth_g) = w_point(1:nth_g)
      g_colat_med(1:nth_g) = w_colat(1:nth_g)
      weight_med(1:nth_g) =  w_coefs(1:nth_g)
!
!
!     set Legendre polynomials
!
      nth = ltr_g
      call allocate_index_4_sph(nth)
      call allocate_schmidt_polynomial
!
      call idx28
      jmax_g = jmax_tri_sph
!
      call allocate_schmidt_poly_med
!
      allocate(dint_p(0:jmax_g))
      dint_p = 0.0d0
!
      do i = 1, nth_g
        dth = g_colat_med(i)
        call full_norm_legendre
!
        do j = 0, jmax_g
          l = idx(j,1)
          m = abs( idx(j,2) )
          P_smdt(i,j) =    p(m,l)
          dPdt_smdt(i,j) = dp(m,l)
        end do
!
      end do
!
      dint_p = 0.0d0
      do j = 0, jmax_g
        l = idx(j,1)
        m = abs( idx(j,2) )
        do i = 1, n_point
          dint_p(j) = dint_p(j) + w_coefs(i) * P_smdt(i,j)**2
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
