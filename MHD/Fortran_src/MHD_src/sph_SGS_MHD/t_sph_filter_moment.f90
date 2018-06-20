!>@file   t_sph_filter_moment.f90
!!@brief  module t_sph_filter_moment
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate horizontal filtering in spectrunm space
!!
!!@verbatim
!!      subroutine alloc_sph_filter_moms(mom)
!!      subroutine dealloc_sph_filter_moms(mom)
!!        type(sph_filter_moment), intent(inout) :: mom
!!
!!      subroutine cal_r_gaussian_moments(filter_length, mom)
!!        type(sph_filter_moment), intent(inout) :: mom
!!
!!      subroutine cal_sph_2nd_order_moments_rtp                        &
!!     &         (sph_rtp, sph_rj, leg, r_moments, sph_moments,         &
!!     &          radial_2nd_moment, theta_2nd_moment, phi_2nd_moment)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(sph_filter_moment), intent(in) :: r_moments
!!        type(sph_filter_moment), intent(in) :: sph_moments
!!@endverbatim
!!
!
      module t_sph_filter_moment
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_spheric_parameter
      use t_schmidt_poly_on_rtm
      use t_group_data
!
      implicit none
!
!
!> filter moments for spherical shell model
      type sph_filter_moment
!>        Truncation degree
        integer(kind = kint) :: num_momentum
        integer(kind = kint) :: nfilter_sides = 3
!
!>        Momentums of filter
        real(kind = kreal), allocatable :: filter_mom(:)
      end type sph_filter_moment
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_sph_filter_moms(mom)
!
      type(sph_filter_moment), intent(inout) :: mom
!
!
      mom%nfilter_sides = (mom%num_momentum + 1) / 2
      allocate(mom%filter_mom(0:mom%num_momentum-1))
      mom%filter_mom = 0.0d0
!
      end subroutine alloc_sph_filter_moms
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_sph_filter_moms(mom)
!
      type(sph_filter_moment), intent(inout) :: mom
!
!
      deallocate(mom%filter_mom)
!
      end subroutine dealloc_sph_filter_moms
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_r_gaussian_moments(filter_length, mom)
!
      real(kind = kreal), intent(in) :: filter_length
      type(sph_filter_moment), intent(inout) :: mom
!
      integer(kind = kint) :: imom
!
!
      mom%filter_mom(0) = one
      do imom = 1, mom%nfilter_sides-1
        mom%filter_mom(2*imom-1) = zero
        mom%filter_mom(2*imom  )                                        &
     &              = real(2*imom-1) * mom%filter_mom(2*imom-2)         &
     &               * (filter_length**2 / 6.0d0)
      end do
!
      end subroutine cal_r_gaussian_moments
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_2nd_order_moments_rtp                          &
     &         (sph_rtp, sph_rj, leg, r_moments, sph_moments,           &
     &          radial_2nd_moment, theta_2nd_moment, phi_2nd_moment)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
!
      type(sph_filter_moment), intent(in) :: r_moments
      type(sph_filter_moment), intent(in) :: sph_moments
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: radial_2nd_moment(sph_rtp%nidx_rtp(1))
      real(kind = kreal), intent(inout)                                 &
     &                   :: theta_2nd_moment(sph_rtp%nidx_rtp(2))
      real(kind = kreal), intent(inout) :: phi_2nd_moment
!
      integer(kind = kint) :: k, l, kr_gl, lt_gl
      real(kind = kreal) :: dr, dtheta, dphi, pi
!
!
      pi = four * atan(one)
!
      do k = 1, sph_rtp%nidx_rtp(1)
        kr_gl = sph_rtp%idx_gl_1d_rtp_r(k)
        if(kr_gl .eq. 1) then
          dr =  sph_rj%radius_1d_rj_r(2) - sph_rj%radius_1d_rj_r(1)
          if(four*dr .gt. sph_rj%radius_1d_rj_r(1)) then
            dr = half * sph_rj%radius_1d_rj_r(2)
          end if
        else if(kr_gl .eq. sph_rtp%nidx_global_rtp(1)) then
          dr =  sph_rj%radius_1d_rj_r(kr_gl  )                          &
     &        - sph_rj%radius_1d_rj_r(kr_gl-1)
        else
          dr = half * (sph_rj%radius_1d_rj_r(kr_gl+1)                   &
     &               - sph_rj%radius_1d_rj_r(kr_gl-1))
        end if
        radial_2nd_moment(k) = r_moments%filter_mom(2) * dr*dr
      end do
!
      do l = 1, sph_rtp%nidx_rtp(2)
        lt_gl = sph_rtp%idx_gl_1d_rtp_t(l)
        if(lt_gl .eq. 1) then
          dtheta = half * (pi - leg%g_colat_rtm(2))
        else if(lt_gl .eq. sph_rtp%nidx_global_rtp(2)) then
          dtheta = half * leg%g_colat_rtm(lt_gl-1)
        else
          dtheta = half * (leg%g_colat_rtm(lt_gl+1)                     &
     &                   - leg%g_colat_rtm(lt_gl-1))
        end if
        theta_2nd_moment(l)                                             &
     &         = sph_moments%filter_mom(2) * dtheta*dtheta
      end do
!
      dphi = two * pi / dble(sph_rtp%nidx_global_rtp(3))
      phi_2nd_moment = sph_moments%filter_mom(2) * dphi*dphi
!
      end subroutine cal_sph_2nd_order_moments_rtp
!
! -----------------------------------------------------------------------
!
      end module t_sph_filter_moment
