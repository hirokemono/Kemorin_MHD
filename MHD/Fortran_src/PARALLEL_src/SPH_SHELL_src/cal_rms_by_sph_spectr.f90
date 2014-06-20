!>@file   cal_rms_by_sph_spectr.f90
!!@brief  module cal_rms_by_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief  Evaluate mean square data for each spherical harmonics mode
!!
!!@verbatim
!!      subroutine cal_rms_each_scalar_sph_spec(d_rj, rms_sph_dat)
!!      subroutine cal_rms_each_vector_sph_spec(d_rj, rms_sph_dat)
!!        (1/4\pi) \int (\bf{u}_{l}^{m})^2 sin \theta d\theta d\phi
!!          = r^{-2} [ l(l+1) / (2l+1) 
!!           ( l(l+1)/r^2 (S_{l}^{m})^2 + (dS_{l}^{m}/dr)^2)
!!            + (T_{l}^{m})^2 ) ]
!!      subroutine set_sph_energies_by_rms(rms_sph_dat)
!!@endverbatim
!!
!!@n @param  d_rj          spectrum data
!!@n @param  rms_sph_dat   mean square data
!
      module cal_rms_by_sph_spectr
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_rms_each_scalar_sph_spec(d_rj, rms_sph_dat)
!
      real(kind = kreal), intent(in) :: d_rj(nnod_rj)
      real(kind = kreal), intent(inout) :: rms_sph_dat(nnod_rj)
!
      integer(kind = kint) :: k, j, inod
!
!
!$omp parallel do private(k,j,inod)
      do k = 1, nidx_rj(1)
        do j = 1, nidx_rj(2)
          inod = j + (k-1) * nidx_rj(2)
          rms_sph_dat(inod) = d_rj(inod)*d_rj(inod)*g_sph_rj(j,11)      &
     &         * radius_1d_rj_r(k) * radius_1d_rj_r(k)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_rms_each_scalar_sph_spec
!
! -----------------------------------------------------------------------
!
      subroutine cal_rms_each_vector_sph_spec(d_rj, rms_sph_dat)
!
      real(kind = kreal), intent(in) :: d_rj(nnod_rj,3)
      real(kind = kreal), intent(inout) :: rms_sph_dat(nnod_rj,3)
!
      integer(kind = kint) :: k, j, inod
!
!
!$omp parallel do private(k,j,inod)
      do k = 1, nidx_rj(1)
        do j = 1, nidx_rj(2)
          inod = j + (k-1) * nidx_rj(2)
          rms_sph_dat(inod,1) = g_sph_rj(j,12)                          &
     &                        * ( g_sph_rj(j,3)                         &
     &                          * a_r_1d_rj_r(k)*a_r_1d_rj_r(k)         &
     &                          * d_rj(inod,1)*d_rj(inod,1)             &
     &                         +  d_rj(inod,2)*d_rj(inod,2))
          rms_sph_dat(inod,2) = g_sph_rj(j,12)                          &
     &                          * d_rj(inod,3)*d_rj(inod,3)
          rms_sph_dat(inod,3) =  rms_sph_dat(inod,1)                    &
     &                         + rms_sph_dat(inod,2)
        end do
      end do
!$omp end parallel do
!
      if(idx_rj_degree_zero .eq. izero) return
!
      j = idx_rj_degree_zero
      do k = 1, nidx_rj(1)
        inod = idx_rj_degree_zero + (k-1) * nidx_rj(2)
        rms_sph_dat(inod,1) = (half * d_rj(inod,1))**2                  &
     &                            * a_r_1d_rj_r(k)*a_r_1d_rj_r(k)
        rms_sph_dat(inod,2) = zero
        rms_sph_dat(inod,3) = rms_sph_dat(inod,1)
      end do
!
      end subroutine cal_rms_each_vector_sph_spec
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_energies_by_rms(rms_sph_dat)
!
      real(kind = kreal), intent(inout) :: rms_sph_dat(nnod_rj,3)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, nnod_rj
        rms_sph_dat(inod,1) = half * rms_sph_dat(inod,1)
        rms_sph_dat(inod,2) = half * rms_sph_dat(inod,2)
        rms_sph_dat(inod,3) = half * rms_sph_dat(inod,3)
      end do
!$omp end parallel do
!
      end subroutine set_sph_energies_by_rms
!
! -----------------------------------------------------------------------
!
      end module cal_rms_by_sph_spectr
