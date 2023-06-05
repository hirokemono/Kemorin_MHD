!>@file   sum_sph_rms_by_degree.f90
!!@brief      module sum_sph_rms_by_degree
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!> @brief  Evaluate mean square by spherical hermonics coefficients
!!
!!@verbatim
!!      subroutine sum_sph_v_rms_by_degree(ltr, nidx_j, istack_sum,     &
!!     &          item_mode_4_sum, ncomp, rms_sph_vol_j, rms_sph_vlc)
!!        integer(kind = kint), intent(in) :: ltr, nidx_j
!!        integer(kind = kint), intent(in) :: ncomp
!!        integer(kind = kint), intent(in) :: istack_sum(-1:ltr)
!!        integer(kind = kint), intent(in) :: item_mode_4_sum(nidx_j)
!!        real(kind = kreal), intent(in) :: rms_sph_vol_j(nidx_j,3)
!!        real(kind = kreal), intent(inout) :: rms_sph_vlc(0:ltr,ncomp)
!!      subroutine sum_sph_l_rms_by_degree(pwr, ltr, nidx_rj,           &
!!     &          istack_sum, item_mode_4_sum, ncomp,                   &
!!     &          rms_sph_rj, rms_sph_lc)
!!        type(sph_mean_squares), intent(in) :: pwr
!!        integer(kind = kint), intent(in) :: ltr
!!        integer(kind = kint), intent(in) :: nidx_rj(2)
!!        integer(kind = kint), intent(in) :: istack_sum(-1:ltr)
!!        integer(kind = kint), intent(in) :: item_mode_4_sum(nidx_rj(2))
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: rms_sph_rj(0:nidx_rj(1),nidx_rj(2),3)
!!        integer(kind = kint), intent(in) :: ncomp
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: rms_sph_lc(pwr%nri_rms,0:ltr,ncomp)
!!@endverbatim
!
      module sum_sph_rms_by_degree
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sum_sph_v_rms_by_degree(ltr, nidx_j, istack_sum,       &
     &          item_mode_4_sum, ncomp, rms_sph_vol_j, rms_sph_vlc)
!
      integer(kind = kint), intent(in) :: ltr, nidx_j
      integer(kind = kint), intent(in) :: ncomp
!
      integer(kind = kint), intent(in) :: istack_sum(-1:ltr)
      integer(kind = kint), intent(in) :: item_mode_4_sum(nidx_j)
      real(kind = kreal), intent(in) :: rms_sph_vol_j(nidx_j,3)
!
      real(kind = kreal), intent(inout) :: rms_sph_vlc(0:ltr,ncomp)
!
      integer(kind = kint) :: lm, j, l0, icomp
      integer(kind = kint) :: lst, led
!
!
!$omp parallel private(icomp)
      do icomp = 1, ncomp
!$omp do private(lm,lst,led,l0,j)
        do lm = 0, ltr
          lst = istack_sum(lm-1) + 1
          led = istack_sum(lm)
          do l0 = lst, led
            j = item_mode_4_sum(l0)
            rms_sph_vlc(lm,icomp) = rms_sph_vlc(lm,icomp)               &
     &                                + rms_sph_vol_j(j,icomp)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine sum_sph_v_rms_by_degree
!
! -----------------------------------------------------------------------
!
      subroutine sum_sph_l_rms_by_degree(pwr, ltr, nidx_rj,             &
     &          istack_sum, item_mode_4_sum, ncomp,                     &
     &          rms_sph_rj, rms_sph_lc)
!
      use t_rms_4_sph_spectr
!
      type(sph_mean_squares), intent(in) :: pwr
      integer(kind = kint), intent(in) :: ltr
      integer(kind = kint), intent(in) :: nidx_rj(2)
!
      integer(kind = kint), intent(in) :: istack_sum(-1:ltr)
      integer(kind = kint), intent(in) :: item_mode_4_sum(nidx_rj(2))
      real(kind = kreal), intent(in)                                    &
     &                   :: rms_sph_rj(0:nidx_rj(1),nidx_rj(2),3)
      integer(kind = kint), intent(in) :: ncomp
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: rms_sph_lc(pwr%nri_rms,0:ltr,ncomp)
!
      integer(kind = kint) :: lm, k, k_in, k_out, j, l0, icomp
      integer(kind = kint) :: lst, led
!
!
!$omp parallel private(icomp)
      do icomp = 1, ncomp
!$omp do private(k,k_in,k_out,lm,lst,led,l0,j)
        do k = 1, pwr%nri_rms
          k_in =  pwr%kr_4_rms(k,1)
          k_out = pwr%kr_4_rms(k,2)
          do lm = 0, ltr
            lst = istack_sum(lm-1) + 1
            led = istack_sum(lm)
            do l0 = lst, led
              j = item_mode_4_sum(l0)
              rms_sph_lc(k,lm,icomp) = rms_sph_lc(k,lm,icomp)           &
     &            + pwr%c_gl_itp(k) * rms_sph_rj(k_in,j,icomp)          &
     &            + (one - pwr%c_gl_itp(k)) * rms_sph_rj(k_out,j,icomp)
            end do
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine sum_sph_l_rms_by_degree
!
! -----------------------------------------------------------------------
!
      end module sum_sph_rms_by_degree
