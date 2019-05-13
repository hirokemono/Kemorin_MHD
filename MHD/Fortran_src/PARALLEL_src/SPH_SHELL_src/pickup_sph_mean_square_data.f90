!>@file   pickup_sph_mean_square_data.f90
!!@brief  module pickup_sph_mean_square_data
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine rms_rj_spectrum_4_monitor                            &
!!     &        (inum, knum, sph_rj, rj_fld, leg, picked,               &
!!     &         ntot_comp_rj, rms_out)
!!      subroutine rms_rj_degree0_monitor(knum, sph_rj, rj_fld, picked, &
!!     &         ntot_comp_rj, rms_out)
!!      subroutine rms_rj_center_monitor(sph_rj, rj_fld, picked,        &
!!     &         ntot_comp_rj, rms_out)
!!      subroutine init_sph_spec_4_monitor(sph_params, sph_rj, rj_fld,  &
!!     &          pick_list, picked)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_data), intent(in) :: rj_fld
!!        type(picked_spectrum_data), intent(in) :: picked
!!
!!      subroutine convert_to_energy_sph__monitor                       &
!!     &         (ipol, picked, ntot_comp_monitor, rms_out)
!!        type(phys_address), intent(in) :: ipol
!!        type(picked_spectrum_data), intent(in) :: picked
!!@endverbatim
!!
!!@n @param  my_rank   Process ID
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module pickup_sph_mean_square_data
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
      use t_schmidt_poly_on_rtm
      use t_phys_address
      use t_phys_data
!
      implicit  none
!
      private :: cal_rj_mean_sq_spectr_monitor
      private :: cal_rj_mean_sq_degree0_monitor
      private :: cal_rj_mean_sq_center_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine rms_rj_spectrum_4_monitor                              &
     &        (inum, knum, sph_rj, rj_fld, leg, picked,                 &
     &         ntot_comp_rj, rms_out)
!
      integer(kind = kint), intent(in) :: inum, knum

      type(sph_rj_grid), intent(in) :: sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_data), intent(in) :: rj_fld
!
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_rj
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_rj)
!
!
      call cal_rj_mean_sq_spectr_monitor                                &
     &   (picked%idx_out(inum,4), picked%id_radius(knum),               &
     &    sph_rj%nidx_rj, sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,    &
     &    leg%g_sph_rj, rj_fld%n_point, rj_fld%num_phys,                &
     &    rj_fld%ntot_phys, rj_fld%istack_component, rj_fld%d_fld,      &
     &    picked%num_field_rj, picked%istack_comp_rj,                   &
     &    picked%ifield_monitor_rj, ntot_comp_rj, rms_out)
!
      end subroutine rms_rj_spectrum_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine rms_rj_degree0_monitor(knum, sph_rj, rj_fld, picked,   &
     &         ntot_comp_rj, rms_out)
!
      integer(kind = kint), intent(in) :: knum

      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_rj
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_rj)
!
!
      call cal_rj_mean_sq_degree0_monitor                               &
     &   (picked%id_radius(knum), sph_rj%idx_rj_degree_zero,            &
     &    sph_rj%nidx_rj, sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,    &
     &    rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,            &
     &    rj_fld%istack_component, rj_fld%d_fld, picked%num_field_rj,   &
     &    picked%istack_comp_rj, picked%ifield_monitor_rj,              &
     &    ntot_comp_rj, rms_out)
!
      end subroutine rms_rj_degree0_monitor
!
! -----------------------------------------------------------------------
!
      subroutine rms_rj_center_monitor(sph_rj, rj_fld, picked,          &
     &         ntot_comp_rj, rms_out)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_rj
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_rj)
!
!
      call cal_rj_mean_sq_center_monitor(sph_rj%inod_rj_center,         &
     &    rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,            &
     &    rj_fld%istack_component, rj_fld%d_fld, picked%num_field_rj,   &
     &    picked%istack_comp_rj, picked%ifield_monitor_rj,              &
     &    ntot_comp_rj, rms_out)
!
      end subroutine rms_rj_center_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rj_mean_sq_spectr_monitor                          &
     &        (j, k, nidx_rj, radius_1d_rj_r, a_r_1d_rj_r, g_sph_rj,    &
     &         n_point, num_phys_rj, ntot_phys_rj, istack_phys_comp_rj, &
     &         d_rj, nfld_monitor, istack_comp_monitor, ifld_monitor,   &
     &         ntot_comp_monitor, rms_out)
!
      integer(kind = kint), intent(in) :: j, k
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: n_point
      integer(kind = kint), intent(in) :: num_phys_rj, ntot_phys_rj
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_phys_comp_rj(0:num_phys_rj)
      real(kind=kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
      real(kind = kreal), intent(in) :: g_sph_rj(nidx_rj(2),13)
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      integer(kind = kint), intent(in) :: nfld_monitor
      integer(kind = kint), intent(in) :: ntot_comp_monitor
      integer(kind = kint), intent(in) :: ifld_monitor(nfld_monitor)
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_comp_monitor(0:nfld_monitor)
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_monitor)
!
      integer(kind = kint) :: nd, i_fld, j_fld, icou, jcou, ncomp, inod
!
!
      inod = j + (k-1) * nidx_rj(2)
      do j_fld = 1, nfld_monitor
        i_fld = ifld_monitor(j_fld)
        ncomp = istack_phys_comp_rj(i_fld)                              &
     &         - istack_phys_comp_rj(i_fld-1)
        icou = istack_phys_comp_rj(i_fld-1)
        jcou = istack_comp_monitor(j_fld-1)
        if(ncomp .eq. 3) then
            rms_out(jcou+1) = g_sph_rj(j,12) * (d_rj(inod,icou+2)**2    &
     &       + g_sph_rj(j,3) * (a_r_1d_rj_r(k) * d_rj(inod,icou+1))**2)
            rms_out(jcou+2) = g_sph_rj(j,12) * d_rj(inod,icou+3)**2
            rms_out(jcou+3) =  rms_out(1) + rms_out(2)
        else
          do nd = 1, ncomp
            rms_out(jcou+nd) = g_sph_rj(j,11)                           &
     &           * (d_rj(inod,icou+nd) * radius_1d_rj_r(k))**2
          end do
        end if
      end do
!
      end subroutine cal_rj_mean_sq_spectr_monitor
!
! -----------------------------------------------------------------------
!
      subroutine cal_rj_mean_sq_degree0_monitor(k, idx_rj_degree_zero,  &
     &          nidx_rj, radius_1d_rj_r, a_r_1d_rj_r, n_point,          &
     &          num_phys_rj, ntot_phys_rj, istack_phys_comp_rj, d_rj,   &
     &          nfld_monitor, istack_comp_monitor, ifld_monitor,        &
     &          ntot_comp_monitor, rms_out)
!
      integer(kind = kint), intent(in) :: k
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: n_point
      integer(kind = kint), intent(in) :: num_phys_rj, ntot_phys_rj
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_phys_comp_rj(0:num_phys_rj)
      real(kind=kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      integer(kind = kint), intent(in) :: nfld_monitor
      integer(kind = kint), intent(in) :: ntot_comp_monitor
      integer(kind = kint), intent(in) :: ifld_monitor(nfld_monitor)
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_comp_monitor(0:nfld_monitor)
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_monitor)
!
      integer(kind = kint) :: nd, i_fld, j_fld, icou, jcou, ncomp, inod
!
!
      inod = idx_rj_degree_zero + (k-1) * nidx_rj(2)
      do j_fld = 1, nfld_monitor
        i_fld = ifld_monitor(j_fld)
        ncomp = istack_phys_comp_rj(i_fld)                              &
     &         - istack_phys_comp_rj(i_fld-1)
        icou = istack_phys_comp_rj(i_fld-1)
        jcou = istack_comp_monitor(j_fld-1)
        if(ncomp .eq. 3) then
            rms_out(jcou+1)                                             &
     &             = (half * d_rj(inod,icou+1) * a_r_1d_rj_r(k))**2
            rms_out(jcou+2) = zero
            rms_out(jcou+3) = rms_out(jcou+1)
        else
          do nd = 1, ncomp
            rms_out(jcou+nd)                                            &
     &           = (d_rj(inod,icou+nd) * radius_1d_rj_r(k))**2
          end do
        end if
      end do
!
      end subroutine cal_rj_mean_sq_degree0_monitor
!
! -----------------------------------------------------------------------
!
      subroutine cal_rj_mean_sq_center_monitor                          &
     &         (inod_rj_center, n_point, num_phys_rj,                   &
     &          ntot_phys_rj, istack_phys_comp_rj, d_rj,                &
     &          nfld_monitor, istack_comp_monitor, ifld_monitor,        &
     &          ntot_comp_monitor, rms_out)
!
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: n_point
      integer(kind = kint), intent(in) :: num_phys_rj, ntot_phys_rj
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_phys_comp_rj(0:num_phys_rj)
      real(kind=kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint), intent(in) :: nfld_monitor
      integer(kind = kint), intent(in) :: ntot_comp_monitor
      integer(kind = kint), intent(in) :: ifld_monitor(nfld_monitor)
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_comp_monitor(0:nfld_monitor)
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_monitor)
!
      integer(kind = kint) :: nd, i_fld, j_fld, icou, jcou, ncomp
!
!
      do j_fld = 1, nfld_monitor
        i_fld = ifld_monitor(j_fld)
        ncomp = istack_phys_comp_rj(i_fld)                              &
     &         - istack_phys_comp_rj(i_fld-1)
        icou = istack_phys_comp_rj(i_fld-1)
        jcou = istack_comp_monitor(j_fld-1)
        if(ncomp .eq. 3) then
            rms_out(jcou+1) = (half * d_rj(inod_rj_center,icou+1))**2
            rms_out(jcou+2) = zero
            rms_out(jcou+3) = rms_out(jcou+1)
        else
          do nd = 1, ncomp
            rms_out(jcou+nd) = d_rj(inod_rj_center,icou+nd)**2
          end do
        end if
      end do
!
      end subroutine cal_rj_mean_sq_center_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine convert_to_energy_sph__monitor                         &
     &         (ipol, picked, ntot_comp_monitor, rms_out)
!
      type(phys_address), intent(in) :: ipol
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_monitor
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_monitor)
!
      integer(kind = kint) :: i_fld, j_fld, jcou
!
!
      do j_fld = 1, picked%num_field_rj
        i_fld = picked%ifield_monitor_rj(j_fld)
        jcou = picked%istack_comp_rj(j_fld-1)
        if (   i_fld .eq. ipol%i_velo                                &
     &      .or. i_fld .eq. ipol%i_magne                             &
     &      .or. i_fld .eq. ipol%i_filter_velo                       &
     &      .or. i_fld .eq. ipol%i_filter_magne                      &
     &      .or. i_fld .eq. ipol%i_wide_fil_velo                     &
     &      .or. i_fld .eq. ipol%i_wide_fil_magne) then
          rms_out(jcou+1) = half * rms_out(jcou+1)
          rms_out(jcou+2) = half * rms_out(jcou+1)
          rms_out(jcou+3) = half * rms_out(jcou+1)
        end if
      end do
!
      end subroutine convert_to_energy_sph__monitor
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_mean_square_data
