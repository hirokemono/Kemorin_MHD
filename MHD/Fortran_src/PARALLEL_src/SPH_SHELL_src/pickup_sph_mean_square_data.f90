!>@file   pickup_sph_mean_square_data.f90
!!@brief  module pickup_sph_mean_square_data
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine cal_rj_mean_sq_spectr_monitor(inum, knum, sph_rj,    &
!!     &          leg, rj_fld, picked, ntot_comp_monitor, rms_out)
!!      subroutine cal_rj_mean_sq_degree0_monitor(knum, sph_rj, rj_fld, &
!!     &          picked, ntot_comp_monitor, rms_out)
!!      subroutine cal_rj_mean_sq_center_monitor                        &
!!     &         (sph_rj, rj_fld, picked, ntot_comp_monitor, rms_out)
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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_rj_mean_sq_spectr_monitor(inum, knum, sph_rj,      &
     &          leg, rj_fld, picked, ntot_comp_monitor, rms_out)
!
      use single_pt_sph_mean_square
!
      integer(kind = kint), intent(in) :: inum, knum
      type(sph_rj_grid), intent(in) :: sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_monitor
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_monitor)
!
      integer(kind = kint) :: nd, i_fld, j_fld, icou, jcou, ncomp
      integer(kind = kint) :: j, k, inod
!
!
      k = picked%id_radius(knum)
      j = picked%idx_out(inum,4)
      inod = j + (k-1) * sph_rj%nidx_rj(2)
      do j_fld = 1, picked%num_field_rj
        i_fld = picked%ifield_monitor_rj(j_fld)
        ncomp = rj_fld%istack_component(i_fld)                          &
     &         - rj_fld%istack_component(i_fld-1)
        icou = rj_fld%istack_component(i_fld-1)
        jcou = picked%istack_comp_rj(j_fld-1)
        if(ncomp .eq. 3) then
          call sph_vector_mean_square(sph_rj%a_r_1d_rj_r(k),            &
     &        leg%g_sph_rj(j,3), leg%g_sph_rj(j,12),                    &
     &        rj_fld%d_fld(inod,icou+1), rj_fld%d_fld(inod,icou+2),     &
     &        rj_fld%d_fld(inod,icou+3), rms_out(jcou+1))
        else
          do nd = 1, ncomp
            rms_out(jcou+nd)                                            &
     &               = sph_scalar_mean_square(sph_rj%radius_1d_rj_r(k), &
     &                leg%g_sph_rj(j,11), rj_fld%d_fld(inod,icou+nd))
          end do
        end if
      end do
!
      end subroutine cal_rj_mean_sq_spectr_monitor
!
! -----------------------------------------------------------------------
!
      subroutine cal_rj_mean_sq_degree0_monitor(knum, sph_rj, rj_fld,   &
     &          picked, ntot_comp_monitor, rms_out)
!
      use single_pt_sph_mean_square
!
      integer(kind = kint), intent(in) :: knum
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_monitor
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_monitor)
!
      integer(kind = kint) :: nd, i_fld, j_fld, icou, jcou, ncomp
      integer(kind = kint) :: j, k, inod
!
!
      k = picked%id_radius(knum)
      j = sph_rj%idx_rj_degree_zero
      inod = j + (k-1) * sph_rj%nidx_rj(2)
      do j_fld = 1, picked%num_field_rj
        i_fld = picked%ifield_monitor_rj(j_fld)
        ncomp = rj_fld%istack_component(i_fld)                          &
     &         - rj_fld%istack_component(i_fld-1)
        icou = rj_fld%istack_component(i_fld-1)
        jcou = picked%istack_comp_rj(j_fld-1)
        if(ncomp .eq. 3) then
          call degree0_vector_mean_square(sph_rj%a_r_1d_rj_r(k),        &
     &       rj_fld%d_fld(inod,icou+1), rms_out(jcou+1))
        else
          do nd = 1, ncomp
            rms_out(jcou+nd)                                            &
     &          = sph_scalar_mean_square(sph_rj%radius_1d_rj_r(k),      &
     &           one, rj_fld%d_fld(inod,icou+nd))
          end do
        end if
      end do
!
      end subroutine cal_rj_mean_sq_degree0_monitor
!
! -----------------------------------------------------------------------
!
      subroutine cal_rj_mean_sq_center_monitor                          &
     &         (sph_rj, rj_fld, picked, ntot_comp_monitor, rms_out)
!
      use single_pt_sph_mean_square
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_monitor
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_monitor)
!
      integer(kind = kint) :: nd, i_fld, j_fld, icou, jcou, ncomp, inod
!
!
      inod = sph_rj%inod_rj_center
      do j_fld = 1, picked%num_field_rj
        i_fld = picked%ifield_monitor_rj(j_fld)
        ncomp = rj_fld%istack_component(i_fld)                          &
     &         - rj_fld%istack_component(i_fld-1)
        icou = rj_fld%istack_component(i_fld-1)
        jcou = picked%istack_comp_rj(j_fld-1)
        if(ncomp .eq. 3) then
          call center_vector_mean_square                                &
     &       (rj_fld%d_fld(inod,icou+1), rms_out(jcou+1))
        else
          do nd = 1, ncomp
            rms_out(jcou+nd)                                            &
     &        = center_scalar_mean_square(rj_fld%d_fld(inod,icou+nd))
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
      use single_pt_sph_mean_square
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
          call one_point_mean_sq_to_energy(rms_out(jcou+1))
        end if
      end do
!
      end subroutine convert_to_energy_sph__monitor
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_mean_square_data
