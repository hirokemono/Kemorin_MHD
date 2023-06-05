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
!!      subroutine convert_to_energy_sph_monitor                        &
!!     &         (ipol, ipol_LES, picked, ntot_comp_monitor, rms_out)
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
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
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
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
      integer(kind = kint) :: j, k_in, k_out, i_in, i_out
!
!
      j = picked%idx_out(inum,4)
      k_in =  picked%id_radius(knum,1)
      k_out = picked%id_radius(knum,2)
      i_in =  1 + (k_in-1) * sph_rj%istep_rj(1)                         &
     &          + (j-1) *    sph_rj%istep_rj(2)
      i_out =  1 + (k_out-1) * sph_rj%istep_rj(1)                       &
     &           + (j-1) *      sph_rj%istep_rj(2)
      do j_fld = 1, picked%num_field_rj
        i_fld = picked%ifield_monitor_rj(j_fld)
        ncomp = rj_fld%istack_component(i_fld)                          &
     &         - rj_fld%istack_component(i_fld-1)
        icou = rj_fld%istack_component(i_fld-1)
        jcou = picked%istack_comp_rj(j_fld-1)
        if(ncomp .eq. 3) then
          call sph_vector_mean_square(picked%radius_gl(knum,2),         &
     &        leg%g_sph_rj(j,3), leg%g_sph_rj(j,12),                    &
     &        rj_fld%d_fld(i_in,icou+1),  rj_fld%d_fld(i_in,icou+2),    &
     &        rj_fld%d_fld(i_in,icou+3),  rj_fld%d_fld(i_out,icou+1),   &
     &        rj_fld%d_fld(i_out,icou+2), rj_fld%d_fld(i_in,i_out+3),   &
     &        picked%coef_radius_gl(knum), rms_out(jcou+1))
        else
          do nd = 1, ncomp
            rms_out(jcou+nd) = sph_scalar_mean_square                   &
     &                  (picked%radius_gl(knum,1), leg%g_sph_rj(j,11),  &
     &                   rj_fld%d_fld(i_in,icou+nd),                    &
     &                   rj_fld%d_fld(i_out,icou+nd),                   &
     &                   picked%coef_radius_gl(knum))
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
      integer(kind = kint) :: j, k_in, k_out, i_in, i_out
!
!
      j = sph_rj%idx_rj_degree_zero
      k_in =  picked%id_radius(knum,1)
      k_out = picked%id_radius(knum,2)
      i_in =  1 + (k_in-1) * sph_rj%istep_rj(1)                         &
     &          + (j-1) *    sph_rj%istep_rj(2)
      i_out =  1 + (k_out-1) * sph_rj%istep_rj(1)                       &
     &           + (j-1) *      sph_rj%istep_rj(2)
      do j_fld = 1, picked%num_field_rj
        i_fld = picked%ifield_monitor_rj(j_fld)
        ncomp = rj_fld%istack_component(i_fld)                          &
     &         - rj_fld%istack_component(i_fld-1)
        icou = rj_fld%istack_component(i_fld-1)
        jcou = picked%istack_comp_rj(j_fld-1)
        if(ncomp .eq. 3) then
          call degree0_vector_mean_square(picked%radius_gl(knum,2),     &
     &        rj_fld%d_fld(i_in,icou+1), rj_fld%d_fld(i_out,icou+1),    &
     &        picked%coef_radius_gl(knum), rms_out(jcou+1))
        else
          do nd = 1, ncomp
            rms_out(jcou+nd) = sph_scalar_mean_square                   &
     &                       (picked%radius_gl(knum,1), one,            &
     &                        rj_fld%d_fld(i_in,icou+nd),               &
     &                        rj_fld%d_fld(i_out,icou+nd),              &
     &                        picked%coef_radius_gl(knum))
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
      subroutine convert_to_energy_sph_monitor                          &
     &         (ipol, ipol_LES, picked, ntot_comp_monitor, rms_out)
!
      use cal_rms_by_sph_spectr
!
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
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
        jcou = picked%istack_comp_rj(j_fld-1) + 1
        call cvt_mag_or_kin_ene_one_point                               &
     &     (ipol%base, i_fld, rms_out(jcou))
!
        call cvt_mag_or_kin_ene_one_point                               &
     &     (ipol_LES%filter_fld, i_fld, rms_out(jcou))
        call cvt_mag_or_kin_ene_one_point                               &
     &     (ipol_LES%wide_filter_fld, i_fld, rms_out(jcou))
        call cvt_mag_or_kin_ene_one_point                               &
     &     (ipol_LES%dbl_filter_fld, i_fld, rms_out(jcou))
      end do
!
      end subroutine convert_to_energy_sph_monitor
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_mean_square_data
