!>@file   SPH_analyzer_correle_all.f90
!!@brief  module SPH_analyzer_correle_all
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_analyze_correlate_all(i_step, MHD_step)
!!        type(MHD_IO_step_param), intent(inout) :: MHD_step
!!@endverbatim
!
      module SPH_analyzer_correle_all
!
      use m_precision
      use calypso_mpi
      use t_phys_address
      use t_file_IO_parameter
      use t_phys_data
      use t_MHD_step_parameter
      use SPH_analyzer_back_trans
!
      implicit none
!
      type(field_IO_params), save :: sph_file_param2
      type(phys_data), save :: ref_rj_fld
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_correlate_all(i_step, MHD_step)
!
      use m_work_time
      use m_t_step_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_schmidt_poly_on_rtm
      use m_sph_trans_arrays_MHD
      use m_rms_4_sph_spectr
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
      use input_control_sph_MHD
      use cal_rms_fields_by_sph
!
      use sph_transforms_snapshot
      use zonal_correlation_rtp
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_IO_step_param), intent(inout) :: MHD_step
!
      integer(kind = kint) :: iflag, ncomp
      integer(kind = kint) :: k, nd
!
      real(kind = kreal), allocatable :: fld1_rtp(:,:)
!
      real(kind = kreal), allocatable :: msq_s1(:,:)
      real(kind = kreal), allocatable :: msq_s2(:,:)
      real(kind = kreal), allocatable :: cor_s(:,:)
      real(kind = kreal), allocatable :: msq_v1(:,:)
      real(kind = kreal), allocatable :: msq_v2(:,:)
      real(kind = kreal), allocatable :: cor_v(:,:)
!
!       read first data
!
      call read_alloc_sph_spectr                                        &
     &   (i_step, MHD1_org_files%rj_file_param, sph_file_param1,        &
     &    sph1%sph_rj, ipol, rj_fld1, MHD_step%ucd_step)
!
      call copy_field_name_type(rj_fld1, ref_rj_fld)
      call copy_field_data_type(rj_fld1, ref_rj_fld)
!
!       Transform second data
!
      call read_alloc_sph_spectr                                        &
     &   (i_step, MHD1_org_files%rj_file_param, sph_file_param2,        &
     &    sph1%sph_rj, ipol, rj_fld1, MHD_step%ucd_step)
!
!       Transform first data
!
      call start_eleps_time(9)
      if (iflag_debug.eq.1) write(*,*) 'sph_all_back_transform'
      call sph_all_back_transform(sph1, comms_sph1, trans_p1,           &
     &    ipol, ref_rj_fld, trns_WK1%trns_MHD)
      call end_eleps_time(9)
!
      ncomp = trns_WK1%trns_MHD%ncomp_rj_2_rtp
      allocate(fld1_rtp(sph1%sph_rtp%nnod_rtp,ncomp))
!
!$omp parallel workshare
      fld1_rtp(1:sph1%sph_rtp%nnod_rtp,1:ncomp)                         &
           = trns_WK1%trns_MHD%fld_rtp(1:sph1%sph_rtp%nnod_rtp,1:ncomp)
!$omp end parallel workshare
!
!       Transform second data
!
      call start_eleps_time(9)
      if (iflag_debug.eq.1) write(*,*) 'sph_all_back_transform'
      call sph_all_back_transform(sph1, comms_sph1, trans_p1,           &
     &    ipol, rj_fld1, trns_WK1%trns_MHD)
      call end_eleps_time(9)
!
!       Evaluate correlation in zonal
!
      call ovrwrt_zonal_correlate_rtp(trns_WK1%trns_MHD%ncomp_rj_2_rtp, &
     &    sph1%sph_rtp%nnod_rtp, sph1%sph_rtp%nidx_rtp,                 &
     &    fld1_rtp, trns_WK1%trns_MHD%fld_rtp)
      trns_WK1%trns_MHD%fld_pole = 0.0d0
!
      deallocate(fld1_rtp)
!
!       Evaluate correlation in sphere
!
      if(my_rank .eq. 0) then
        allocate(msq_s1(pwr1%nri_rms,pwr1%ntot_comp_sq))
        allocate(msq_s2(pwr1%nri_rms,pwr1%ntot_comp_sq))
        allocate(cor_s(pwr1%nri_rms,pwr1%ntot_comp_sq))
        allocate(msq_v1(pwr1%num_vol_spectr,pwr1%ntot_comp_sq))
        allocate(msq_v2(pwr1%num_vol_spectr,pwr1%ntot_comp_sq))
        allocate(cor_v(pwr1%num_vol_spectr,pwr1%ntot_comp_sq))
      end if
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_rms_sph_outer_core'
      call cal_mean_squre_in_shell                                      &
     &   (sph1%sph_params%l_truncation, sph1%sph_rj, ipol, ref_rj_fld,  &
     &    trans_p1%leg%g_sph_rj, pwr1, WK_pwr)
!
      if(my_rank .eq. 0) then
        do nd = 1, pwr1%ntot_comp_sq
          msq_s1(1:pwr1%nri_rms,nd) = pwr1%shl_sq(1:pwr1%nri_rms,nd)    &
     &                             - pwr1%shl_l(1:pwr1%nri_rms,0,nd)
        end do
        do nd = 1, pwr1%ntot_comp_sq
          do k = 1, pwr1%num_vol_spectr
            msq_v1(k,nd) = pwr1%v_spectr(k)%v_sq(nd)                    &
     &                   - pwr1%v_spectr(k)%v_l(0,nd)
          end do
        end do
      end if
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_rms_sph_outer_core'
      call cal_mean_squre_in_shell                                      &
     &   (sph1%sph_params%l_truncation, sph1%sph_rj, ipol, rj_fld1,     &
     &    trans_p1%leg%g_sph_rj, pwr1, WK_pwr)
!
      if(my_rank .eq. 0) then
        do nd = 1, pwr1%ntot_comp_sq
          msq_s2(1:pwr1%nri_rms,nd) = pwr1%shl_sq(1:pwr1%nri_rms,nd)    &
     &                           - pwr1%shl_l(1:pwr1%nri_rms,0,nd)
        end do
        do nd = 1, pwr1%ntot_comp_sq
          do k = 1, pwr1%num_vol_spectr
            msq_v2(k,nd) = pwr1%v_spectr(k)%v_sq(nd)                    &
     &                 - pwr1%v_spectr(k)%v_l(0,nd)
          end do
        end do
      end if
!
      call cal_correlate_in_shell(sph1%sph_params%l_truncation,         &
     &    sph1%sph_rj, rj_fld1, ref_rj_fld, trans_p1%leg%g_sph_rj,      &
     &    pwr1, WK_pwr)
!
      call dealloc_phys_data_type(ref_rj_fld)
      call dealloc_phys_name_type(ref_rj_fld)
!
!
      if(my_rank .eq. 0) then
        do nd = 1, pwr1%ntot_comp_sq
          cor_s(1:pwr1%nri_rms,nd) = pwr1%shl_sq(1:pwr1%nri_rms,nd)     &
     &                           - pwr1%shl_l(1:pwr1%nri_rms,0,nd)
        end do
        do nd = 1, pwr1%ntot_comp_sq
          do k = 1, pwr1%num_vol_spectr
            cor_v(k,nd) = pwr1%v_spectr(k)%v_sq(nd)                     &
     &                   - pwr1%v_spectr(k)%v_l(0,nd)
          end do
        end do
!
        do nd = 1, pwr1%ntot_comp_sq
           do k = 1, pwr1%nri_rms
             if(msq_s1(k,nd)*msq_s2(k,nd) .eq. zero) then
              pwr1%shl_sq(k,nd) = zero
            else
              pwr1%shl_sq(k,nd) = cor_s(k,nd)                           &
     &                        / sqrt(msq_s1(k,nd)*msq_s2(k,nd))
            end if
          end do
          do k = 1, pwr1%num_vol_spectr
            if(msq_v1(k,nd)*msq_v2(k,nd) .eq. zero) then
              pwr1%shl_sq(k,nd) = zero
            else
              pwr1%v_spectr(k)%v_sq(nd) = cor_v(k,nd)                   &
     &                       / sqrt(msq_v1(k,nd)*msq_v2(k,nd))
            end if
          end do
        end do
!
        deallocate(msq_s1, msq_s2, cor_s)
        deallocate(msq_v1, msq_v2, cor_v)
      end if
!
      call write_sph_vol_ms_file(my_rank, i_step, time_d1%time,         &
     &   sph1%sph_params, sph1%sph_rj, pwr1)
      call write_sph_layer_ms_file(my_rank, i_step, time_d1%time,       &
     &   sph1%sph_params, pwr1)
!
      end subroutine SPH_analyze_correlate_all
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_snap
!
!      end subroutine SPH_finalize_snap
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_correle_all
