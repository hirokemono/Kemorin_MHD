!
!     module SPH_analyzer_zm_snap
!
!      Written by H. Matsui
!
!      subroutine SPH_init_sph_zm_snap
!      subroutine SPH_analyze_snap(i_step)
!      subroutine SPH_finalize_zm_snap
!
      module SPH_analyzer_zm_snap
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_init_sph_zm_snap
!
      use SPH_analyzer_snap
!
!
      if (iflag_debug.eq.1) write(*,*) 'SPH_init_sph_snap'
      call SPH_init_sph_snap
!
      end subroutine SPH_init_sph_zm_snap
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_zm_snap(i_step)
!
      use m_work_time
      use m_t_step_parameter
      use m_node_id_spherical_IO
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use set_reference_sph_mhd
      use lead_fields_4_sph_mhd
      use lead_pole_data_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
      use sph_transforms_4_MHD
      use cal_energy_flux_rtp
      use lead_pole_data_4_sph_mhd
      use cal_nonlinear_sph_MHD
!
      use cal_zonal_mean_sph_spectr
!
      integer(kind = kint), intent(in) :: i_step
!
!
      call read_alloc_sph_rst_4_snap(i_step)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_eleps_time(12)
      call nonlinear
      call end_eleps_time(12)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(4)
      call start_eleps_time(7)
!
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph
!*
      if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
      call s_lead_fields_4_sph_mhd
      call end_eleps_time(7)
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'take_zonal_mean_sph_spectr'
      call take_zonal_mean_sph_spectr
!
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(10)
      if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
      call output_rms_sph_mhd_control
      call end_eleps_time(10)
      call end_eleps_time(4)
!
! ----  spherical transform
!
      if (iflag_debug.eq.1) write(*,*) 'sph_back_trans_4_MHD'
      call sph_back_trans_4_MHD
      if (iflag_debug.eq.1) write(*,*) 's_cal_nonlinear_sph_MHD'
      call s_cal_nonlinear_sph_MHD
!
!
      if (iflag_debug.eq.1) write(*,*) 'sph_back_trans_snapshot_MHD'
      call sph_back_trans_snapshot_MHD
!
      if (iflag_debug.eq.1) write(*,*) 's_cal_energy_flux_rtp'
      call s_cal_energy_flux_rtp

      if (iflag_debug.eq.1) write(*,*) 'lead_pole_fields_4_sph_mhd'
      call lead_pole_fields_4_sph_mhd
!
      end subroutine SPH_analyze_zm_snap
!
! ----------------------------------------------------------------------
!
      subroutine SPH_finalize_zm_snap
!
      use SPH_analyzer_snap
!
!
      call SPH_finalize_snap
!
      end subroutine SPH_finalize_zm_snap
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_zm_snap
