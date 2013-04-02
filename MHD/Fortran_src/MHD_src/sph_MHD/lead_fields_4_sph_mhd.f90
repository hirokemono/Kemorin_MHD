!
!      module lead_fields_4_sph_mhd
!
!        programmed by H.Matsui
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine s_lead_fields_4_sph_mhd
!
      module lead_fields_4_sph_mhd
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_lead_fields_4_sph_mhd
!
      use m_machine_parameter
      use cal_sol_sph_fluid_crank
      use m_t_step_parameter
!
      use cal_sph_field_by_rotation
      use const_radial_forces_on_bc
      use cal_div_of_forces
!
      use sph_transforms_4_MHD
      use cal_energy_flux_rtp
      use output_viz_file_control
!
      integer (kind =kint) :: iflag
!
!
      call set_lead_physical_values_flag(iflag)
!
      if (iflag_debug.eq.1) write(*,*) 'iflag_field...', iflag
!
      if ( (iflag*mod(istep_max_dt,i_step_output_rst)) .eq.0 ) then
         if (iflag_debug.eq.1) write(*,*) 'cal_div_of_forces_sph_2'
         call cal_div_of_forces_sph_2
!
         call s_const_radial_forces_on_bc
         call s_cal_div_of_forces
!
         if (iflag_debug.eq.1) write(*,*) 'cal_sol_pressure_by_div_v'
        call cal_sol_pressure_by_div_v
      end if
!
!
      if(iflag .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'sph_back_trans_snapshot_MHD'
        call sph_back_trans_snapshot_MHD
!
        if (iflag_debug.eq.1) write(*,*) 's_cal_energy_flux_rtp'
        call s_cal_energy_flux_rtp
        if (iflag_debug.eq.1) write(*,*)                                &
     &                          'sph_forward_trans_snapshot_MHD'
        call sph_forward_trans_snapshot_MHD
      end if
!
      end subroutine s_lead_fields_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      end module lead_fields_4_sph_mhd
