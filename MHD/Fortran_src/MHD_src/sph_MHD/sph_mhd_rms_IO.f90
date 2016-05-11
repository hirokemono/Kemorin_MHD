!>@file   sph_mhd_rms_IO.f90
!!@brief  module sph_mhd_rms_IO
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!!      subroutine open_sph_vol_rms_file_mhd(rj_fld)
!!      subroutine output_rms_sph_mhd_control(rj_fld)
!!        type(phys_data), intent(in) :: rj_fld
!!@endverbatim
!
      module sph_mhd_rms_IO
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_spheric_parameter
      use m_gauss_coefs_monitor_data
      use m_pickup_sph_spectr_data
!
      use t_phys_data
!
      use pickup_sph_coefs
      use pickup_gauss_coefficients
      use output_sph_m_square_file
!
      implicit none
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_vol_rms_file_mhd(rj_fld)
!
      use m_rms_4_sph_spectr
      use cal_rms_fields_by_sph
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_rms_4_sph_spectr'
      call init_rms_4_sph_spectr                                        &
     &   (sph_param1%l_truncation, sph_rj1, rj_fld)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_gauss_coefs_4_monitor'
      call init_gauss_coefs_4_monitor(sph_param1%l_truncation, sph_rj1)
      if ( iflag_debug.gt.0 ) write(*,*) 'init_sph_spec_4_monitor'
      call init_sph_spec_4_monitor                                      &
     &   (sph_param1%l_truncation, sph_rj1, rj_fld)
!
      end subroutine open_sph_vol_rms_file_mhd
!
!  --------------------------------------------------------------------
!
      subroutine output_rms_sph_mhd_control(rj_fld)
!
      use m_machine_parameter
      use m_t_step_parameter
      use m_boundary_params_sph_MHD
      use set_exit_flag_4_visualizer
      use cal_rms_fields_by_sph
      use m_no_heat_Nusselt_num
      use volume_average_4_sph
!
      type(phys_data), intent(in) :: rj_fld
!
      integer (kind = kint) :: i_flag
!
!
      call set_output_flag(i_flag, istep_max_dt, i_step_check)
!
      if (i_flag .ne. 0) return
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_rms_sph_outer_core'
      call cal_mean_squre_in_shell                                      &
     &   (sph_param1%nlayer_ICB, sph_param1%nlayer_CMB,                 &
     &    sph_param1%l_truncation, sph_rj1, rj_fld)
      if(iflag_debug.gt.0)  write(*,*) 'cal_gauss_coefficients'
      call cal_gauss_coefficients                                       &
     &   (sph_param1%nlayer_ICB, sph_param1%nlayer_CMB,                 &
     &    sph_rj1%nidx_rj, sph_rj1%radius_1d_rj_r,                      &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      if(iflag_debug.gt.0)  write(*,*) 'pickup_sph_spec_4_monitor'
      call pickup_sph_spec_4_monitor                                    &
     &   (sph_rj1, rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,   &
     &    rj_fld%istack_component, rj_fld%d_fld)
      if(iflag_debug.gt.0)  write(*,*) 'cal_no_heat_source_Nu'
      call cal_no_heat_source_Nu(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    sph_bc_U%r_ICB(0), sph_bc_U%r_CMB(0),                         &
     &    sph_rj1%idx_rj_degree_zero, sph_rj1%nidx_rj,                  &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      if(iflag_debug.gt.0)  write(*,*) 'write_total_energy_to_screen'
      call write_total_energy_to_screen(my_rank, i_step_MHD, time)
!
      call write_sph_vol_ave_file                                       &
     &   (i_step_MHD, time, sph_param1%l_truncation,                    &
     &    sph_param1%nlayer_ICB, sph_param1%nlayer_CMB,                 &
     &    sph_rj1%idx_rj_degree_zero)
      call write_sph_vol_ms_file                                        &
     &   (my_rank, i_step_MHD, time, sph_param1%l_truncation,           &
     &    sph_param1%nlayer_ICB, sph_param1%nlayer_CMB)
      call write_sph_vol_ms_spectr_file                                 &
     &   (my_rank, i_step_MHD, time, sph_param1%l_truncation,           &
     &    sph_param1%nlayer_ICB, sph_param1%nlayer_CMB)
      call write_sph_layer_ms_file                                      &
     &   (my_rank, i_step_MHD, time, sph_param1%l_truncation,           &
     &    sph_param1%nlayer_ICB, sph_param1%nlayer_CMB)
!
      call write_gauss_coefs_4_monitor(my_rank, istep_max_dt, time)
      call write_sph_spec_4_monitor(my_rank, istep_max_dt, time)
!
      call write_no_heat_source_Nu(sph_rj1%idx_rj_degree_zero ,         &
     &    istep_max_dt, time)
!
      end subroutine output_rms_sph_mhd_control
!
!  --------------------------------------------------------------------
!
      end module sph_mhd_rms_IO
