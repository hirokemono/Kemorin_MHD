!>@file   sph_mhd_rms_IO.f90
!!@brief  module sph_mhd_rms_IO
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!!      subroutine open_sph_vol_rms_file_mhd                            &
!!     &         (sph_params, sph_rj, ipol, rj_fld, pwr, WK_pwr)
!!      subroutine output_rms_sph_mhd_control                           &
!!     &         (sph_params, sph_rj, leg, ipol, rj_fld, pwr, WK_pwr)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
!!@endverbatim
!
      module sph_mhd_rms_IO
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_gauss_coefs_monitor_data
!
      use t_spheric_parameter
      use t_schmidt_poly_on_rtm
      use t_phys_address
      use t_phys_data
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
      use t_pickup_sph_spectr_data
!
      use pickup_sph_spectr_data
      use pickup_gauss_coefficients
      use output_sph_m_square_file
!
      implicit none
!
!
!>        Structure for pickup list
      type(pickup_mode_list), save :: pick_list1
!>        Structure for pickup list
      type(picked_spectrum_data), save :: pick1
!
      character(len = kchara) :: pickup_sph_head =  'picked_ene_spec'
!
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_vol_rms_file_mhd                              &
     &         (sph_params, sph_rj, ipol, rj_fld, pwr, WK_pwr)
!
      use cal_rms_fields_by_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_rms_4_sph_spectr'
      call init_rms_4_sph_spectr                                        &
     &   (sph_params%l_truncation, sph_rj, rj_fld, pwr, WK_pwr)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_gauss_coefs_4_monitor'
      call init_gauss_coefs_4_monitor                                   &
     &   (sph_params%l_truncation, sph_rj, ipol)
      if ( iflag_debug.gt.0 ) write(*,*) 'init_sph_spec_4_monitor'
      call init_sph_spec_4_monitor                                      &
     &   (sph_params%l_truncation, sph_rj, rj_fld, pick_list1, pick1)
!
      end subroutine open_sph_vol_rms_file_mhd
!
!  --------------------------------------------------------------------
!
      subroutine output_rms_sph_mhd_control                             &
     &         (sph_params, sph_rj, leg, ipol, rj_fld, pwr, WK_pwr)
!
      use m_machine_parameter
      use m_t_step_parameter
      use m_boundary_params_sph_MHD
      use m_no_heat_Nusselt_num
!
      use set_exit_flag_4_visualizer
      use cal_rms_fields_by_sph
      use volume_average_4_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
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
     &   (sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    sph_params%l_truncation, sph_rj, ipol, rj_fld, leg%g_sph_rj,  &
     &    pwr, WK_pwr)
      if(iflag_debug.gt.0)  write(*,*) 'cal_gauss_coefficients'
      call cal_gauss_coefficients                                       &
     &   (sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    sph_rj%nidx_rj, sph_rj%radius_1d_rj_r, ipol,                  &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      if(iflag_debug.gt.0)  write(*,*) 'pickup_sph_spec_4_monitor'
      call pickup_sph_spec_4_monitor                                    &
     &   (sph_rj, rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,    &
     &    rj_fld%istack_component, rj_fld%d_fld, pick1)
      if(iflag_debug.gt.0)  write(*,*) 'cal_no_heat_source_Nu'
      call cal_no_heat_source_Nu(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    sph_bc_U%r_ICB(0), sph_bc_U%r_CMB(0),                         &
     &    sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj, ipol,              &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      if(iflag_debug.gt.0)  write(*,*) 'write_total_energy_to_screen'
      call write_total_energy_to_screen                                 &
     &   (my_rank, i_step_MHD, time, pwr)
!
      call write_sph_vol_ave_file                                       &
     &   (i_step_MHD, time, sph_params, sph_rj, pwr)
      call write_sph_vol_ms_file                                        &
     &   (my_rank, i_step_MHD, time, sph_params, pwr)
      call write_sph_vol_ms_spectr_file                                 &
     &   (my_rank, i_step_MHD, time, sph_params, pwr)
      call write_sph_layer_ms_file                                      &
     &   (my_rank, i_step_MHD, time, sph_params, pwr)
!
      call write_gauss_coefs_4_monitor(my_rank, istep_max_dt, time)
      call write_sph_spec_monitor                                       &
     &   (pickup_sph_head, my_rank, istep_max_dt, time, pick1)
!
      call write_no_heat_source_Nu(sph_rj%idx_rj_degree_zero,           &
     &    istep_max_dt, time)
!
      end subroutine output_rms_sph_mhd_control
!
!  --------------------------------------------------------------------
!
      end module sph_mhd_rms_IO
