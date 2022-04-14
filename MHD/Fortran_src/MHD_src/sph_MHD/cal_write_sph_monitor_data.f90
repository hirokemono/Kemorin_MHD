!>@file   cal_write_sph_monitor_data.f90
!!@brief  module cal_write_sph_monitor_data
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!!      subroutine cal_sph_monitor_data                                 &
!!     &         (sph_params, sph_rj, sph_bc_U, leg, ipol, rj_fld,      &
!!     &          pwr, WK_pwr, Nusselt, dip)
!!      subroutine output_sph_monitor_data                              &
!!     &         (ene_labels, time_d, sph_params, sph_rj, ipol, rj_fld, &
!!     &          pwr, pick_coef, gauss_coef, Nusselt, dip, tsl, SR_sig)
!!      subroutine output_sph_mean_square_files                         &
!!     &         (ene_labels, time_d, sph_params, sph_rj, pwr)
!!
!!      subroutine cal_write_no_heat_sourse_Nu(time_d, sph_rj,          &
!!     &          sph_bc_U, ipol, rj_fld, Nusselt)
!!      subroutine cal_write_dipolarity(time_d, sph_params, sph_rj, leg,&
!!     &          ipol, rj_fld, pwr, WK_pwr, dip)
!!      subroutine cal_write_typical_scale(time_d, rj_fld, pwr, tsl)
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
!!        type(picked_spectrum_data), intent(inout) :: pick_coef
!!        type(picked_spectrum_data), intent(inout) :: gauss_coef
!!        type(nusselt_number_data), intent(inout) :: Nusselt
!!        type(dipolarity_data), intent(inout) :: dip
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(typical_scale_data), intent(inout) :: tsl
!!@endverbatim
!
      module cal_write_sph_monitor_data
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_phys_data
      use t_boundary_data_sph_MHD
      use t_schmidt_poly_on_rtm
      use t_time_data
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
      use t_pickup_sph_spectr_data
      use t_no_heat_Nusselt
      use t_CMB_dipolarity
      use t_sph_typical_scales
      use t_energy_label_parameters
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine cal_sph_monitor_data                                   &
     &         (sph_params, sph_rj, sph_bc_U, leg, ipol, rj_fld,        &
     &          pwr, WK_pwr, Nusselt, dip, tsl)
!
      use cal_rms_fields_by_sph
      use pickup_sph_spectr_data
      use pickup_gauss_coefficients
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
      type(nusselt_number_data), intent(inout) :: Nusselt
      type(dipolarity_data), intent(inout) :: dip
      type(typical_scale_data), intent(inout) :: tsl
!
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_rms_sph_outer_core'
      call cal_mean_squre_in_shell                                      &
     &   (sph_params, sph_rj, ipol, rj_fld, leg%g_sph_rj, pwr, WK_pwr)
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_no_heat_source_Nu'
      call cal_no_heat_source_Nu(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    sph_bc_U%r_ICB(0), sph_bc_U%r_CMB(0),                         &
     &    sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj, ipol,              &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld, Nusselt)
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_CMB_dipolarity'
      call cal_CMB_dipolarity                                           &
     &   (sph_params%nlayer_CMB, sph_params, sph_rj, ipol,              &
     &    leg%g_sph_rj, rj_fld, pwr, WK_pwr, dip)
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_typical_scales'
      call cal_typical_scales(rj_fld, pwr, tsl)
!
      end subroutine cal_sph_monitor_data
!
!  --------------------------------------------------------------------
!
      subroutine output_sph_monitor_data                                &
     &         (ene_labels, time_d, sph_params, sph_rj, ipol, rj_fld,   &
     &          pwr, pick_coef, gauss_coef, Nusselt, dip, tsl, SR_sig)
!
      use t_solver_SR
      use output_sph_pwr_volume_file
      use write_picked_sph_spectr
      use write_sph_gauss_coefs
!
      type(energy_label_param), intent(in) :: ene_labels
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(dipolarity_data), intent(in) :: dip
      type(typical_scale_data), intent(in) :: tsl
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(picked_spectrum_data), intent(inout) :: pick_coef
      type(picked_spectrum_data), intent(inout) :: gauss_coef
      type(nusselt_number_data), intent(inout) :: Nusselt
      type(send_recv_status), intent(inout) :: SR_sig
!
!
      if(iflag_debug.gt.0)  write(*,*) 'write_total_energy_to_screen'
      call write_total_energy_to_screen(my_rank, time_d, pwr)
!
      call output_sph_mean_square_files                                 &
     &   (ene_labels, time_d, sph_params, sph_rj, pwr)
!
      call write_no_heat_source_Nu(sph_rj%idx_rj_degree_zero,           &
     &    time_d%i_time_step, time_d%time, Nusselt)
      call write_dipolarity(time_d%i_time_step, time_d%time,            &
     &    sph_params%radius_CMB, ipol, pwr, dip)
      call write_typical_scales(time_d%i_time_step, time_d%time,        &
     &                          pwr, tsl)
!
      call write_each_picked_specr_file                                 &
     &   (time_d, sph_rj, rj_fld, pick_coef)
      call append_sph_gauss_coefs_file                                  &
     &   (time_d, sph_params, sph_rj, ipol, rj_fld, gauss_coef, SR_sig)
!
      end subroutine output_sph_monitor_data
!
!  --------------------------------------------------------------------
!
      subroutine output_sph_mean_square_files                           &
     &         (ene_labels, time_d, sph_params, sph_rj, pwr)
!
      use output_sph_volume_ave_file
      use output_sph_pwr_volume_file
      use output_sph_pwr_layer_file
!
      type(energy_label_param), intent(in) :: ene_labels
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(sph_mean_squares), intent(inout) :: pwr
!
!
      call write_sph_vol_ave_file                                       &
     &   (ene_labels, time_d, sph_params, sph_rj, pwr)
      call write_sph_vol_ms_file                                        &
     &   (my_rank, ene_labels, time_d, sph_params, sph_rj, pwr)
      call write_sph_vol_ms_spectr_file                                 &
     &   (my_rank, ene_labels, time_d, sph_params, sph_rj, pwr)
      call write_sph_layer_ms_file                                      &
     &   (my_rank, ene_labels, time_d, sph_params, pwr)
      call write_sph_layer_spectr_file                                  &
     &   (my_rank, ene_labels, time_d, sph_params, pwr)
!
      end subroutine output_sph_mean_square_files
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine cal_write_no_heat_sourse_Nu(time_d, sph_rj,            &
     &          sph_bc_U, ipol, rj_fld, Nusselt)
!
      use pickup_gauss_coefficients
!
      type(time_data), intent(in) :: time_d
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(nusselt_number_data), intent(inout) :: Nusselt
!
!
      call cal_no_heat_source_Nu(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    sph_bc_U%r_ICB(0), sph_bc_U%r_CMB(0),                         &
     &    sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj, ipol,              &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld,               &
     &    Nusselt)
      call write_no_heat_source_Nu(sph_rj%idx_rj_degree_zero,           &
     &    time_d%i_time_step, time_d%time, Nusselt)
!
      end subroutine cal_write_no_heat_sourse_Nu
!
!  --------------------------------------------------------------------
!
      subroutine cal_write_dipolarity(time_d, sph_params, sph_rj, leg,  &
     &          ipol, rj_fld, pwr, WK_pwr, dip)
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
      type(dipolarity_data), intent(inout) :: dip
!
!
      call cal_CMB_dipolarity                                           &
     &   (sph_params%nlayer_CMB, sph_params, sph_rj, ipol,              &
     &    leg%g_sph_rj, rj_fld, pwr, WK_pwr, dip)
      call write_dipolarity(time_d%i_time_step, time_d%time,            &
     &    sph_params%radius_CMB, ipol, pwr, dip)
!
      end subroutine cal_write_dipolarity
!
!  --------------------------------------------------------------------
!
      subroutine cal_write_typical_scale(time_d, rj_fld, pwr, tsl)
!
      type(time_data), intent(in) :: time_d
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      type(typical_scale_data), intent(inout) :: tsl
!
!
      call cal_typical_scales(rj_fld, pwr, tsl)
      call write_typical_scales(time_d%i_time_step, time_d%time,        &
     &                          pwr, tsl)
!
      end subroutine cal_write_typical_scale
!
!  --------------------------------------------------------------------
!
      end module cal_write_sph_monitor_data
