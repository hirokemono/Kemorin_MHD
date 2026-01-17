!>@file   cal_write_sph_scales.f90
!!@brief  module cal_write_sph_scales
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!!      subroutine cal_write_no_heat_sourse_Nu                          &
!!     &         (is_scalar, is_source, is_grad_s, time_d, sph, sc_prop,&
!!     &          sph_bc_S, sph_bc_U, bcs_S, fdm2_center, r_2nd,        &
!!     &          band_s00_poisson_fixS, rj_fld, Nusselt)
!!        integer(kind = kint), intent(in) :: is_scalar, is_source
!!        integer(kind = kint), intent(in) :: is_grad_s
!!        type(sph_grids), intent(in) :: sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(scalar_property), intent(in) :: sc_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_S, sph_bc_U
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_S
!!        type(phys_data), intent(in) :: rj_fld
!!        type(band_matrix_type), intent(in) :: band_s00_poisson_fixS
!!        type(nusselt_number_data), intent(inout) :: Nusselt
!!      subroutine cal_write_dipolarity(time_d, sph_params, sph_rj,     &
!!     &          ipol, rj_fld, pwr, dip)
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(dipolarity_data), intent(inout) :: dip
!!      subroutine pick_write_CMB_avetage(time_d, sph_params, sph_rj,   &
!!     &                                  ipol, rj_fld, ave_CMB)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(CMB_average_data), intent(inout) :: ave_CMB
!!      subroutine cal_write_typical_scale(time_d, sph_params, sph_rj,  &
!!     &                                   sph_bc_U, pwr, tsl)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(typical_scale_data), intent(inout) :: tsl
!!@endverbatim
!
      module cal_write_sph_scales
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_SPH_mesh_field_data
      use t_sph_mhd_monitor_data_IO
      use t_scalar_property
      use t_phys_data
      use t_boundary_data_sph_MHD
      use t_boundary_sph_spectr
      use t_work_4_sph_trans
      use t_time_data
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
      use t_pickup_sph_spectr_data
      use t_no_heat_Nusselt
      use t_CMB_dipolarity
      use t_sph_typical_scales
      use t_energy_label_parameters
      use t_fdm_coefs
      use t_physical_property
      use t_radial_matrices_sph_MHD
      use t_sph_matrix
!
      implicit none
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine cal_write_no_heat_sourse_Nu                            &
     &         (is_scalar, is_source, is_grad_s, time_d, sph, sc_prop,  &
     &          sph_bc_S, sph_bc_U, bcs_S, fdm2_center, r_2nd,          &
     &          band_s00_poisson_fixS, rj_fld, Nusselt)
!
      use pickup_gauss_coefficients
      use cal_heat_source_Nu
!
      integer(kind = kint), intent(in) :: is_scalar, is_source
      integer(kind = kint), intent(in) :: is_grad_s
!
      type(time_data), intent(in) :: time_d
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(scalar_property), intent(in) :: sc_prop
      type(sph_boundary_type), intent(in) :: sph_bc_S, sph_bc_U
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
      type(phys_data), intent(in) :: rj_fld
      type(band_matrix_type), intent(in) :: band_s00_poisson_fixS
!
      type(nusselt_number_data), intent(inout) :: Nusselt
!
!
      if(Nusselt%iflag_Nusselt .eq. 0) return
      call sel_Nusselt_routine(is_scalar, is_source, is_grad_s,         &
     &    sph, r_2nd, sc_prop, sph_bc_S, sph_bc_U, bcs_S,               &
     &    fdm2_center, band_s00_poisson_fixS, rj_fld, Nusselt)
      call write_Nusselt_file(time_d%i_time_step, time_d%time,          &
     &    sph%sph_params%l_truncation, sph%sph_rj%nidx_rj(1),           &
     &    sph%sph_params%nlayer_ICB, sph%sph_params%nlayer_CMB,         &
     &    sph%sph_rj%idx_rj_degree_zero, Nusselt)
!
      end subroutine cal_write_no_heat_sourse_Nu
!
!  --------------------------------------------------------------------
!
      subroutine cal_write_dipolarity(time_d, sph_params, sph_rj,       &
     &          ipol, rj_fld, pwr, dip)
!
      use cal_CMB_dipolarity
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(dipolarity_data), intent(inout) :: dip
!
!
      call s_cal_CMB_dipolarity(my_rank, rj_fld, pwr, dip)
!
      if(my_rank .eq. pwr%irank_l) then
        call write_dipolarity(time_d%i_time_step, time_d%time,          &
     &      sph_params%l_truncation, sph_rj%nidx_rj(1),                 &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      ipol%base%i_magne, dip)
      end if
!
      end subroutine cal_write_dipolarity
!
!  --------------------------------------------------------------------
!
      subroutine pick_write_CMB_avetage(time_d, sph_params, sph_rj,     &
     &                                  ipol, rj_fld, ave_CMB)
!
      use cal_CMB_dipolarity
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(CMB_average_data), intent(inout) :: ave_CMB
!
!
      call s_pick_CMB_average(sph_rj, ipol, rj_fld, ave_CMB)
      call write_CMB_average(time_d%i_time_step, time_d%time,           &
     &                       sph_params, sph_rj, ave_CMB)
!
      end subroutine pick_write_CMB_avetage
!
!  --------------------------------------------------------------------
!
      subroutine cal_write_typical_scale(time_d, sph_params, sph_rj,    &
     &                                   sph_bc_U, pwr, tsl)
!
      use cal_typical_scale
      use write_typical_scale
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_mean_squares), intent(in) :: pwr
!
      type(typical_scale_data), intent(inout) :: tsl
!
!
      call cal_typical_scales(pwr, tsl)
      call write_typical_scales(time_d%i_time_step, time_d%time,        &
     &    sph_params, sph_rj, sph_bc_U, pwr, tsl)
!
      end subroutine cal_write_typical_scale
!
!  --------------------------------------------------------------------
!
      end module cal_write_sph_scales
