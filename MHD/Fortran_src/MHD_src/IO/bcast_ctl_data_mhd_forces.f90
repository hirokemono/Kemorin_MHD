!>@file   bcast_ctl_data_mhd_forces.f90
!!@brief  module bcast_ctl_data_mhd_forces
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!!@date Modified in July, 2013
!
!>@brief  Distribute control data for normalization
!!
!!@verbatim
!!      subroutine bcast_coef_term_ctl(eqs_ctl)
!!        type(equations_control), intent(in) :: eqs_ctl
!!      subroutine bcast_dimless_ctl(dless_ctl)
!!        type(dimless_control), intent(inout) :: dless_ctl
!!      subroutine bcast_forces_ctl(frc_ctl)
!!        type(forces_control), intent(inout) :: frc_ctl
!!      subroutine bcast_gravity_ctl(g_ctl)
!!        type(forces_control), intent(inout) :: g_ctl
!!      subroutine bcast_coriolis_ctl(cor_ctl)
!!        type(coriolis_control), intent(inout) :: cor_ctl
!!      subroutine bcast_magneto_ctl(mcv_ctl)
!!        type(magneto_convection_control), intent(inout) :: mcv_ctl
!!      subroutine bcast_magnetic_scale_ctl(bscale_ctl)
!!        type(magnetic_field_scale_control), intent(inout) :: bscale_ctl
!!      subroutine reset_ref_scalar_ctl(refs_ctl)
!!        type(reference_temperature_ctl), intent(inout) :: refs_ctl
!!
!!      subroutine bcast_coef_term_ctl(eqs_ctl)
!!        type(equations_control), intent(inout) :: eqs_ctl
!!      subroutine bcast_thermal_ctl(heat_ctl)
!!        type(heat_equation_control), intent(inout) :: heat_ctl
!!      subroutine bcast_momentum_ctl(mom_ctl)
!!        type(momentum_equation_control), intent(inout) :: mom_ctl
!!      subroutine bcast_induction_ctl(induct_ctl)
!!        type(induction_equation_control), intent(inout) :: induct_ctl
!!      subroutine bcast_ref_value_ctl(ref_ctl)
!!        type(reference_point_control), intent(inout) :: ref_ctl
!!@endverbatim
!
      module bcast_ctl_data_mhd_forces
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
      private :: bcast_thermal_ctl, bcast_momentum_ctl
      private :: bcast_induction_ctl, bcast_ref_value_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine bcast_coef_term_ctl(eqs_ctl)
!
      use t_ctl_data_mhd_normalize
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(equations_control), intent(inout) :: eqs_ctl
!
!
      call bcast_thermal_ctl(eqs_ctl%heat_ctl)
      call bcast_momentum_ctl(eqs_ctl%mom_ctl)
      call bcast_induction_ctl(eqs_ctl%induct_ctl)
      call bcast_thermal_ctl(eqs_ctl%comp_ctl)
!
      call calypso_mpi_bcast_one_int(eqs_ctl%i_coef_term_ctl, 0)
!
      end subroutine bcast_coef_term_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_dimless_ctl(dless_ctl)
!
      use t_ctl_data_dimless_numbers
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(dimless_control), intent(inout) :: dless_ctl
!
!
      call bcast_ctl_array_cr(dless_ctl%dimless)
      call calypso_mpi_bcast_one_int(dless_ctl%i_dimless_ctl, 0)
!
      end subroutine bcast_dimless_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_forces_ctl(frc_ctl)
!
      use t_ctl_data_mhd_forces
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(forces_control), intent(inout) :: frc_ctl
!
!
      call bcast_ctl_array_c1(frc_ctl%force_names)
!
      call calypso_mpi_bcast_one_int(frc_ctl%i_forces_ctl, 0)
!
      end subroutine bcast_forces_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_gravity_ctl(g_ctl)
!
      use t_ctl_data_gravity
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(gravity_control), intent(inout) :: g_ctl
!
!
      call bcast_ctl_array_cr(g_ctl%gravity_vector)
      call bcast_ctl_type_c1(g_ctl%gravity)
!
      call calypso_mpi_bcast_one_int(g_ctl%i_gravity_ctl, 0)
!
      end subroutine bcast_gravity_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_coriolis_ctl(cor_ctl)
!
      use t_ctl_data_coriolis_force
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(coriolis_control), intent(inout) :: cor_ctl
!
!
      call bcast_ctl_array_cr(cor_ctl%system_rotation)
!
      call calypso_mpi_bcast_one_int(cor_ctl%i_coriolis_ctl, 0)
!
      end subroutine bcast_coriolis_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_magneto_ctl(mcv_ctl)
!
      use t_ctl_data_mhd_magne
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(magneto_convection_control), intent(inout) :: mcv_ctl
!
!
      call bcast_ctl_array_cr(mcv_ctl%ext_magne)
      call bcast_ctl_type_c1(mcv_ctl%magneto_cv)
      call bcast_ctl_type_c1(mcv_ctl%filterd_induction_ctl)
!
      call calypso_mpi_bcast_one_int(mcv_ctl%i_magneto_ctl, 0)
!
      end subroutine bcast_magneto_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_magnetic_scale_ctl(bscale_ctl)
!
      use t_ctl_data_magnetic_scale
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(magnetic_field_scale_control), intent(inout) :: bscale_ctl
!
!
      call bcast_ctl_array_cr(bscale_ctl%mag_to_kin_energy_ctl)
      call calypso_mpi_bcast_one_int(bscale_ctl%i_bscale_ctl, 0)
!
      end subroutine bcast_magnetic_scale_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_ref_scalar_ctl(refs_ctl)
!
      use t_ctl_data_temp_model
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(reference_temperature_ctl), intent(inout) :: refs_ctl
!
!
      call bcast_ref_value_ctl(refs_ctl%low_ctl)
      call bcast_ref_value_ctl(refs_ctl%high_ctl)
      call bcast_takepiro_ctl(refs_ctl%takepiro_ctl)
!
      call bcast_ctl_type_c1(refs_ctl%filterd_advect_ctl)
      call bcast_ctl_type_c1(refs_ctl%reference_ctl)
      call bcast_ctl_type_c1(refs_ctl%stratified_ctl)
!
      call bcast_ctl_type_r1(refs_ctl%ICB_diffuse_reduction_ctl)
!
      call calypso_mpi_bcast_one_int(refs_ctl%i_temp_def, 0)
!
      end subroutine bcast_ref_scalar_ctl
!
!   --------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_thermal_ctl(heat_ctl)
!
      use t_ctl_data_termal_norm
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(heat_equation_control), intent(inout) :: heat_ctl
!
!
      call bcast_ctl_array_cr(heat_ctl%coef_4_adv_flux)
      call bcast_ctl_array_cr(heat_ctl%coef_4_diffuse)
      call bcast_ctl_array_cr(heat_ctl%coef_4_source)
!
      call calypso_mpi_bcast_one_int(heat_ctl%i_diff_adv, 0)
!
      end subroutine bcast_thermal_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_momentum_ctl(mom_ctl)
!
      use t_ctl_data_momentum_norm
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(momentum_equation_control), intent(inout) :: mom_ctl
!
!
      call bcast_ctl_array_cr(mom_ctl%coef_4_intertia)
      call bcast_ctl_array_cr(mom_ctl%coef_4_grad_p)
      call bcast_ctl_array_cr(mom_ctl%coef_4_viscous)
!
      call bcast_ctl_array_cr(mom_ctl%coef_4_termal_buo)
      call bcast_ctl_array_cr(mom_ctl%coef_4_comp_buo)
      call bcast_ctl_array_cr(mom_ctl%coef_4_Coriolis)
      call bcast_ctl_array_cr(mom_ctl%coef_4_Lorentz)
!
      call calypso_mpi_bcast_one_int(mom_ctl%i_momentum, 0)
!
      end subroutine bcast_momentum_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_induction_ctl(induct_ctl)
!
      use t_ctl_data_induct_norm
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(induction_equation_control), intent(inout) :: induct_ctl
!
!
      call bcast_ctl_array_cr(induct_ctl%coef_4_magne_evo)
      call bcast_ctl_array_cr(induct_ctl%coef_4_mag_potential)
      call bcast_ctl_array_cr(induct_ctl%coef_4_mag_diffuse)
      call bcast_ctl_array_cr(induct_ctl%coef_4_induction)
!
      call calypso_mpi_bcast_one_int(induct_ctl%i_induct_ctl, 0)
!
      end subroutine bcast_induction_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ref_value_ctl(ref_ctl)
!
      use t_ctl_data_temp_model
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(reference_point_control), intent(inout) :: ref_ctl
!
!
      call bcast_ctl_type_r1(ref_ctl%depth)
      call bcast_ctl_type_r1(ref_ctl%value)
!
      call calypso_mpi_bcast_one_int(ref_ctl%i_referenced, 0)
!
      end subroutine bcast_ref_value_ctl
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_data_mhd_forces
