!>@file   diff_r_const_buoyancy_sph.f90
!!@brief  module diff_r_const_buoyancy_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate divergence of buoyancy for pressure evaluation
!!
!!@verbatim
!!      subroutine cal_rot_r_const_buo_sph_SGS                          &
!!     &         (sph_rj, ipol, MHD_prop,  sph_bc_U, rj_fld)
!!      subroutine cal_div_r_const_buo_sph_SGS                          &
!!     &         (sph_rj, ipol, MHD_prop,  sph_bc_U, rj_fld)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic velocity
!!                 boundary condition parameters
!
      module diff_r_const_buoyancy_sph
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_rj_data
      use t_phys_data
!
      implicit  none
!
      private :: sel_rot_r_const_fil_buo_sph
      private :: sel_div_r_const_fil_buo_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_rot_r_const_buo_sph_SGS                            &
     &         (sph_rj, ipol, MHD_prop,  sph_bc_U, rj_fld)
!
      use t_control_parameter
      use t_phys_address
      use t_boundary_params_sph_MHD
      use rot_r_const_buoyancies_sph
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_rot_r_const_buo_sph_mhd                                  &
     &   (sph_rj, ipol%base, ipol%rot_forces, MHD_prop%fl_prop,         &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C, sph_bc_U, rj_fld)
!
      call sel_rot_r_const_fil_buo_sph                                  &
     &   (sph_rj, ipol%filter_fld, ipol%rot_frc_by_filter,              &
     &    MHD_prop%fl_prop, sph_bc_U, rj_fld)
!
      end subroutine cal_rot_r_const_buo_sph_SGS
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_r_const_buo_sph_SGS                            &
     &         (sph_rj, ipol, MHD_prop,  sph_bc_U, rj_fld)
!
      use t_control_parameter
      use t_phys_address
      use t_boundary_params_sph_MHD
      use div_r_const_buoyancies_sph
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_div_r_const_buo_sph_mhd                                  &
     &   (sph_rj, ipol%base, ipol%grad_fld, ipol%div_forces,            &
     &    MHD_prop%fl_prop, MHD_prop%ref_param_T, MHD_prop%ref_param_C, &
     &    sph_bc_U, rj_fld)
!
      call sel_div_r_const_fil_buo_sph(sph_rj,                          &
     &    ipol%filter_fld, ipol%grad_fil_fld, ipol%div_frc_by_filter,   &
     &    MHD_prop%fl_prop, sph_bc_U, rj_fld)
!
      end subroutine cal_div_r_const_buo_sph_SGS
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_rot_r_const_fil_buo_sph(sph_rj,                    &
     &          ipol_fil, ipol_rot_fil_frc, fl_prop, sph_bc_U, rj_fld)
!
      use t_physical_property
      use t_reference_scalar_param
      use t_base_field_labels
      use t_base_force_labels
      use t_boundary_params_sph_MHD
      use rot_r_const_buoyancies_sph
!
      type(fluid_property), intent(in) :: fl_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_fil
      type(base_force_address), intent(in) :: ipol_rot_fil_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (fl_prop%iflag_4_filter_gravity .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_rot_cst_buo_sph'
        call cal_rot_cst_buo_sph(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &      fl_prop%coef_buo, ipol_fil%i_temp,                          &
     &      ipol_rot_fil_frc%i_buoyancy, sph_rj%nidx_rj,                &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if (fl_prop%iflag_4_filter_comp_buo .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_rot_cst_buo_sph'
        call cal_rot_cst_buo_sph(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &      fl_prop%coef_comp_buo, ipol_fil%i_light,                    &
     &      ipol_rot_fil_frc%i_comp_buo, sph_rj%nidx_rj,                &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_rot_r_const_fil_buo_sph
!
!-----------------------------------------------------------------------
!
      subroutine sel_div_r_const_fil_buo_sph                            &
     &         (sph_rj, ipol_fil, ipol_gfl, ipol_div_fil_frc,           &
     &          fl_prop, sph_bc_U, rj_fld)
!
      use t_physical_property
      use t_reference_scalar_param
      use t_base_field_labels
      use t_base_force_labels
      use t_grad_field_labels
      use t_boundary_params_sph_MHD
      use div_r_const_buoyancies_sph
!
      type(fluid_property), intent(in) :: fl_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_fil
      type(gradient_field_address), intent(in) :: ipol_gfl
      type(base_force_address), intent(in) :: ipol_div_fil_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(fl_prop%iflag_4_filter_gravity .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_div_cst_buo_sph'
        call cal_div_cst_buo_sph                                        &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_buo,          &
     &      ipol_fil%i_temp, ipol_gfl%i_grad_temp,                      &
     &      ipol_div_fil_frc%i_buoyancy,                                &
     &      sph_rj%nidx_rj, sph_rj%a_r_1d_rj_r,                         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(fl_prop%iflag_4_filter_comp_buo .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_div_cst_buo_sph'
        call cal_div_cst_buo_sph                                        &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_comp_buo,     &
     &      ipol_fil%i_light, ipol_gfl%i_grad_composit,                 &
     &      ipol_div_fil_frc%i_comp_buo,                                &
     &      sph_rj%nidx_rj, sph_rj%a_r_1d_rj_r,                         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_div_r_const_fil_buo_sph
!
!-----------------------------------------------------------------------
!
      end module diff_r_const_buoyancy_sph
