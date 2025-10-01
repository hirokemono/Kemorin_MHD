!>@file   explicit_scalars_sph_w_SGS.f90
!!@brief  module explicit_scalars_sph_w_SGS
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2010
!
!>@brief Time integration for momentum equation by explicit scheme
!!
!!@verbatim
!!      subroutine explicit_scalars_sph_SGS_adams(dt, SGS_param,        &
!!     &          sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,         &
!!     &          ipol_base, ipol_exp, ipol_frc, ipol_dif, ipol_div_SGS,&
!!     &          rj_fld)
!!      subroutine explicit_scalars_sph_SGS_euler(dt, SGS_param,        &
!!     &          sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,         &
!!     &          ipol_base, ipol_frc, ipol_dif, ipol_div_SGS, rj_fld)
!!      subroutine first_scalars_SGS_prev_adams(SGS_param,              &
!!     &          sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,         &
!!     &          ipol_base, ipol_exp, ipol_frc, ipol_div_SGS, rj_fld)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(diffusion_address), intent(in) :: ipol_dif
!!        type(SGS_term_address), intent(in) :: ipol_div_SGS
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param i_step  time step
!
      module explicit_scalars_sph_w_SGS
!
      use m_precision
!
      use t_SGS_control_parameter
      use t_spheric_rj_data
      use t_boundary_data_sph_MHD
      use t_physical_property
      use t_base_field_labels
      use t_base_force_labels
      use t_diffusion_term_labels
      use t_explicit_term_labels
      use t_SGS_term_labels
      use t_phys_data
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine explicit_scalars_sph_SGS_adams(dt, SGS_param,          &
     &          sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,           &
     &          ipol_base, ipol_exp, ipol_frc, ipol_dif, ipol_div_SGS,  &
     &          rj_fld)
!
      use select_SGS_diff_adv_source
!
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
      type(base_field_address), intent(in) :: ipol_base
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(SGS_term_address), intent(in) :: ipol_div_SGS
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'sel_scl_diff_adv_SGS_src_adams temperature'
        call sel_scl_diff_adv_SGS_src_adams                             &
     &    (SGS_param%SGS_heat%iflag_SGS_flux,                           &
     &     sph_bc_T%kr_in, sph_bc_T%kr_out, ipol_dif%i_t_diffuse,       &
     &     ipol_frc%i_h_advect, ipol_div_SGS%i_SGS_h_flux,              &
     &     ipol_base%i_heat_source, ipol_base%i_temp,                   &
     &     ipol_exp%i_pre_heat, dt,                                     &
     &     ht_prop%coef_exp, ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'sel_scl_diff_adv_SGS_src_adams composition'
        call sel_scl_diff_adv_SGS_src_adams(SGS_param%SGS_light%iflag_SGS_flux, &
     &     sph_bc_C%kr_in, sph_bc_C%kr_out, ipol_dif%i_c_diffuse,       &
     &     ipol_frc%i_c_advect, ipol_div_SGS%i_SGS_c_flux,              &
     &     ipol_base%i_light_source, ipol_base%i_light,                 &
     &     ipol_exp%i_pre_composit, dt,                                 &
     &     cp_prop%coef_exp, cp_prop%coef_source, sph_rj, rj_fld)
      end if
!
!  Center evolution
!
      if(sph_rj%inod_rj_center .eq. 0) return
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_ctr_scl_SGS_dadv_src_adms temperature'
        call sel_ctr_scl_SGS_dadv_src_adms                              &
     &     (SGS_param%SGS_heat%iflag_SGS_flux, ipol_dif%i_t_diffuse,    &
     &      ipol_frc%i_h_advect, ipol_div_SGS%i_SGS_h_flux,             &
     &      ipol_base%i_heat_source, ipol_base%i_temp,                  &
     &      ipol_exp%i_pre_heat, dt, ht_prop%coef_exp,                  &
     &      ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'sel_ctr_scl_SGS_dadv_src_adms composition'
        call sel_ctr_scl_SGS_dadv_src_adms(SGS_param%SGS_light%iflag_SGS_flux,  &
     &      ipol_dif%i_c_diffuse, ipol_frc%i_c_advect,                  &
     &      ipol_div_SGS%i_SGS_c_flux, ipol_base%i_light_source,        &
     &      ipol_base%i_light, ipol_exp%i_pre_composit,                 &
     &      dt, cp_prop%coef_exp, cp_prop%coef_source, sph_rj, rj_fld)
      end if
!
      end subroutine explicit_scalars_sph_SGS_adams
!
! ----------------------------------------------------------------------
!
      subroutine explicit_scalars_sph_SGS_euler(dt, SGS_param,          &
     &          sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,           &
     &          ipol_base, ipol_frc, ipol_dif, ipol_div_SGS, rj_fld)
!
      use select_SGS_diff_adv_source
!
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(SGS_term_address), intent(in) :: ipol_div_SGS
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_scl_diff_adv_SGS_src_elr temperature'
        call sel_scl_diff_adv_SGS_src_elr                               &
     &    (SGS_param%SGS_heat%iflag_SGS_flux,                           &
     &     sph_bc_T%kr_in, sph_bc_T%kr_out, ipol_dif%i_t_diffuse,       &
     &     ipol_frc%i_h_advect, ipol_div_SGS%i_SGS_h_flux,              &
     &     ipol_base%i_heat_source, ipol_base%i_temp, dt,               &
     &     ht_prop%coef_exp, ht_prop%coef_advect, ht_prop%coef_source,  &
     &     sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_scl_diff_adv_SGS_src_elr composition'
        call sel_scl_diff_adv_SGS_src_elr(SGS_param%SGS_light%iflag_SGS_flux,   &
     &     sph_bc_C%kr_in, sph_bc_C%kr_out, ipol_dif%i_c_diffuse,       &
     &     ipol_frc%i_c_advect, ipol_div_SGS%i_SGS_c_flux,              &
     &     ipol_base%i_light_source, ipol_base%i_light, dt,             &
     &     cp_prop%coef_exp, cp_prop%coef_advect, cp_prop%coef_source,  &
     &     sph_rj, rj_fld)
      end if
!
!   Center evolution
!
      if(sph_rj%inod_rj_center .eq. 0) return
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_ctr_scl_SGS_dadv_src_elr temperature'
        call sel_ctr_scl_SGS_dadv_src_elr                               &
     &     (SGS_param%SGS_heat%iflag_SGS_flux, ipol_dif%i_t_diffuse,    &
     &      ipol_frc%i_h_advect, ipol_div_SGS%i_SGS_h_flux,             &
     &      ipol_base%i_heat_source, ipol_base%i_temp, dt,              &
     &      ht_prop%coef_exp, ht_prop%coef_advect, ht_prop%coef_source, &
     &      sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_ctr_scl_SGS_dadv_src_elr composition'
        call sel_ctr_scl_SGS_dadv_src_elr                               &
     &     (SGS_param%SGS_light%iflag_SGS_flux, ipol_dif%i_c_diffuse,           &
     &      ipol_frc%i_c_advect, ipol_div_SGS%i_SGS_c_flux,             &
     &      ipol_base%i_light_source, ipol_base%i_light, dt,            &
     &      cp_prop%coef_exp, cp_prop%coef_advect, cp_prop%coef_source, &
     &      sph_rj, rj_fld)
      end if
!
      end subroutine explicit_scalars_sph_SGS_euler
!
! ----------------------------------------------------------------------
!
      subroutine first_scalars_SGS_prev_adams(SGS_param,                &
     &          sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,           &
     &          ipol_base, ipol_exp, ipol_frc, ipol_div_SGS, rj_fld)
!
      use select_SGS_diff_adv_source
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
      type(base_field_address), intent(in) :: ipol_base
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(SGS_term_address), intent(in) :: ipol_div_SGS
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        call sel_ini_adams_sscl_w_src_SGS                               &
     &     (SGS_param%SGS_heat%iflag_SGS_flux,                          &
     &      sph_bc_T%kr_in, sph_bc_T%kr_out, ipol_frc%i_h_advect,       &
     &      ipol_div_SGS%i_SGS_h_flux, ipol_base%i_heat_source,         &
     &      ipol_exp%i_pre_heat, ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call sel_ini_adams_sscl_w_src_SGS(SGS_param%SGS_light%iflag_SGS_flux,   &
     &      sph_bc_C%kr_in, sph_bc_C%kr_out, ipol_frc%i_c_advect,       &
     &      ipol_div_SGS%i_SGS_c_flux, ipol_base%i_light_source,        &
     &      ipol_exp%i_pre_composit, cp_prop%coef_source,               &
     &      sph_rj, rj_fld)
      end if
!
!   Center evolution
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        call sel_ctr_ini_adams_scl_w_src                                &
     &     (SGS_param%SGS_heat%iflag_SGS_flux, ipol_frc%i_h_advect,     &
     &      ipol_div_SGS%i_SGS_h_flux, ipol_base%i_heat_source,         &
     &      ipol_exp%i_pre_heat, ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call sel_ctr_ini_adams_scl_w_src                                &
     &     (SGS_param%SGS_light%iflag_SGS_flux, ipol_frc%i_c_advect,            &
     &      ipol_div_SGS%i_SGS_c_flux, ipol_base%i_light_source,        &
     &      ipol_exp%i_pre_composit, cp_prop%coef_source,               &
     &      sph_rj, rj_fld)
      end if
!
      end subroutine first_scalars_SGS_prev_adams
!
! ----------------------------------------------------------------------
!
      end module explicit_scalars_sph_w_SGS
