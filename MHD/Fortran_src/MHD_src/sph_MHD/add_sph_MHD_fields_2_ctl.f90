!>@file   add_sph_MHD_fields_2_ctl.f90
!!@brief  module add_sph_MHD_fields_2_ctl
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Add fields in control list for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_field_name_4_sph_mhd                             &
!!     &         (evo_T, evo_C, fl_prop, cd_prop, field_ctl)
!!      subroutine add_field_name_4_SGS(SGS_param, field_ctl)
!!      subroutine add_field_name_dynamic_SGS                           &
!!     &         (SGS_param, fl_prop, field_ctl)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!
      module add_sph_MHD_fields_2_ctl
!
      use m_precision
!
      use m_phys_labels
      use t_time_stepping_parameter
      use t_SGS_control_parameter
      use t_read_control_arrays
      use t_physical_property
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_field_name_4_sph_mhd                               &
     &         (evo_T, evo_C, fl_prop, cd_prop, field_ctl)
!
      use add_nodal_fields_ctl
!
      type(time_evolution_params), intent(in) :: evo_T, evo_C
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
!   velocity flag
      if(fl_prop%iflag_scheme .gt. id_no_evolution                      &
     &     .or. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
      end if
!   vorticity flag
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_vort, field_ctl)
      end if
!   magnetic field flag
      if(cd_prop%iflag_Bevo_scheme .gt. id_no_evolution                 &
     &     .or. fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_current, field_ctl)
      end if
!
!   gradient of temperature flag
      if(evo_T%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_grad_temp, field_ctl)
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
        call add_phys_name_ctl(fhd_grad_par_temp, field_ctl)
      end if
!
!   gradient of dummy scalar flag
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_grad_composit, field_ctl)
        call add_phys_name_ctl(fhd_part_light, field_ctl)
        call add_phys_name_ctl(fhd_grad_par_light, field_ctl)
      end if
!
!
!
!   advection flag
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_viscous, field_ctl)
        call add_phys_name_ctl(fhd_div_viscous, field_ctl)
        call add_phys_name_ctl(fhd_w_viscous, field_ctl)
!
        call add_phys_name_ctl(fhd_inertia, field_ctl)
        call add_phys_name_ctl(fhd_rot_inertia, field_ctl)
        call add_phys_name_ctl(fhd_div_inertia, field_ctl)
!
!   Coriolis flag
        if(fl_prop%iflag_4_coriolis .gt. id_turn_OFF) then
          call add_phys_name_ctl(fhd_Coriolis, field_ctl)
          call add_phys_name_ctl(fhd_rot_Coriolis, field_ctl)
          call add_phys_name_ctl(fhd_div_Coriolis, field_ctl)
        end if
!   Lorentz flag
        if(fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
          call add_phys_name_ctl(fhd_Lorentz, field_ctl)
          call add_phys_name_ctl(fhd_rot_Lorentz, field_ctl)
          call add_phys_name_ctl(fhd_div_Lorentz, field_ctl)
        end if
!   buoyancy flag
        if(fl_prop%iflag_4_gravity .gt. id_turn_OFF) then
          call add_phys_name_ctl(fhd_buoyancy, field_ctl)
          call add_phys_name_ctl(fhd_rot_buoyancy, field_ctl)
          call add_phys_name_ctl(fhd_div_buoyancy, field_ctl)
        end if
!   compositional buoyancy flag
        if(fl_prop%iflag_4_composit_buo .gt. id_turn_OFF) then
          call add_phys_name_ctl(fhd_comp_buo, field_ctl)
          call add_phys_name_ctl(fhd_div_comp_buo, field_ctl)
          call add_phys_name_ctl(fhd_rot_comp_buo, field_ctl)
        end if
!   filtered buoyancy flag
        if(fl_prop%iflag_4_filter_gravity .gt. id_turn_OFF) then
          call add_phys_name_ctl(fhd_filter_buo, field_ctl)
          call add_phys_name_ctl(fhd_div_filter_buo, field_ctl)
          call add_phys_name_ctl(fhd_rot_filter_buo, field_ctl)
        end if
      end if
!
!   induction flag
      if(cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_mag_diffuse, field_ctl)
!
        call add_phys_name_ctl(fhd_mag_induct, field_ctl)
        call add_phys_name_ctl(fhd_vp_induct, field_ctl)
      end if
!
!   divergence of heat flux flag
      if(evo_T%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_thermal_diffusion, field_ctl)
        call add_phys_name_ctl(fhd_h_flux, field_ctl)
        call add_phys_name_ctl(fhd_heat_advect, field_ctl)
      end if
!
!   divergence of dummy scalar flag
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_c_diffuse, field_ctl)
        call add_phys_name_ctl(fhd_c_flux, field_ctl)
        call add_phys_name_ctl(fhd_composit_advect, field_ctl)
      end if
!
      end subroutine add_field_name_4_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine add_field_name_4_SGS(SGS_param, field_ctl)
!
      use add_nodal_fields_ctl
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(SGS_param%iflag_SGS_h_flux .gt. id_SGS_none) then
        call add_phys_name_ctl(fhd_filter_velo, field_ctl)
        call add_phys_name_ctl(fhd_filter_temp, field_ctl)
!
        call add_phys_name_ctl(fhd_SGS_h_flux, field_ctl)
        call add_phys_name_ctl(fhd_div_SGS_h_flux, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_c_flux .gt. id_SGS_none) then
        call add_phys_name_ctl(fhd_filter_velo, field_ctl)
        call add_phys_name_ctl(fhd_filter_comp, field_ctl)
!
        call add_phys_name_ctl(fhd_SGS_c_flux, field_ctl)
        call add_phys_name_ctl(fhd_div_SGS_c_flux, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_m_flux .gt. id_SGS_none) then
        call add_phys_name_ctl(fhd_filter_velo, field_ctl)
        call add_phys_name_ctl(fhd_filter_vort, field_ctl)
!
        call add_phys_name_ctl(fhd_SGS_inertia, field_ctl)
        call add_phys_name_ctl(fhd_SGS_rot_inertia, field_ctl)
        call add_phys_name_ctl(fhd_SGS_div_inertia, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_lorentz .gt. id_SGS_none) then
        call add_phys_name_ctl(fhd_filter_magne, field_ctl)
        call add_phys_name_ctl(fhd_filter_current, field_ctl)
!
        call add_phys_name_ctl(fhd_SGS_Lorentz, field_ctl)
        call add_phys_name_ctl(fhd_SGS_rot_Lorentz, field_ctl)
        call add_phys_name_ctl(fhd_SGS_div_Lorentz, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_uxb .gt. id_SGS_none) then
        call add_phys_name_ctl(fhd_filter_velo, field_ctl)
        call add_phys_name_ctl(fhd_filter_magne, field_ctl)
!
        call add_phys_name_ctl(fhd_SGS_vp_induct, field_ctl)
        call add_phys_name_ctl(fhd_SGS_induction, field_ctl)
      end if
!
      end subroutine add_field_name_4_SGS
!
! -----------------------------------------------------------------------
!
      subroutine add_field_name_dynamic_SGS                             &
     &         (SGS_param, fl_prop, field_ctl)
!
      use add_nodal_fields_ctl
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(fluid_property), intent(in) :: fl_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
!
      if(SGS_param%iflag_SGS_h_flux .gt. id_SGS_none) then
        call add_phys_name_ctl(fhd_w_filter_velo, field_ctl)
        call add_phys_name_ctl(fhd_w_filter_temp, field_ctl)
!
        call add_phys_name_ctl(fhd_wide_SGS_h_flux, field_ctl)
        call add_phys_name_ctl(fhd_Csim_SGS_h_flux, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_c_flux .gt. id_SGS_none) then
        call add_phys_name_ctl(fhd_w_filter_velo, field_ctl)
        call add_phys_name_ctl(fhd_w_filter_comp, field_ctl)
!
        call add_phys_name_ctl(fhd_wide_SGS_c_flux, field_ctl)
        call add_phys_name_ctl(fhd_Csim_SGS_c_flux, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_m_flux .gt. id_SGS_none) then
        call add_phys_name_ctl(fhd_w_filter_velo, field_ctl)
        call add_phys_name_ctl(fhd_w_filter_vort, field_ctl)
!
        call add_phys_name_ctl(fhd_wide_SGS_inertia, field_ctl)
        call add_phys_name_ctl(fhd_Csim_SGS_m_flux, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_lorentz .gt. id_SGS_none) then
        call add_phys_name_ctl(fhd_w_filter_magne, field_ctl)
        call add_phys_name_ctl(fhd_w_filter_current, field_ctl)
!
        call add_phys_name_ctl(fhd_wide_SGS_Lorentz, field_ctl)
        call add_phys_name_ctl(fhd_Csim_SGS_Lorentz, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_uxb .gt. id_SGS_none) then
        call add_phys_name_ctl(fhd_w_filter_velo, field_ctl)
        call add_phys_name_ctl(fhd_w_filter_magne, field_ctl)
!
        call add_phys_name_ctl(fhd_wide_SGS_vp_induct, field_ctl)
        call add_phys_name_ctl(fhd_Csim_SGS_induction, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_gravity .gt. id_SGS_none) then
        call add_phys_name_ctl(fhd_Reynolds_work, field_ctl)
!
        if(fl_prop%iflag_4_gravity .gt. id_turn_OFF) then
          call add_phys_name_ctl(fhd_SGS_buo_flux, field_ctl)
          call add_phys_name_ctl(fhd_Csim_SGS_buoyancy, field_ctl)
        end if
!
        if(fl_prop%iflag_4_composit_buo .gt. id_turn_OFF) then
          call add_phys_name_ctl(fhd_SGS_comp_buo_flux, field_ctl)
          call add_phys_name_ctl(fhd_Csim_SGS_comp_buo, field_ctl)
        end if
      end if
!
!      if(SGS_param%iflag_SGS_gravity .gt. id_SGS_none) then
!        call add_phys_name_ctl(fhd_filter_velo, field_ctl)
!        call add_phys_name_ctl(fhd_filter_temp, field_ctl)
!
!        call add_phys_name_ctl(fhd_SGS_comp_buo, field_ctl)
!      end if
!
      end subroutine add_field_name_dynamic_SGS
!
! -----------------------------------------------------------------------
!
      end module add_sph_MHD_fields_2_ctl
