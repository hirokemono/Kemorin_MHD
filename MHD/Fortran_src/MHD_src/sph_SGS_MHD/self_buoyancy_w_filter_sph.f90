!>@file   self_buoyancy_w_filter_sph.f90
!!@brief  module self_buoyancy_w_filter_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate divergence of buoyancy for pressure evaluation
!!
!!@verbatim
!!      subroutine cal_self_buoyancy_sph_SGS_MHD                        &
!!     &         (sph, leg, ipol, ipol_LES, MHD_prop, sph_bc_U, rj_fld)
!!      subroutine sel_rot_filter_buoyancy_sph                          &
!!     &         (sph, ipol_LES, MHD_prop, sph_bc_U, rj_fld)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_grids), intent(in) ::  sph
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic velocity
!!                 boundary condition parameters
!
      module self_buoyancy_w_filter_sph
!
      use m_precision
      use m_machine_parameter
!
      use t_control_parameter
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_boundary_params_sph_MHD
!
      use t_physical_property
      use t_reference_scalar_param
      use t_base_field_labels
      use t_base_force_labels
      use t_grad_field_labels
!
      implicit  none
!
      private :: sel_self_filtered_buo_sph
      private :: sel_rot_self_filtered_buo_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_self_buoyancy_sph_SGS_MHD                          &
     &         (sph, leg, ipol, ipol_LES, MHD_prop, sph_bc_U, rj_fld)
!
      use t_schmidt_poly_on_rtm
      use cal_self_buoyancies_sph
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_grids), intent(in) :: sph
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
      call sel_buoyancies_sph_MHD                                       &
     &   (sph%sph_rj, leg, ipol%base, ipol%forces,                      &
     &    MHD_prop%fl_prop, MHD_prop%ref_param_T, MHD_prop%ref_param_C, &
     &    sph_bc_U, rj_fld)
!
      call sel_self_filtered_buo_sph(sph%sph_rj, leg,                   &
     &    ipol_LES%filter_fld, ipol_LES%force_by_filter,                &
     &    MHD_prop%fl_prop, sph_bc_U, rj_fld)
!
      end subroutine cal_self_buoyancy_sph_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sel_rot_filter_buoyancy_sph                            &
     &         (sph, ipol_LES, MHD_prop, sph_bc_U, rj_fld)
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_grids), intent(in) ::  sph
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(MHD_prop%fl_prop%i_grav .eq. iflag_radial_g) then
        call sel_rot_r_cst_filtered_buo_sph                             &
     &    (sph%sph_rj, ipol_LES%filter_fld, ipol_LES%rot_frc_by_filter, &
     &      MHD_prop%fl_prop, sph_bc_U, rj_fld)
      else
        call sel_rot_self_filtered_buo_sph                              &
     &    (sph%sph_rj, ipol_LES%filter_fld, ipol_LES%rot_frc_by_filter, &
     &      MHD_prop%fl_prop, sph_bc_U, rj_fld)
      end if
!
      end subroutine sel_rot_filter_buoyancy_sph
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_self_filtered_buo_sph                              &
     &         (sph_rj, leg, ipol_fil, ipol_fil_frc, fl_prop,           &
     &          sph_bc_U, rj_fld)
!
      use t_schmidt_poly_on_rtm
!
      use cal_buoyancies_sph_MHD
      use adjust_reference_fields
!
      type(legendre_4_sph_trans), intent(in) :: leg
      type(fluid_property), intent(in) :: fl_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_fil
      type(base_force_address), intent(in) :: ipol_fil_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol_fil_frc%i_buoyancy .gt. 0) then
          if (iflag_debug.ge.1)  write(*,*)                             &
     &      'cal_self_buoyancy_sph_MHD by filtrered temperature'
        call cal_self_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out, &
     &      leg%g_sph_rj, fl_prop%coef_buo,                             &
     &      ipol_fil%i_temp, ipol_fil_frc%i_buoyancy,    &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call delete_sphere_average                                      &
     &     (ipol_fil_frc%i_buoyancy, sph_rj, rj_fld)
      end if
!
      if(ipol_fil_frc%i_comp_buo .gt. 0) then
        if (iflag_debug.ge.1)  write(*,*)                               &
     &      'cal_self_buoyancy_sph_MHD by filtrered composition'
        call cal_self_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out, &
     &      leg%g_sph_rj, fl_prop%coef_comp_buo,                        &
     &      ipol_fil%i_light, ipol_fil_frc%i_comp_buo,   &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call delete_sphere_average                                      &
     &     (ipol_fil_frc%i_comp_buo, sph_rj, rj_fld)
      end if
!
      end subroutine sel_self_filtered_buo_sph
!
!-----------------------------------------------------------------------
!
      subroutine sel_rot_self_filtered_buo_sph(sph_rj,                  &
     &          ipol_fil, ipol_rot_fil_frc, fl_prop, sph_bc_U, rj_fld)
!
      use cal_buoyancies_sph_MHD
!
      type(fluid_property), intent(in) :: fl_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_fil
      type(base_force_address), intent(in) :: ipol_rot_fil_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (fl_prop%iflag_4_filter_gravity) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'rot_self_buoyancy_sph_MHD by filtrered temperature'
        call rot_self_buoyancy_sph_MHD                                  &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_buo,          &
     &      ipol_fil%i_temp, ipol_rot_fil_frc%i_buoyancy,               &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(fl_prop%iflag_4_filter_comp_buo) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'rot_self_buoyancy_sph_MHD by filtrered composition'
        call rot_self_buoyancy_sph_MHD                                  &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_comp_buo,     &
     &      ipol_fil%i_light, ipol_rot_fil_frc%i_comp_buo,              &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_rot_self_filtered_buo_sph
!
!-----------------------------------------------------------------------
!
      subroutine sel_rot_r_cst_filtered_buo_sph(sph_rj,                 &
     &          ipol_fil, ipol_rot_fil_frc, fl_prop, sph_bc_U, rj_fld)
!
      use cal_buoyancies_sph_MHD
!
      type(fluid_property), intent(in) :: fl_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_fil
      type(base_force_address), intent(in) :: ipol_rot_fil_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (fl_prop%iflag_4_filter_gravity) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'rot_r_const_buoyancy_sph_MHD by filtrered temperature'
        call rot_r_const_buoyancy_sph_MHD                               &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_buo,          &
     &      ipol_fil%i_temp, ipol_rot_fil_frc%i_buoyancy,               &
     &      sph_rj%nidx_rj, rj_fld%n_point, rj_fld%ntot_phys,           &
     &      rj_fld%d_fld)
      end if
!
      if(fl_prop%iflag_4_filter_comp_buo) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'rot_r_const_buoyancy_sph_MHD by filtrered composition'
        call rot_r_const_buoyancy_sph_MHD                               &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_comp_buo,     &
     &      ipol_fil%i_light, ipol_rot_fil_frc%i_comp_buo,              &
     &      sph_rj%nidx_rj, rj_fld%n_point, rj_fld%ntot_phys,           &
     &      rj_fld%d_fld)
      end if
!
      end subroutine sel_rot_r_cst_filtered_buo_sph
!
!-----------------------------------------------------------------------
!
      end module self_buoyancy_w_filter_sph
