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
!!     &         (sph_rj, leg, ipol, MHD_prop, sph_bc_U, rj_fld)
!!      subroutine cal_div_buoyancy_w_fil_sph_2(sph_rj, r_2nd,          &
!!     &          MHD_prop, sph_bc_U, g_sph_rj, ipol, ipol_LES, rj_fld)
!!      subroutine rot_self_filter_buoyancy_sph                         &
!!     &         (sph_rj, ipol, MHD_prop, sph_bc_U, rj_fld)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_rj_grid), intent(in) ::  sph_rj
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
      private :: sel_div_self_filtered_buo_sph
      private :: cal_div_of_filtered_buo_sph_2
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_self_buoyancy_sph_SGS_MHD                          &
     &         (sph_rj, leg, ipol, MHD_prop, sph_bc_U, rj_fld)
!
      use t_schmidt_poly_on_rtm
      use cal_self_buoyancies_sph
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
      call sel_buoyancies_sph_MHD(sph_rj, leg, ipol%base, ipol%forces,  &
     &    MHD_prop%fl_prop, MHD_prop%ref_param_T, MHD_prop%ref_param_C, &
     &    sph_bc_U, rj_fld)
!
      call sel_self_filtered_buo_sph                                    &
     &   (sph_rj, leg, ipol%filter_fld, ipol%force_by_filter,           &
     &    MHD_prop%fl_prop, sph_bc_U, rj_fld)
!
      end subroutine cal_self_buoyancy_sph_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_buoyancy_w_fil_sph_2(sph_rj, r_2nd,            &
     &          MHD_prop, sph_bc_U, g_sph_rj, ipol, ipol_LES, rj_fld)
!
      use t_fdm_coefs
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(sph_boundary_type), intent(in) :: sph_bc_U
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
      call cal_div_of_filtered_buo_sph_2(sph_rj, r_2nd,                 &
     &    MHD_prop%fl_prop, sph_bc_U, g_sph_rj,                         &
     &    ipol%force_by_filter, ipol_LES%div_frc_by_filter, rj_fld)
!
!      call sel_div_self_filtered_buo_sph(sph_rj, ipol%filter_fld,      &
!     &    ipol_LES%grad_fil_fld, ipol_LES%div_frc_by_filter,           &
!     &    MHD_prop%fl_prop, sph_bc_U, rj_fld)
!
      end subroutine cal_div_buoyancy_w_fil_sph_2
!
!-----------------------------------------------------------------------
!
      subroutine rot_self_filter_buoyancy_sph                           &
     &         (sph_rj, ipol, MHD_prop, sph_bc_U, rj_fld)
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_rot_self_filtered_buo_sph                                &
     &   (sph_rj, ipol%filter_fld, ipol%rot_frc_by_filter,              &
     &    MHD_prop%fl_prop, sph_bc_U, rj_fld)
!
      end subroutine rot_self_filter_buoyancy_sph
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
      use cal_self_buoyancies_sph
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
     &      'cal_buoyancy_sph_MHD by filtrered temperature'
        call cal_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
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
     &      'cal_buoyancy_sph_MHD by filtrered composition'
        call cal_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
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
      use rot_self_buoyancies_sph
!
      type(fluid_property), intent(in) :: fl_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_fil
      type(base_force_address), intent(in) :: ipol_rot_fil_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: it_rot_buo
!
!
      if (fl_prop%iflag_4_filter_gravity .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'cal_rot_buoyancy_sph_MHD by filtrered temperature'
        it_rot_buo = ipol_rot_fil_frc%i_buoyancy + 2
        call cal_rot_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out,  &
     &      fl_prop%coef_buo, ipol_fil%i_temp, it_rot_buo,              &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(fl_prop%iflag_4_filter_comp_buo .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'cal_rot_buoyancy_sph_MHD by filtrered composition'
        it_rot_buo = ipol_rot_fil_frc%i_comp_buo + 2
        call cal_rot_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out,  &
     &      fl_prop%coef_comp_buo, ipol_fil%i_light, it_rot_buo,        &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_rot_self_filtered_buo_sph
!
!-----------------------------------------------------------------------
!
      subroutine sel_div_self_filtered_buo_sph                          &
     &         (sph_rj, ipol_fil, ipol_gfl, ipol_div_fil_frc,           &
     &          fl_prop, sph_bc_U, rj_fld)
!
      use div_self_buoyancies_sph
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
          if (iflag_debug.ge.1)  write(*,*)                             &
     &      'cal_div_buoyancy_sph_MHD by filtrered temperature'
        call cal_div_buoyancy_sph_MHD                                   &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_buo,          &
     &      ipol_fil%i_temp, ipol_gfl%i_grad_temp,                      &
     &      ipol_div_fil_frc%i_buoyancy,                                &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(fl_prop%iflag_4_filter_comp_buo .gt. id_turn_OFF) then
        if (iflag_debug.ge.1)  write(*,*)                               &
     &      'cal_div_buoyancy_sph_MHD by filtrered composition'
        call cal_div_buoyancy_sph_MHD                                   &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_comp_buo,     &
     &      ipol_fil%i_light, ipol_gfl%i_grad_composit,                 &
     &      ipol_div_fil_frc%i_comp_buo,                                &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_div_self_filtered_buo_sph
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_of_filtered_buo_sph_2                          &
     &         (sph_rj, r_2nd, fl_prop, sph_bc_U, g_sph_rj,             &
     &          ipol_fil_frc, ipol_div_fil_frc, rj_fld)
!
      use t_fdm_coefs
      use const_sph_divergence
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(base_force_address), intent(in) :: ipol_fil_frc
      type(base_force_address), intent(in) :: ipol_div_fil_frc
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(fl_prop%iflag_4_filter_gravity .gt. id_turn_OFF) then
        call const_sph_div_force(sph_rj, r_2nd, sph_bc_U, g_sph_rj,     &
     &      ipol_fil_frc%i_buoyancy, ipol_div_fil_frc%i_buoyancy,       &
     &      rj_fld)
      end if
!
      if(fl_prop%iflag_4_filter_comp_buo .gt. id_turn_OFF) then
        call const_sph_div_force(sph_rj, r_2nd, sph_bc_U, g_sph_rj,     &
     &      ipol_fil_frc%i_comp_buo, ipol_div_fil_frc%i_comp_buo,       &
     &      rj_fld)
      end if
!
      end subroutine cal_div_of_filtered_buo_sph_2
!
!-----------------------------------------------------------------------
!
      end module self_buoyancy_w_filter_sph
