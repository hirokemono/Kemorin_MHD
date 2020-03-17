!>@file   momentum_w_SGS_explicit.f90
!!@brief  module momentum_w_SGS_explicit
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2010
!
!>@brief Time integration for momentum equation by explicit scheme
!!
!!@verbatim
!!      subroutine sel_explicit_sph_SGS_MHD                             &
!!     &         (i_step, dt, SGS_param, MHD_prop, sph_MHD_bc, SPH_MHD)
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!@endverbatim
!!
!!@param i_step  time step
!
      module momentum_w_SGS_explicit
!
      use m_precision
!
      use t_SGS_control_parameter
      use t_control_parameter
      use t_physical_property
      use t_SPH_mesh_field_data
      use t_fdm_coefs
      use t_schmidt_poly_on_rtm
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
!
      implicit  none
!
      private :: cal_explicit_sph_SGS_euler
      private :: cal_first_SGS_prev_step_adams
      private :: cal_explicit_sph_SGS_adams
      private :: sel_diff_induction_MHD_adams
      private :: sel_diff_induction_MHD_euler
      private :: sel_ini_adams_mag_induct
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph_SGS_MHD                               &
     &         (i_step, dt, SGS_param, MHD_prop, sph_MHD_bc, SPH_MHD)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!
      if(MHD_prop%iflag_all_scheme .eq. id_explicit_euler) then
        call cal_explicit_sph_SGS_euler                                 &
     &     (dt, SGS_param, SPH_MHD%sph%sph_rj,                          &
     &      MHD_prop%fl_prop, MHD_prop%cd_prop,                         &
     &      MHD_prop%ht_prop, MHD_prop%cp_prop, sph_MHD_bc%sph_bc_U,    &
     &      sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_C,                   &
     &      SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
      else if(i_step .eq. 1) then
        if(iflag_debug.gt.0) write(*,*) 'cal_explicit_sph_SGS_euler'
        call cal_explicit_sph_SGS_euler                                 &
     &     (dt, SGS_param, SPH_MHD%sph%sph_rj,                          &
     &      MHD_prop%fl_prop, MHD_prop%cd_prop,                         &
     &      MHD_prop%ht_prop, MHD_prop%cp_prop, sph_MHD_bc%sph_bc_U,    &
     &      sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_C,                   &
     &      SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
        call cal_first_SGS_prev_step_adams                              &
     &     (SGS_param, SPH_MHD%sph%sph_rj,                              &
     &      MHD_prop%fl_prop, MHD_prop%cd_prop,                         &
     &      MHD_prop%ht_prop, MHD_prop%cp_prop,                         &
     &      sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_C,                   &
     &      SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
      else
        if(iflag_debug.gt.0) write(*,*) 'cal_explicit_sph_SGS_adams'
        call cal_explicit_sph_SGS_adams                                 &
     &     (dt, SGS_param, SPH_MHD%sph%sph_rj,                          &
     &      MHD_prop%fl_prop, MHD_prop%cd_prop,                         &
     &      MHD_prop%ht_prop, MHD_prop%cp_prop, sph_MHD_bc%sph_bc_U,    &
     &      sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_C,                   &
     &      SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
      end if
!
      end subroutine sel_explicit_sph_SGS_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_explicit_sph_SGS_adams(dt, SGS_param,              &
     &          sph_rj, fl_prop, cd_prop, ht_prop, cp_prop,             &
     &          sph_bc_U, sph_bc_T, sph_bc_C, ipol, itor, rj_fld)
!
      use cal_vorticity_terms_adams
      use cal_nonlinear_sph_MHD
      use select_SGS_diff_adv_source
!
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist, ied
!
!
      if(fl_prop%iflag_scheme .gt.     id_no_evolution) then
        ist = (sph_bc_U%kr_in-1)*sph_rj%nidx_rj(2) + 1
        ied = sph_bc_U%kr_out * sph_rj%nidx_rj(2)
        call cal_vorticity_eq_adams                                     &
     &     (ipol, itor, ist, ied, dt, fl_prop%coef_exp,                 &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(cd_prop%iflag_Bevo_scheme .gt.    id_no_evolution) then
        call sel_diff_induction_MHD_adams                               &
     &     (SGS_param%iflag_SGS_uxb, dt, cd_prop, ipol, itor, rj_fld)
      end if
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'sel_scl_diff_adv_SGS_src_adams temperature'
        call sel_scl_diff_adv_SGS_src_adams(SGS_param%iflag_SGS_h_flux, &
     &     sph_bc_T%kr_in, sph_bc_T%kr_out, ipol%diffusion%i_t_diffuse, &
     &     ipol%forces%i_h_advect, ipol%div_SGS%i_SGS_h_flux,           &
     &     ipol%base%i_heat_source, ipol%base%i_temp, ipol%i_pre_heat,  &
     &     dt, ht_prop%coef_exp, ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'sel_scl_diff_adv_SGS_src_adams composition'
        call sel_scl_diff_adv_SGS_src_adams(SGS_param%iflag_SGS_c_flux, &
     &     sph_bc_C%kr_in, sph_bc_C%kr_out, ipol%diffusion%i_c_diffuse, &
     &     ipol%forces%i_c_advect, ipol%div_SGS%i_SGS_c_flux,           &
     &     ipol%base%i_light_source, ipol%base%i_light,                 &
     &     ipol%exp_work%i_pre_composit, dt,                            &
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
     &     (SGS_param%iflag_SGS_h_flux, ipol%diffusion%i_t_diffuse,     &
     &      ipol%forces%i_h_advect, ipol%div_SGS%i_SGS_h_flux,          &
     &      ipol%base%i_heat_source, ipol%base%i_temp, ipol%i_pre_heat, &
     &      dt, ht_prop%coef_exp, ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'sel_ctr_scl_SGS_dadv_src_adms composition'
        call sel_ctr_scl_SGS_dadv_src_adms(SGS_param%iflag_SGS_c_flux,  &
     &      ipol%diffusion%i_c_diffuse, ipol%forces%i_c_advect,         &
     &      ipol%div_SGS%i_SGS_c_flux, ipol%base%i_light_source,        &
     &      ipol%base%i_light, ipol%exp_work%i_pre_composit,            &
     &      dt, cp_prop%coef_exp, cp_prop%coef_source, sph_rj, rj_fld)
      end if
!
      end subroutine cal_explicit_sph_SGS_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_explicit_sph_SGS_euler(dt, SGS_param, sph_rj,      &
     &         fl_prop, cd_prop, ht_prop, cp_prop,                      &
     &         sph_bc_U, sph_bc_T, sph_bc_C, ipol, itor, rj_fld)
!
      use cal_vorticity_terms_adams
      use select_SGS_diff_adv_source
!
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist, ied
!
!
      if(fl_prop%iflag_scheme .gt.     id_no_evolution) then
        ist = (sph_bc_U%kr_in-1)*sph_rj%nidx_rj(2) + 1
        ied = sph_bc_U%kr_out * sph_rj%nidx_rj(2)
        call cal_vorticity_eq_euler                                     &
     &     (ipol, itor, ist, ied, dt, fl_prop%coef_exp,                 &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(cd_prop%iflag_Bevo_scheme .gt.    id_no_evolution) then
        call sel_diff_induction_MHD_euler(SGS_param%iflag_SGS_uxb,      &
     &      dt, cd_prop, ipol, itor, rj_fld)
      end if
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_scl_diff_adv_SGS_src_elr temperature'
        call sel_scl_diff_adv_SGS_src_elr(SGS_param%iflag_SGS_h_flux,   &
     &     sph_bc_T%kr_in, sph_bc_T%kr_out, ipol%diffusion%i_t_diffuse, &
     &     ipol%forces%i_h_advect, ipol%div_SGS%i_SGS_h_flux,           &
     &     ipol%base%i_heat_source, ipol%base%i_temp, dt,               &
     &     ht_prop%coef_exp, ht_prop%coef_advect, ht_prop%coef_source,  &
     &     sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_scl_diff_adv_SGS_src_elr composition'
        call sel_scl_diff_adv_SGS_src_elr(SGS_param%iflag_SGS_c_flux,   &
     &     sph_bc_C%kr_in, sph_bc_C%kr_out, ipol%diffusion%i_c_diffuse, &
     &     ipol%forces%i_c_advect, ipol%div_SGS%i_SGS_c_flux,           &
     &     ipol%base%i_light_source, ipol%base%i_light, dt,             &
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
     &     (SGS_param%iflag_SGS_h_flux, ipol%diffusion%i_t_diffuse,     &
     &      ipol%forces%i_h_advect, ipol%div_SGS%i_SGS_h_flux,          &
     &      ipol%base%i_heat_source, ipol%base%i_temp, dt,              &
     &      ht_prop%coef_exp, ht_prop%coef_advect, ht_prop%coef_source, &
     &      sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_ctr_scl_SGS_dadv_src_elr composition'
        call sel_ctr_scl_SGS_dadv_src_elr                               &
     &     (SGS_param%iflag_SGS_c_flux, ipol%diffusion%i_c_diffuse,     &
     &      ipol%forces%i_c_advect, ipol%div_SGS%i_SGS_c_flux,          &
     &      ipol%base%i_light_source, ipol%base%i_light, dt,            &
     &      cp_prop%coef_exp, cp_prop%coef_advect, cp_prop%coef_source, &
     &      sph_rj, rj_fld)
      end if
!
      end subroutine cal_explicit_sph_SGS_euler
!
! ----------------------------------------------------------------------
!
      subroutine cal_first_SGS_prev_step_adams(SGS_param, sph_rj,       &
     &          fl_prop, cd_prop, ht_prop, cp_prop, sph_bc_T, sph_bc_C, &
     &          ipol, itor, rj_fld)
!
      use cal_vorticity_terms_adams
      use select_diff_adv_source
      use select_SGS_diff_adv_source
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
      if(fl_prop%iflag_scheme .gt.     id_no_evolution) then
        call set_ini_adams_inertia(ipol, itor,                          &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(cd_prop%iflag_Bevo_scheme .gt.    id_no_evolution) then
        call sel_ini_adams_mag_induct                                   &
     &     (SGS_param%iflag_SGS_uxb, ipol, itor, rj_fld)
      end if
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        call sel_ini_adams_sscl_w_src_SGS(SGS_param%iflag_SGS_h_flux,   &
     &      sph_bc_T%kr_in, sph_bc_T%kr_out, ipol%forces%i_h_advect,    &
     &      ipol%div_SGS%i_SGS_h_flux, ipol%base%i_heat_source,         &
     &      ipol%i_pre_heat, ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call sel_ini_adams_sscl_w_src_SGS(SGS_param%iflag_SGS_c_flux,   &
     &      sph_bc_C%kr_in, sph_bc_C%kr_out, ipol%forces%i_c_advect,    &
     &      ipol%div_SGS%i_SGS_c_flux, ipol%base%i_light_source,        &
     &      ipol%exp_work%i_pre_composit, cp_prop%coef_source,          &
     &      sph_rj, rj_fld)
      end if
!
!   Center evolution
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        call sel_ctr_ini_adams_scl_w_src                                &
     &     (SGS_param%iflag_SGS_h_flux, ipol%forces%i_h_advect,         &
     &      ipol%div_SGS%i_SGS_h_flux, ipol%base%i_heat_source,         &
     &      ipol%i_pre_heat, ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call sel_ctr_ini_adams_scl_w_src                                &
     &     (SGS_param%iflag_SGS_c_flux, ipol%forces%i_c_advect,         &
     &      ipol%div_SGS%i_SGS_c_flux, ipol%base%i_light_source,        &
     &      ipol%exp_work%i_pre_composit, cp_prop%coef_source,          &
     &      sph_rj, rj_fld)
      end if
!
      end subroutine cal_first_SGS_prev_step_adams
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_diff_induction_MHD_adams                           &
     &         (iflag_SGS, dt, cd_prop, ipol, itor, rj_fld)
!
      use cal_explicit_terms
!
      integer(kind = kint), intent(in) :: iflag_SGS
      real(kind = kreal), intent(in) :: dt
!
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(iflag_SGS .gt. id_SGS_none) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'cal_diff_induction_wSGS_adams'
        call cal_diff_induction_wSGS_adams                              &
     &     (dt, cd_prop%coef_exp, ipol, itor,                           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'cal_diff_induction_MHD_adams'
        call cal_diff_induction_MHD_adams                               &
     &     (dt, cd_prop%coef_exp, ipol, itor,                           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_diff_induction_MHD_adams
!
! ----------------------------------------------------------------------
!
      subroutine sel_diff_induction_MHD_euler                           &
     &         (iflag_SGS, dt, cd_prop, ipol, itor, rj_fld)
!
      use cal_explicit_terms
!
      integer(kind = kint), intent(in) :: iflag_SGS
      real(kind = kreal), intent(in) :: dt
!
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
      if(iflag_SGS .gt. id_SGS_none) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'cal_diff_induction_wSGS_euler'
        call cal_diff_induction_wSGS_euler                              &
     &     (dt, cd_prop%coef_exp, ipol, itor,                           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'cal_diff_induction_MHD_euler'
        call cal_diff_induction_MHD_euler                               &
     &     (dt, cd_prop%coef_exp, ipol, itor,                           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_diff_induction_MHD_euler
!
! ----------------------------------------------------------------------
!
      subroutine sel_ini_adams_mag_induct                               &
     &         (iflag_SGS, ipol, itor, rj_fld)
!
      use cal_explicit_terms
!
      integer(kind = kint), intent(in) :: iflag_SGS
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(iflag_SGS .gt. id_SGS_none) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'SGS_ini_adams_mag_induct'
        call SGS_ini_adams_mag_induct(ipol, itor,                       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'set_ini_adams_mag_induct'
        call set_ini_adams_mag_induct(ipol, itor,                       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_ini_adams_mag_induct
!
! ----------------------------------------------------------------------
!
      end module momentum_w_SGS_explicit
