!>@file   cal_momentum_eq_explicit.f90
!!@brief  module cal_momentum_eq_explicit
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2010
!
!>@brief Time integration for momentum equation by explicit scheme
!!
!!@verbatim
!!      subroutine sel_explicit_sph(i_step, dt, SGS_param, MHD_prop,    &
!!     &          sph_rj, ipol, itor, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(scalar_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol, itor
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param i_step  time step
!
      module cal_momentum_eq_explicit
!
      use m_precision
!
      use t_SGS_control_parameter
      use t_control_parameter
      use t_physical_property
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_schmidt_poly_on_rtm
!
      implicit  none
!
      private :: cal_explicit_sph_euler, cal_first_prev_step_adams
      private :: cal_explicit_sph_adams
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
      subroutine sel_explicit_sph(i_step, dt, SGS_param, MHD_prop,      &
     &          sph_rj, ipol, itor, rj_fld)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(MHD_prop%iflag_all_scheme .eq. id_explicit_euler) then
        call cal_explicit_sph_euler(dt, SGS_param, sph_rj,              &
     &      MHD_prop%fl_prop, MHD_prop%cd_prop,                         &
     &      MHD_prop%ht_prop, MHD_prop%cp_prop, ipol, itor, rj_fld)
      else if(i_step .eq. 1) then
        if(iflag_debug.gt.0) write(*,*) 'cal_explicit_sph_euler'
        call cal_explicit_sph_euler(dt, SGS_param, sph_rj,              &
     &      MHD_prop%fl_prop, MHD_prop%cd_prop,                         &
     &      MHD_prop%ht_prop, MHD_prop%cp_prop, ipol, itor, rj_fld)
        call cal_first_prev_step_adams(SGS_param, sph_rj,               &
     &      MHD_prop%fl_prop, MHD_prop%cd_prop,                         &
     &      MHD_prop%ht_prop, MHD_prop%cp_prop, ipol, itor, rj_fld)
      else
        if(iflag_debug.gt.0) write(*,*) 'cal_explicit_sph_adams'
        call cal_explicit_sph_adams(dt, SGS_param, sph_rj,              &
     &      MHD_prop%fl_prop, MHD_prop%cd_prop,                         &
     &      MHD_prop%ht_prop, MHD_prop%cp_prop, ipol, itor, rj_fld)
      end if
!
      end subroutine sel_explicit_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_explicit_sph_adams(dt, SGS_param,                  &
     &          sph_rj, fl_prop, cd_prop, ht_prop, cp_prop,             &
     &          ipol, itor, rj_fld)
!
      use m_boundary_params_sph_MHD
      use cal_vorticity_terms_adams
      use cal_nonlinear_sph_MHD
      use select_diff_adv_source
!
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
!
!$omp parallel
      if(fl_prop%iflag_scheme .gt.     id_no_evolution) then
        call cal_vorticity_eq_adams(ipol, itor,                         &
     &      sph_bc_U%kr_in, sph_bc_U%kr_out, dt, fl_prop%coef_exp,      &
     &      rj_fld%n_point,sph_rj%nidx_rj(2), rj_fld%ntot_phys,         &
     &      rj_fld%d_fld)
      end if
!
      if(cd_prop%iflag_Bevo_scheme .gt.    id_no_evolution) then
        call sel_diff_induction_MHD_adams                               &
     &     (SGS_param%iflag_SGS_uxb, dt, cd_prop, ipol, itor, rj_fld)
      end if
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'sel_scalar_diff_adv_src_adams temperature'
        call sel_scalar_diff_adv_src_adams(SGS_param%iflag_SGS_h_flux,  &
     &      sph_bc_T%kr_in, sph_bc_T%kr_out, ipol%i_t_diffuse,          &
     &      ipol%i_h_advect, ipol%i_SGS_div_h_flux,                     &
     &      ipol%i_heat_source, ipol%i_temp, ipol%i_pre_heat,           &
     &      dt, ht_prop%coef_exp, ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'sel_scalar_diff_adv_src_adams composition'
        call sel_scalar_diff_adv_src_adams(SGS_param%iflag_SGS_c_flux,  &
     &      sph_bc_C%kr_in, sph_bc_C%kr_out, ipol%i_c_diffuse,          &
     &      ipol%i_c_advect, ipol%i_SGS_div_c_flux,                     &
     &      ipol%i_light_source, ipol%i_light, ipol%i_pre_composit,     &
     &      dt, cp_prop%coef_exp, cp_prop%coef_source, sph_rj, rj_fld)
      end if
!$omp end parallel
!
!  Center evolution
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_ct_scl_diff_adv_src_adams temperature'
        call sel_ct_scl_diff_adv_src_adams                              &
     &     (SGS_param%iflag_SGS_h_flux, ipol%i_t_diffuse,               &
     &      ipol%i_h_advect, ipol%i_SGS_div_h_flux,                     &
     &      ipol%i_heat_source, ipol%i_temp, ipol%i_pre_heat,           &
     &      dt, ht_prop%coef_exp, ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'sel_ct_scl_diff_adv_src_adams composition'
        call sel_ct_scl_diff_adv_src_adams                              &
     &     (SGS_param%iflag_SGS_c_flux, ipol%i_c_diffuse,               &
     &      ipol%i_c_advect, ipol%i_SGS_div_c_flux,                     &
     &      ipol%i_light_source, ipol%i_light, ipol%i_pre_composit,     &
     &      dt, cp_prop%coef_exp, cp_prop%coef_source, sph_rj, rj_fld)
      end if
!
      end subroutine cal_explicit_sph_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_explicit_sph_euler(dt, SGS_param, sph_rj,          &
     &         fl_prop, cd_prop, ht_prop, cp_prop, ipol, itor, rj_fld)
!
      use m_boundary_params_sph_MHD
      use cal_vorticity_terms_adams
      use select_diff_adv_source
!
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
!$omp parallel
      if(fl_prop%iflag_scheme .gt.     id_no_evolution) then
        call cal_vorticity_eq_euler(ipol, itor,                         &
     &      sph_bc_U%kr_in, sph_bc_U%kr_out, dt, fl_prop%coef_exp,      &
     &      rj_fld%n_point, sph_rj%nidx_rj(2), rj_fld%ntot_phys,        &
     &      rj_fld%d_fld)
      end if
!
      if(cd_prop%iflag_Bevo_scheme .gt.    id_no_evolution) then
        call sel_diff_induction_MHD_euler(SGS_param%iflag_SGS_uxb,      &
     &      dt, cd_prop, ipol, itor, rj_fld)
      end if
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_scalar_diff_adv_src_euler temperature'
        call sel_scalar_diff_adv_src_euler(SGS_param%iflag_SGS_h_flux,  &
     &      sph_bc_T%kr_in, sph_bc_T%kr_out, ipol%i_t_diffuse,          &
     &      ipol%i_h_advect, ipol%i_SGS_div_h_flux,                     &
     &      ipol%i_heat_source, ipol%i_temp, dt, ht_prop%coef_exp,      &
     &      ht_prop%coef_advect, ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_scalar_diff_adv_src_euler composition'
        call sel_scalar_diff_adv_src_euler(SGS_param%iflag_SGS_c_flux,  &
     &      sph_bc_C%kr_in, sph_bc_C%kr_out, ipol%i_c_diffuse,          &
     &      ipol%i_c_advect, ipol%i_SGS_div_c_flux,                     &
     &      ipol%i_light_source, ipol%i_light, dt, cp_prop%coef_exp,    &
     &      cp_prop%coef_advect, cp_prop%coef_source, sph_rj, rj_fld)
      end if
!$omp end parallel
!
!   Center evolution
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_ctr_scl_diff_adv_src_euler temperature'
        call sel_ctr_scl_diff_adv_src_euler                             &
     &     (SGS_param%iflag_SGS_h_flux, ipol%i_t_diffuse,               &
     &      ipol%i_h_advect, ipol%i_SGS_div_h_flux,                     &
     &      ipol%i_heat_source, ipol%i_temp, dt, ht_prop%coef_exp,      &
     &      ht_prop%coef_advect, ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_ctr_scl_diff_adv_src_euler composition'
        call sel_ctr_scl_diff_adv_src_euler                             &
     &     (SGS_param%iflag_SGS_c_flux, ipol%i_c_diffuse,               &
     &      ipol%i_c_advect, ipol%i_SGS_div_c_flux,                     &
     &      ipol%i_light_source, ipol%i_light, dt, cp_prop%coef_exp,    &
     &      cp_prop%coef_advect, cp_prop%coef_source, sph_rj, rj_fld)
      end if
!
      end subroutine cal_explicit_sph_euler
!
! ----------------------------------------------------------------------
!
      subroutine cal_first_prev_step_adams(SGS_param, sph_rj,           &
     &          fl_prop, cd_prop, ht_prop, cp_prop, ipol, itor, rj_fld)
!
      use m_boundary_params_sph_MHD
      use cal_vorticity_terms_adams
      use select_diff_adv_source
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
!$omp parallel
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
        call sel_ini_adams_scalar_w_src(SGS_param%iflag_SGS_h_flux,     &
     &      sph_bc_T%kr_in, sph_bc_T%kr_out, ipol%i_h_advect,           &
     &      ipol%i_SGS_div_h_flux, ipol%i_heat_source,                  &
     &      ipol%i_pre_heat, ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call sel_ini_adams_scalar_w_src(SGS_param%iflag_SGS_c_flux,     &
     &      sph_bc_C%kr_in, sph_bc_C%kr_out, ipol%i_c_advect,           &
     &      ipol%i_SGS_div_c_flux, ipol%i_light_source,                 &
     &      ipol%i_pre_composit, cp_prop%coef_source, sph_rj, rj_fld)
      end if
!$omp end parallel
!
!   Center evolution
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        call sel_ctr_ini_adams_scl_w_src                                &
     &     (SGS_param%iflag_SGS_h_flux, ipol%i_h_advect,                &
     &      ipol%i_SGS_div_h_flux, ipol%i_heat_source,                  &
     &      ipol%i_pre_heat, ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call sel_ctr_ini_adams_scl_w_src                                &
     &     (SGS_param%iflag_SGS_c_flux, ipol%i_c_advect,                &
     &      ipol%i_SGS_div_c_flux, ipol%i_light_source,                 &
     &      ipol%i_pre_composit, cp_prop%coef_source, sph_rj, rj_fld)
      end if
!
      end subroutine cal_first_prev_step_adams
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
      end module cal_momentum_eq_explicit
