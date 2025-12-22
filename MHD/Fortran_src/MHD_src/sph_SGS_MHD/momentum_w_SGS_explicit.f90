!>@file   momentum_w_SGS_explicit.f90
!!@brief  module momentum_w_SGS_explicit
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2010
!
!>@brief Time integration for momentum equation by explicit scheme
!!
!!@verbatim
!!      subroutine sel_explicit_sph_SGS_MHD(time_d, SGS_par, sph,       &
!!     &          MHD_prop, sph_MHD_bc, ipol, ipol_LES, rj_fld)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_grids), intent(in) ::  sph
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module momentum_w_SGS_explicit
!
      use m_precision
!
      use t_control_parameter
      use t_physical_property
      use t_spheric_parameter
      use t_time_data
!
      use t_fdm_coefs
      use t_schmidt_poly_on_rtm
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_phys_data
      use t_phys_address
      use t_SGS_control_parameter
      use t_SGS_model_addresses
!
      implicit  none
!
      private :: sel_explicit_sph_SGS_induction
      private :: sel_explicit_sph_SGS_scalar
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph_SGS_MHD(time_d, SGS_par, sph,         &
     &          MHD_prop, sph_MHD_bc, ipol, ipol_LES, rj_fld)
!
      use cal_momentum_eq_explicit
!
      type(time_data), intent(in) :: time_d
      type(sph_grids), intent(in) ::  sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(SGS_paremeters), intent(in) :: SGS_par
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_explicit_sph_momentum(time_d, sph%sph_rj,                &
     &    MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U, ipol, rj_fld)
      call sel_explicit_sph_SGS_induction(time_d, SGS_par,              &
     &    MHD_prop%cd_prop, ipol, ipol_LES, rj_fld)
!
      call sel_explicit_sph_SGS_scalar                                  &
     &   (time_d, sph%sph_rj, SGS_par%model_p%SGS_heat,                 &
     &     MHD_prop%ht_prop, sph_MHD_bc%sph_bc_T,                       &
     &    ipol%diffusion%i_t_diffuse, ipol%forces%i_h_advect,           &
     &    ipol_LES%div_SGS%i_SGS_h_flux, ipol%base%i_heat_source,       &
     &    ipol%base%i_temp, ipol%exp_work%i_pre_heat, rj_fld)
      call sel_explicit_sph_SGS_scalar                                  &
     &   (time_d, sph%sph_rj, SGS_par%model_p%SGS_light,                &
     &    MHD_prop%cp_prop, sph_MHD_bc%sph_bc_C,                        &
     &    ipol%diffusion%i_c_diffuse, ipol%forces%i_c_advect,           &
     &    ipol_LES%div_SGS%i_SGS_c_flux, ipol%base%i_light_source,      &
     &    ipol%base%i_light, ipol%exp_work%i_pre_composit, rj_fld)
!
      end subroutine sel_explicit_sph_SGS_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph_SGS_induction(time_d, SGS_par,        &
     &          cd_prop, ipol, ipol_LES, rj_fld)
!
      use sel_diff_induction_MHD
      use cal_explicit_terms
!
      type(time_data), intent(in) :: time_d
      type(SGS_paremeters), intent(in) :: SGS_par
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(cd_prop%iflag_Bevo_scheme .eq. id_no_evolution) return
!
      if(cd_prop%coef_magne .eq. zero) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                  'sel_exp_static_induction_euler'
        call sel_exp_static_induction_euler(ipol%base, ipol%forces,     &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(cd_prop%iflag_Bevo_scheme .eq. id_explicit_euler) then
        call sel_diff_induction_MHD_euler(SGS_par%model_p, time_d%dt,   &
     &      cd_prop, ipol, ipol_LES, rj_fld)
      else if(time_d%i_time_step .eq. 1) then
        call sel_diff_induction_MHD_euler(SGS_par%model_p, time_d%dt,   &
     &      cd_prop, ipol, ipol_LES, rj_fld)
        call sel_ini_adams_mag_induct(SGS_par%model_p, cd_prop,         &
     &                                ipol, ipol_LES, rj_fld)
      else
        call sel_diff_induction_MHD_adams(SGS_par%model_p, time_d%dt,   &
     &      cd_prop, ipol, ipol_LES, rj_fld)
      end if
!
      end subroutine sel_explicit_sph_SGS_induction
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph_SGS_scalar                            &
     &         (time_d, sph_rj, SGS_scalar, scl_prop, sph_bc_S,         &
     &          ipol_diffuse, ipol_advect, ipol_SGS_advect,             &
     &          ipol_source, ipol_scalar, ipol_pre, rj_fld)
!
      use select_SGS_diff_adv_source
      use select_diff_adv_source
!
      type(time_data), intent(in) :: time_d
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(SGS_model_control_parameter), intent(in) :: SGS_scalar
      type(scalar_property), intent(in) :: scl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_S
      integer(kind = kint), intent(in) :: ipol_diffuse
      integer(kind = kint), intent(in) :: ipol_advect, ipol_SGS_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar, ipol_pre
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist, ied
!
!
      ist = (sph_bc_S%kr_in-1) * sph_rj%nidx_rj(2) + 1
      ied =  sph_bc_S%kr_out *   sph_rj%nidx_rj(2)
!
      if(scl_prop%iflag_scheme .eq. id_no_evolution) return
!
      if(scl_prop%coef_advect .eq. zero) then
        call sel_exp_static_src_euler(ist, ied, sph_rj%inod_rj_center,  &
     &      ipol_source, ipol_scalar, scl_prop%coef_source, rj_fld)
      else if(scl_prop%iflag_scheme .eq. id_explicit_euler) then
        call sel_scl_diff_adv_SGS_src_elr(SGS_scalar%iflag_SGS_flux,    &
     &      ist, ied, sph_rj%inod_rj_center, ipol_diffuse,              &
     &      ipol_advect, ipol_SGS_advect, ipol_source, ipol_scalar,     &
     &      time_d%dt, scl_prop%coef_exp, scl_prop%coef_advect,         &
     &      scl_prop%coef_source, rj_fld)
      else if(time_d%i_time_step .eq. 1) then
        call sel_scl_diff_adv_SGS_src_elr(SGS_scalar%iflag_SGS_flux,    &
     &      ist, ied, sph_rj%inod_rj_center, ipol_diffuse,              &
     &      ipol_advect, ipol_SGS_advect, ipol_source, ipol_scalar,     &
     &      time_d%dt, scl_prop%coef_exp, scl_prop%coef_advect,         &
     &      scl_prop%coef_source, rj_fld)
        call sel_ini_adams_scl_w_src_SGS                                &
     &     (SGS_scalar%iflag_SGS_flux, ist, ied, sph_rj%inod_rj_center, &
     &      ipol_advect, ipol_SGS_advect, ipol_source, ipol_pre,        &
     &      scl_prop%coef_source, rj_fld)
      else
        call sel_scl_diff_adv_SGS_src_adams                             &
     &     (SGS_scalar%iflag_SGS_flux, ist, ied, sph_rj%inod_rj_center, &
     &      ipol_diffuse, ipol_advect, ipol_SGS_advect,                 &
     &      ipol_source, ipol_scalar, ipol_pre, time_d%dt,              &
     &      scl_prop%coef_exp, scl_prop%coef_source, rj_fld)
      end if
!
      end subroutine sel_explicit_sph_SGS_scalar
!
! ----------------------------------------------------------------------
!
      end module momentum_w_SGS_explicit
