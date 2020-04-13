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
     &     (dt, SGS_param, SPH_MHD%sph%sph_rj, MHD_prop, sph_MHD_bc,    &
     &      SPH_MHD%ipol, SPH_MHD%fld)
      else if(i_step .eq. 1) then
        if(iflag_debug.gt.0) write(*,*) 'cal_explicit_sph_SGS_euler'
        call cal_explicit_sph_SGS_euler                                 &
     &     (dt, SGS_param, SPH_MHD%sph%sph_rj, MHD_prop, sph_MHD_bc,    &
     &      SPH_MHD%ipol, SPH_MHD%fld)
        call cal_first_SGS_prev_step_adams                              &
     &     (SGS_param, SPH_MHD%sph%sph_rj, MHD_prop, sph_MHD_bc,        &
     &      SPH_MHD%ipol, SPH_MHD%fld)
      else
        if(iflag_debug.gt.0) write(*,*) 'cal_explicit_sph_SGS_adams'
        call cal_explicit_sph_SGS_adams                                 &
     &     (dt, SGS_param, SPH_MHD%sph%sph_rj, MHD_prop, sph_MHD_bc,    &
     &      SPH_MHD%ipol, SPH_MHD%fld)
      end if
!
      end subroutine sel_explicit_sph_SGS_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_explicit_sph_SGS_adams(dt, SGS_param,              &
     &          sph_rj, MHD_prop, sph_MHD_bc, ipol, rj_fld)
!
      use cal_vorticity_terms_adams
      use cal_nonlinear_sph_MHD
      use explicit_scalars_sph_w_SGS
      use cal_explicit_SGS_induction
!
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_vorticity_eq_adams                                       &
     &   (sph_rj, MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,                &
     &    ipol%base, ipol%exp_work, ipol%diffusion,                     &
     &    dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call sel_diff_induction_MHD_adams                                 &
     &   (SGS_param%iflag_SGS_uxb, dt, MHD_prop%cd_prop,                &
     &    ipol%base, ipol%exp_work, ipol%forces, ipol%diffusion,        &
     &    ipol%SGS_term, rj_fld)
!
      call explicit_scalars_sph_SGS_adams(dt, SGS_param,                &
     &    sph_rj, MHD_prop%ht_prop, MHD_prop%cp_prop,                   &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_C,                     &
     &    ipol%base, ipol%exp_work, ipol%forces, ipol%diffusion,        &
     &    ipol%div_SGS, rj_fld)
!
      end subroutine cal_explicit_sph_SGS_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_explicit_sph_SGS_euler(dt, SGS_param,              &
     &          sph_rj, MHD_prop, sph_MHD_bc, ipol, rj_fld)
!
      use cal_vorticity_terms_adams
      use explicit_scalars_sph_w_SGS
      use cal_explicit_SGS_induction
!
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_vorticity_eq_euler                                       &
     &   (sph_rj, MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,                &
     &    ipol%base, ipol%exp_work, ipol%diffusion,                     &
     &    dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call sel_diff_induction_MHD_euler                                 &
     &   (SGS_param%iflag_SGS_uxb, dt, MHD_prop%cd_prop,                &
     &    ipol%base, ipol%forces, ipol%diffusion, ipol%SGS_term,        &
     &    rj_fld)
!
      call explicit_scalars_sph_SGS_euler(dt, SGS_param,                &
     &    sph_rj, MHD_prop%ht_prop, MHD_prop%cp_prop,                   &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_C,                     &
     &    ipol%base, ipol%exp_work, ipol%forces, ipol%diffusion,        &
     &    ipol%div_SGS, rj_fld)
!
      end subroutine cal_explicit_sph_SGS_euler
!
! ----------------------------------------------------------------------
!
      subroutine cal_first_SGS_prev_step_adams(SGS_param,               &
     &          sph_rj, MHD_prop, sph_MHD_bc, ipol, rj_fld)
!
      use cal_vorticity_terms_adams
      use select_diff_adv_source
      use explicit_scalars_sph_w_SGS
      use cal_explicit_SGS_induction
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      call set_ini_adams_inertia(MHD_prop%fl_prop, ipol%exp_work,       &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call sel_ini_adams_mag_induct                                     &
     &   (SGS_param%iflag_SGS_uxb, MHD_prop%cd_prop,                    &
     &    ipol%exp_work, ipol%forces, ipol%SGS_term, rj_fld)
!
      call first_scalars_SGS_prev_adams(SGS_param,                      &
     &    sph_rj, MHD_prop%ht_prop, MHD_prop%cp_prop,                   &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_C,                     &
     &    ipol%base, ipol%exp_work, ipol%forces, ipol%div_SGS, rj_fld)
!
      end subroutine cal_first_SGS_prev_step_adams
!
! ----------------------------------------------------------------------
!
      end module momentum_w_SGS_explicit
