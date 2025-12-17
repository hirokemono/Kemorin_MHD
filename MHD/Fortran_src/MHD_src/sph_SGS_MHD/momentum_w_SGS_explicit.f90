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
!!     &         (i_step, dt, MHD_prop, sph_MHD_bc, SPH_SGS, SPH_MHD)
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(SPH_SGS_structure), intent(in) :: SPH_SGS
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
      use t_SPH_SGS_structure
      use t_SPH_mesh_field_data
      use t_fdm_coefs
      use t_schmidt_poly_on_rtm
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_phys_address
      use t_SGS_model_addresses
!
      implicit  none
!
      private :: sel_explicit_sph_SGS_momentum
      private :: sel_explicit_sph_SGS_induction
      private :: sel_explicit_sph_SGS_temp, sel_explicit_sph_SGS_comp
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
!
      subroutine sel_explicit_sph_SGS_MHD                               &
     &         (i_step, dt, MHD_prop, sph_MHD_bc, SPH_SGS, SPH_MHD)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: dt
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(SPH_SGS_structure), intent(in) :: SPH_SGS
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!
      call sel_explicit_sph_SGS_momentum                                &
     &   (i_step, dt, SPH_MHD%sph, MHD_prop%fl_prop,                    &
     &    sph_MHD_bc%sph_bc_U, SPH_MHD%ipol, SPH_MHD%fld)
      call sel_explicit_sph_SGS_induction                               &
     &   (i_step, dt, SPH_SGS%SGS_par, MHD_prop%cd_prop,                &
     &    SPH_MHD%ipol, SPH_SGS%ipol_LES, SPH_MHD%fld)
!
      call sel_explicit_sph_SGS_temp(i_step, dt, SPH_SGS%SGS_par,       &
     &    SPH_MHD%sph, MHD_prop%ht_prop, sph_MHD_bc%sph_bc_T,           &
     &    SPH_MHD%ipol, SPH_SGS%ipol_LES, SPH_MHD%fld)
      call sel_explicit_sph_SGS_comp(i_step, dt, SPH_SGS%SGS_par,       &
     &    SPH_MHD%sph, MHD_prop%cp_prop, sph_MHD_bc%sph_bc_C,           &
     &    SPH_MHD%ipol, SPH_SGS%ipol_LES, SPH_MHD%fld)
!
      end subroutine sel_explicit_sph_SGS_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph_SGS_momentum                          &
     &         (i_step, dt, sph, fl_prop, sph_bc_U, ipol, rj_fld)
!
      use cal_vorticity_terms_adams
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: dt
!
      type(sph_grids), intent(in) :: sph
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(fl_prop%iflag_scheme .eq. id_explicit_euler) then
        call cal_vorticity_eq_euler(sph%sph_rj, fl_prop, sph_bc_U,      &
     &      ipol%base, ipol%exp_work, ipol%diffusion,                   &
     &      dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(i_step .eq. 1) then
        call cal_vorticity_eq_euler(sph%sph_rj, fl_prop, sph_bc_U,      &
     &      ipol%base, ipol%exp_work, ipol%diffusion,                   &
     &      dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call set_ini_adams_inertia(fl_prop, ipol%exp_work,              &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call cal_vorticity_eq_adams(sph%sph_rj, fl_prop, sph_bc_U,      &
     &      ipol%base, ipol%exp_work, ipol%diffusion,                   &
     &      dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_explicit_sph_SGS_momentum
!
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph_SGS_induction(i_step, dt, SGS_par,    &
     &          cd_prop, ipol, ipol_LES, rj_fld)
!
      use sel_diff_induction_MHD
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(   cd_prop%iflag_Bevo_scheme .eq. id_explicit_euler            &
     & .or. cd_prop%iflag_Aevo_scheme .eq. id_explicit_euler) then
        call sel_diff_induction_MHD_euler(SGS_par%model_p, dt,          &
     &      cd_prop, ipol, ipol_LES, rj_fld)
      else if(i_step .eq. 1) then
        call sel_diff_induction_MHD_euler(SGS_par%model_p, dt,          &
     &      cd_prop, ipol, ipol_LES, rj_fld)
        call sel_ini_adams_mag_induct                                   &
     &     (SGS_par%model_p, cd_prop, ipol, ipol_LES, rj_fld)
      else
        call sel_diff_induction_MHD_adams                               &
     &    (SGS_par%model_p, dt, cd_prop, ipol, ipol_LES, rj_fld)
      end if
!
      end subroutine sel_explicit_sph_SGS_induction
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph_SGS_temp(i_step, dt, SGS_par,         &
     &          sph, ht_prop, sph_bc_T, ipol, ipol_LES, rj_fld)
!
      use explicit_scalars_sph_w_SGS
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_grids), intent(in) :: sph
      type(scalar_property), intent(in) :: ht_prop
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ht_prop%iflag_scheme .eq. id_no_evolution) return
      if(ht_prop%iflag_scheme .eq. id_explicit_euler) then
        call explicit_temp_sph_SGS_euler(dt, SGS_par%model_p%SGS_heat,  &
     &      sph%sph_rj, ht_prop, sph_bc_T,                              &
     &      ipol%base, ipol%forces, ipol%diffusion, ipol_LES%div_SGS,   &
     &      rj_fld)
      else if(i_step .eq. 1) then
        call explicit_temp_sph_SGS_euler(dt, SGS_par%model_p%SGS_heat,  &
     &      sph%sph_rj, ht_prop, sph_bc_T,                              &
     &      ipol%base, ipol%forces, ipol%diffusion, ipol_LES%div_SGS,   &
     &      rj_fld)
        call first_temp_SGS_prev_adams(SGS_par%model_p%SGS_heat,        &
     &      sph%sph_rj, ht_prop, sph_bc_T,                              &
     &      ipol%base, ipol%exp_work, ipol%forces, ipol_LES%div_SGS,    &
     &      rj_fld)
      else
        call explicit_temp_sph_SGS_adams(dt, SGS_par%model_p%SGS_heat,  &
     &      sph%sph_rj, ht_prop, sph_bc_T,                              &
     &      ipol%base, ipol%exp_work, ipol%forces, ipol%diffusion,      &
     &      ipol_LES%div_SGS, rj_fld)
      end if
!
      end subroutine sel_explicit_sph_SGS_temp
!
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph_SGS_comp(i_step, dt, SGS_par,         &
     &          sph, cp_prop, sph_bc_C, ipol, ipol_LES, rj_fld)
!
      use explicit_scalars_sph_w_SGS
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_grids), intent(in) :: sph
      type(scalar_property), intent(in) :: cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_C
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(cp_prop%iflag_scheme .eq. id_no_evolution) return
      if(cp_prop%iflag_scheme .eq. id_explicit_euler) then
        call explicit_comp_sph_SGS_euler(dt, SGS_par%model_p%SGS_light, &
     &      sph%sph_rj, cp_prop, sph_bc_C,                              &
     &      ipol%base, ipol%forces, ipol%diffusion, ipol_LES%div_SGS,   &
     &      rj_fld)
      else if(i_step .eq. 1) then
        call explicit_comp_sph_SGS_euler(dt, SGS_par%model_p%SGS_light, &
     &      sph%sph_rj, cp_prop, sph_bc_C,                              &
     &      ipol%base, ipol%forces, ipol%diffusion, ipol_LES%div_SGS,   &
     &      rj_fld)
        call first_comp_SGS_prev_adams(SGS_par%model_p%SGS_light,       &
     &      sph%sph_rj, cp_prop, sph_bc_C,                              &
     &      ipol%base, ipol%exp_work, ipol%forces, ipol_LES%div_SGS,    &
     &      rj_fld)
      else
        call explicit_comp_sph_SGS_adams(dt, SGS_par%model_p%SGS_light, &
     &      sph%sph_rj, cp_prop, sph_bc_C,                              &
     &      ipol%base, ipol%exp_work, ipol%forces, ipol%diffusion,      &
     &      ipol_LES%div_SGS, rj_fld)
      end if
!
      end subroutine sel_explicit_sph_SGS_comp
!
! ----------------------------------------------------------------------
!
      end module momentum_w_SGS_explicit
