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
!!     &          ipol_base, ipol_exp, ipol_dif, ipol_frc,              &
!!     &          ipol_fil_frc, ipol_div_SGS, rj_fld)
!!      subroutine explicit_scalars_sph_SGS_euler(dt, SGS_param,        &
!!     &          sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,         &
!!     &          ipol_base, ipol_dif, ipol_frc, ipol_fil_frc,          &
!!     &          ipol_div_SGS, rj_fld)
!!      subroutine first_scalars_SGS_prev_adams(SGS_param,              &
!!     &          sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,         &
!!     &          ipol_base, ipol_exp, ipol_frc, ipol_fil_frc,          &
!!     &          ipol_div_SGS, rj_fld)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(base_force_address), intent(in) :: ipol_fil_frc
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
      private :: sel_scl_diff_adv_fil_SGS_adams
      private :: sel_scl_diff_adv_fil_SGS_elr
      private :: sel_ini_adams_scl_w_fil_SGS
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine explicit_scalars_sph_SGS_adams(dt, SGS_param,          &
     &          sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,           &
     &          ipol_base, ipol_exp, ipol_dif, ipol_frc,                &
     &          ipol_fil_frc, ipol_div_SGS, rj_fld)
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
      type(base_force_address), intent(in) :: ipol_fil_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(SGS_term_address), intent(in) :: ipol_div_SGS
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'sel_scl_diff_adv_fil_SGS_adams temperature'
        call sel_scl_diff_adv_fil_SGS_adams                             &
     &    (ht_prop%iflag_4_filter_advection,                            &
     &     SGS_param%iflag_SGS_h_flux, sph_bc_T%kr_in, sph_bc_T%kr_out, &
     &     ipol_dif%i_t_diffuse, ipol_frc%i_h_advect,                   &
     &     ipol_fil_frc%i_h_advect,  ipol_div_SGS%i_SGS_h_flux,         &
     &     ipol_base%i_heat_source, ipol_base%i_temp,                   &
     &     ipol_exp%i_pre_heat, dt,                                     &
     &     ht_prop%coef_exp, ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'sel_scl_diff_adv_fil_SGS_adams composition'
        call sel_scl_diff_adv_fil_SGS_adams                             &
     &    (cp_prop%iflag_4_filter_advection,                            &
     &     SGS_param%iflag_SGS_c_flux, sph_bc_C%kr_in, sph_bc_C%kr_out, &
     &     ipol_dif%i_c_diffuse, ipol_frc%i_c_advect,                   &
     &     ipol_fil_frc%i_c_advect, ipol_div_SGS%i_SGS_c_flux,          &
     &     ipol_base%i_light_source, ipol_base%i_light,                 &
     &     ipol_exp%i_pre_composit, dt,                                 &
     &     cp_prop%coef_exp, cp_prop%coef_source, sph_rj, rj_fld)
      end if
!
      end subroutine explicit_scalars_sph_SGS_adams
!
! ----------------------------------------------------------------------
!
      subroutine explicit_scalars_sph_SGS_euler(dt, SGS_param,          &
     &          sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,           &
     &          ipol_base, ipol_dif, ipol_frc, ipol_fil_frc,            &
     &          ipol_div_SGS, rj_fld)
!
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_frc
      type(base_force_address), intent(in) :: ipol_fil_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(SGS_term_address), intent(in) :: ipol_div_SGS
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_scl_diff_adv_fil_SGS_elr temperature'
        call sel_scl_diff_adv_fil_SGS_elr                               &
     &    (ht_prop%iflag_4_filter_advection,                            &
     &     SGS_param%iflag_SGS_h_flux, sph_bc_T%kr_in, sph_bc_T%kr_out, &
     &     ipol_dif%i_t_diffuse, ipol_frc%i_h_advect,                   &
     &     ipol_fil_frc%i_h_advect, ipol_div_SGS%i_SGS_h_flux,          &
     &     ipol_base%i_heat_source, ipol_base%i_temp, dt,               &
     &     ht_prop%coef_exp, ht_prop%coef_advect, ht_prop%coef_source,  &
     &     sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'sel_scl_diff_adv_fil_SGS_elr composition'
        call sel_scl_diff_adv_fil_SGS_elr                               &
     &    (cp_prop%iflag_4_filter_advection,                            &
     &     SGS_param%iflag_SGS_c_flux, sph_bc_C%kr_in, sph_bc_C%kr_out, &
     &     ipol_dif%i_c_diffuse, ipol_frc%i_c_advect,                   &
     &     ipol_fil_frc%i_c_advect, ipol_div_SGS%i_SGS_c_flux,          &
     &     ipol_base%i_light_source, ipol_base%i_light, dt,             &
     &     cp_prop%coef_exp, cp_prop%coef_advect, cp_prop%coef_source,  &
     &     sph_rj, rj_fld)
      end if
!
      end subroutine explicit_scalars_sph_SGS_euler
!
! ----------------------------------------------------------------------
!
      subroutine first_scalars_SGS_prev_adams(SGS_param,                &
     &          sph_rj, ht_prop, cp_prop, sph_bc_T, sph_bc_C,           &
     &          ipol_base, ipol_exp, ipol_frc, ipol_fil_frc,            &
     &          ipol_div_SGS, rj_fld)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
      type(base_field_address), intent(in) :: ipol_base
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(base_force_address), intent(in) :: ipol_fil_frc
      type(SGS_term_address), intent(in) :: ipol_div_SGS
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ht_prop%iflag_scheme .gt.     id_no_evolution) then
        call sel_ini_adams_scl_w_fil_SGS                                &
     &    (ht_prop%iflag_4_filter_advection,                            &
     &     SGS_param%iflag_SGS_h_flux, sph_bc_T%kr_in, sph_bc_T%kr_out, &
     &     ipol_frc%i_h_advect, ipol_fil_frc%i_h_advect,                &
     &     ipol_div_SGS%i_SGS_h_flux, ipol_base%i_heat_source,          &
     &     ipol_exp%i_pre_heat, ht_prop%coef_source, sph_rj, rj_fld)
      end if
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call sel_ini_adams_scl_w_fil_SGS                                &
     &    (cp_prop%iflag_4_filter_advection,                            &
     &     SGS_param%iflag_SGS_c_flux, sph_bc_C%kr_in, sph_bc_C%kr_out, &
     &     ipol_frc%i_c_advect, ipol_fil_frc%i_c_advect,                &
     &     ipol_div_SGS%i_SGS_c_flux, ipol_base%i_light_source,         &
     &     ipol_exp%i_pre_composit, cp_prop%coef_source,                &
     &     sph_rj, rj_fld)
      end if
!
      end subroutine first_scalars_SGS_prev_adams
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_scl_diff_adv_fil_SGS_adams                         &
     &         (iflag_4_filter_advection, iflag_SGS, kr_st, kr_ed,      &
     &          ipol_diffuse, ipol_advect, ipol_fil_advect,             &
     &          ipol_SGS_advect, ipol_source, ipol_scalar, ipol_pre,    &
     &          dt, coef_exp, coef_src, sph_rj, rj_fld)
!
      use select_SGS_diff_adv_source
!
      type(sph_rj_grid), intent(in) :: sph_rj
      logical, intent(in) :: iflag_4_filter_advection
      integer(kind = kint), intent(in) :: iflag_SGS
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_fil_advect
      integer(kind = kint), intent(in) :: ipol_SGS_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar, ipol_pre
      real(kind = kreal), intent(in) :: coef_exp, coef_src
      real(kind = kreal), intent(in) :: dt
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(iflag_4_filter_advection .eqv. .FALSE.) then
        call sel_scl_diff_adv_SGS_src_adams                             &
     &     (iflag_SGS, kr_st, kr_ed, ipol_diffuse, ipol_advect,         &
     &      ipol_SGS_advect, ipol_source, ipol_scalar, ipol_pre,        &
     &      dt, coef_exp, coef_src, sph_rj, rj_fld)
      else
        call sel_scl_diff_adv_SGS_src_adams                             &
     &     (iflag_SGS, kr_st, kr_ed, ipol_diffuse, ipol_fil_advect,     &
     &      ipol_SGS_advect, ipol_source, ipol_scalar, ipol_pre,        &
     &      dt, coef_exp, coef_src, sph_rj, rj_fld)
      end if
!
      end subroutine sel_scl_diff_adv_fil_SGS_adams
!
! ----------------------------------------------------------------------
!
      subroutine sel_scl_diff_adv_fil_SGS_elr                           &
     &         (iflag_4_filter_advection, iflag_SGS, kr_st, kr_ed,      &
     &          ipol_diffuse, ipol_advect, ipol_fil_advect,             &
     &          ipol_SGS_advect, ipol_source, ipol_scalar,              &
     &          dt, coef_exp, coef_adv, coef_src, sph_rj, rj_fld)
!
      use select_SGS_diff_adv_source
!
      type(sph_rj_grid), intent(in) :: sph_rj
      logical, intent(in) :: iflag_4_filter_advection
      integer(kind = kint), intent(in) :: iflag_SGS
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_fil_advect
      integer(kind = kint), intent(in) :: ipol_SGS_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar
      real(kind = kreal), intent(in) :: coef_exp, coef_adv, coef_src
      real(kind = kreal), intent(in) :: dt
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(iflag_4_filter_advection .eqv. .FALSE.) then
        call sel_scl_diff_adv_SGS_src_elr                               &
     &     (iflag_SGS, kr_st, kr_ed, ipol_diffuse, ipol_advect,         &
     &      ipol_SGS_advect, ipol_source, ipol_scalar,                  &
     &      dt, coef_exp, coef_adv, coef_src, sph_rj, rj_fld)
      else
        call sel_scl_diff_adv_SGS_src_elr                               &
     &     (iflag_SGS, kr_st, kr_ed, ipol_diffuse, ipol_fil_advect,     &
     &      ipol_SGS_advect, ipol_source, ipol_scalar,                  &
     &      dt, coef_exp, coef_adv, coef_src, sph_rj, rj_fld)
      end if
!
      end subroutine sel_scl_diff_adv_fil_SGS_elr
!
! ----------------------------------------------------------------------
!
      subroutine sel_ini_adams_scl_w_fil_SGS                            &
     &         (iflag_4_filter_advection, iflag_SGS, kr_st, kr_ed,      &
     &          ipol_advect, ipol_fil_advect, ipol_SGS_advect,          &
     &          ipol_source, ipol_pre, coef_src, sph_rj, rj_fld)
!
      use select_SGS_diff_adv_source
!
      type(sph_rj_grid), intent(in) :: sph_rj
      logical, intent(in) :: iflag_4_filter_advection
      integer(kind = kint), intent(in) :: iflag_SGS
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: ipol_advect, ipol_source
      integer(kind = kint), intent(in) :: ipol_fil_advect
      integer(kind = kint), intent(in) :: ipol_SGS_advect
      integer(kind = kint), intent(in) :: ipol_pre
      real(kind = kreal), intent(in) :: coef_src
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(iflag_4_filter_advection .eqv. .FALSE.) then
        call sel_ini_adams_sscl_w_src_SGS(iflag_SGS, kr_st, kr_ed,      &
     &      ipol_advect, ipol_SGS_advect, ipol_source, ipol_pre,        &
     &      coef_src, sph_rj, rj_fld)
      else
        call sel_ini_adams_sscl_w_src_SGS(iflag_SGS, kr_st, kr_ed,      &
     &      ipol_fil_advect, ipol_SGS_advect, ipol_source, ipol_pre,    &
     &      coef_src, sph_rj, rj_fld)
      end if
!
      end subroutine sel_ini_adams_scl_w_fil_SGS
!
! ----------------------------------------------------------------------
!
      end module explicit_scalars_sph_w_SGS
