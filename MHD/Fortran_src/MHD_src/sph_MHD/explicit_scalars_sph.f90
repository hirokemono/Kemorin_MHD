!>@file   explicit_scalars_sph.f90
!!@brief  module explicit_scalars_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2010
!
!>@brief Time integration for momentum equation by explicit scheme
!!
!!@verbatim
!!      subroutine explicit_temp_sph_adams                              &
!!     &         (dt, sph_params, sph_rj, ht_prop, sph_bc_T,            &
!!     &          ipol_base, ipol_exp, ipol_frc, ipol_dif, rj_fld)
!!      subroutine explicit_comp_sph_adams                              &
!!     &         (dt, sph_params, sph_rj, cp_prop, sph_bc_C,            &
!!     &          ipol_base, ipol_exp, ipol_frc, ipol_dif, rj_fld)
!!
!!      subroutine explicit_temp_sph_euler                              &
!!     &         (dt, sph_rj, ht_prop, sph_bc_T,                        &
!!     &          ipol_base, ipol_frc, ipol_dif, rj_fld)
!!      subroutine explicit_comp_sph_euler                              &
!!     &         (dt, sph_rj, cp_prop, sph_bc_C,                        &
!!     &          ipol_base, ipol_frc, ipol_dif, rj_fld)
!!
!!      subroutine first_temp_prev_step_adams(sph_rj, ht_prop, sph_bc_T,&
!!     &          ipol_base, ipol_exp, ipol_frc, rj_fld)
!!      subroutine first_comp_prev_step_adams(sph_rj, cp_prop, sph_bc_C,&
!!     &          ipol_base, ipol_exp, ipol_frc, rj_fld)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(diffusion_address), intent(in) :: ipol_dif
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param i_step  time step
!
      module explicit_scalars_sph
!
      use m_precision
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_boundary_data_sph_MHD
      use t_scalar_property
      use t_base_field_labels
      use t_base_force_labels
      use t_diffusion_term_labels
      use t_explicit_term_labels
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
      subroutine explicit_temp_sph_adams                                &
     &         (dt, sph_params, sph_rj, ht_prop, sph_bc_T,              &
     &          ipol_base, ipol_exp, ipol_frc, ipol_dif, rj_fld)
!
      use select_diff_adv_source
      use cal_inner_core_rotation
!
      real(kind = kreal), intent(in) :: dt
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: ht_prop
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(base_field_address), intent(in) :: ipol_base
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist, ied
!
      ist = (sph_bc_T%kr_in-1) * sph_rj%nidx_rj(2) + 1
      ied =  sph_bc_T%kr_out *   sph_rj%nidx_rj(2)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                'sel_scalar_diff_adv_src_adams temperature'
      call sel_scalar_diff_adv_src_adams                                &
     &   (ist, ied, sph_rj%inod_rj_center,                              &
     &    ipol_dif%i_t_diffuse, ipol_frc%i_h_advect,                    &
     &    ipol_base%i_heat_source, ipol_base%i_temp,                    &
     &    ipol_exp%i_pre_heat, dt, ht_prop%coef_exp,                    &
     &    ht_prop%coef_source, rj_fld)
!
      end subroutine explicit_temp_sph_adams
!
! ----------------------------------------------------------------------
!
      subroutine explicit_comp_sph_adams                                &
     &         (dt, sph_params, sph_rj, cp_prop, sph_bc_C,              &
     &          ipol_base, ipol_exp, ipol_frc, ipol_dif, rj_fld)
!
      use select_diff_adv_source
      use cal_inner_core_rotation
!
      real(kind = kreal), intent(in) :: dt
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_C
      type(base_field_address), intent(in) :: ipol_base
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist, ied
!
      ist = (sph_bc_C%kr_in-1) * sph_rj%nidx_rj(2) + 1
      ied =  sph_bc_C%kr_out *   sph_rj%nidx_rj(2)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                'sel_scalar_diff_adv_src_adams composition'
      call sel_scalar_diff_adv_src_adams                                &
     &   (ist, ied, sph_rj%inod_rj_center,                              &
     &    ipol_dif%i_c_diffuse, ipol_frc%i_c_advect,                    &
     &    ipol_base%i_light_source, ipol_base%i_light,                  &
     &    ipol_exp%i_pre_composit, dt, cp_prop%coef_exp,                &
     &    cp_prop%coef_source, rj_fld)
!
      end subroutine explicit_comp_sph_adams
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine explicit_temp_sph_euler                                &
     &         (dt, sph_rj, ht_prop, sph_bc_T,                          &
     &          ipol_base, ipol_frc, ipol_dif, rj_fld)
!
      use select_diff_adv_source
!
      real(kind = kreal), intent(in) :: dt
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: ht_prop
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist, ied
!
      ist = (sph_bc_T%kr_in-1) * sph_rj%nidx_rj(2) + 1
      ied =  sph_bc_T%kr_out *   sph_rj%nidx_rj(2)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &              'sel_scalar_diff_adv_src_euler temperature'
      call sel_scalar_diff_adv_src_euler                                &
     &   (ist, ied, sph_rj%inod_rj_center,                              &
     &    ipol_dif%i_t_diffuse, ipol_frc%i_h_advect,                    &
     &    ipol_base%i_heat_source, ipol_base%i_temp,                    &
     &    dt, ht_prop%coef_exp, ht_prop%coef_advect,                    &
     &    ht_prop%coef_source, rj_fld)
!
      end subroutine explicit_temp_sph_euler
!
! ----------------------------------------------------------------------
!
      subroutine explicit_comp_sph_euler                                &
     &         (dt, sph_rj, cp_prop, sph_bc_C,                          &
     &          ipol_base, ipol_frc, ipol_dif, rj_fld)
!
      use select_diff_adv_source
!
      real(kind = kreal), intent(in) :: dt
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_C
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist, ied
!
      ist = (sph_bc_C%kr_in-1) * sph_rj%nidx_rj(2) + 1
      ied =  sph_bc_C%kr_out *   sph_rj%nidx_rj(2)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                'sel_scalar_diff_adv_src_euler composition'
      call sel_scalar_diff_adv_src_euler                                &
     &   (ist, ied, sph_rj%inod_rj_center,                              &
     &    ipol_dif%i_c_diffuse, ipol_frc%i_c_advect,                    &
     &    ipol_base%i_light_source, ipol_base%i_light,                  &
     &    dt, cp_prop%coef_exp, cp_prop%coef_advect,                    &
     &    cp_prop%coef_source, rj_fld)
!
      end subroutine explicit_comp_sph_euler
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine first_temp_prev_step_adams(sph_rj, ht_prop, sph_bc_T,  &
     &          ipol_base, ipol_exp, ipol_frc, rj_fld)
!
      use select_diff_adv_source
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: ht_prop
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(base_field_address), intent(in) :: ipol_base
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist, ied
!
      ist = (sph_bc_T%kr_in-1) * sph_rj%nidx_rj(2) + 1
      ied =  sph_bc_T%kr_out *   sph_rj%nidx_rj(2)
      call sel_ini_adams_scalar_w_src                                   &
     &   (ist, ied, sph_rj%inod_rj_center, ipol_frc%i_h_advect,         &
     &    ipol_base%i_heat_source, ipol_exp%i_pre_heat,                 &
     &    ht_prop%coef_source, rj_fld)
!
      end subroutine first_temp_prev_step_adams
!
! ----------------------------------------------------------------------
!
      subroutine first_comp_prev_step_adams(sph_rj, cp_prop, sph_bc_C,  &
     &          ipol_base, ipol_exp, ipol_frc, rj_fld)
!
      use select_diff_adv_source
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(scalar_property), intent(in) :: cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_C
      type(base_field_address), intent(in) :: ipol_base
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist, ied
!
      ist = (sph_bc_C%kr_in-1) * sph_rj%nidx_rj(2) + 1
      ied =  sph_bc_C%kr_out *   sph_rj%nidx_rj(2)
      call sel_ini_adams_scalar_w_src                                   &
     &   (ist, ied, sph_rj%inod_rj_center, ipol_frc%i_c_advect,         &
     &    ipol_base%i_light_source, ipol_exp%i_pre_composit,            &
     &    cp_prop%coef_source, rj_fld)
!
      end subroutine first_comp_prev_step_adams
!
! ----------------------------------------------------------------------
!
      end module explicit_scalars_sph
