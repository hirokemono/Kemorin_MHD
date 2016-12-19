!>@file   cal_momentum_eq_explicit.f90
!!@brief  module cal_momentum_eq_explicit
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2010
!
!>@brief Time integration for momentum equation by explicit scheme
!!
!!@verbatim
!!      subroutine cal_expricit_sph_adams(sph_rj, ipol, itor, rj_fld)
!!      subroutine cal_expricit_sph_euler                               &
!!     &         (i_step, sph_rj, ipol, itor, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
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
      use m_control_parameter
!
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_schmidt_poly_on_rtm
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_expricit_sph_adams(sph_rj, ipol, itor, rj_fld)
!
      use m_boundary_params_sph_MHD
      use m_physical_property
      use cal_explicit_terms
      use cal_vorticity_terms_adams
      use cal_nonlinear_sph_MHD
      use select_diff_adv_source
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
!
!$omp parallel
      if(evo_velo%iflag_scheme .gt.     id_no_evolution) then
        call cal_vorticity_eq_adams(ipol, itor,                         &
     &      sph_bc_U%kr_in, sph_bc_U%kr_out, evo_velo%coef_exp,         &
     &      rj_fld%n_point,sph_rj%nidx_rj(2), rj_fld%ntot_phys,         &
     &      rj_fld%d_fld)
      end if
!
      if(evo_magne%iflag_scheme .gt.    id_no_evolution) then
        call cal_diff_induction_MHD_adams(evo_magne%coef_exp,           &
     &      ipol, itor, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(iflag_t_evo_4_temp .gt.     id_no_evolution) then
        call sel_scalar_diff_adv_src_adams                              &
     &     (sph_bc_T%kr_in, sph_bc_T%kr_out,                            &
     &      ipol%i_t_diffuse, ipol%i_h_advect, ipol%i_heat_source,      &
     &      ipol%i_temp, ipol%i_pre_heat, coef_exp_t, coef_h_src,       &
     &      sph_rj, rj_fld)
      end if
      if(evo_comp%iflag_scheme .gt. id_no_evolution) then
        call sel_scalar_diff_adv_src_adams                              &
     &     (sph_bc_C%kr_in, sph_bc_C%kr_out,                            &
     &      ipol%i_c_diffuse, ipol%i_c_advect, ipol%i_light_source,     &
     &      ipol%i_light, ipol%i_pre_composit,                          &
     &      evo_comp%coef_exp, coef_c_src, sph_rj, rj_fld)
      end if
!$omp end parallel
!
      end subroutine cal_expricit_sph_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_expricit_sph_euler                                 &
     &         (i_step, sph_rj, ipol, itor, rj_fld)
!
      use m_boundary_params_sph_MHD
      use m_physical_property
      use cal_explicit_terms
      use cal_vorticity_terms_adams
      use select_diff_adv_source
!
      integer(kind = kint), intent(in) :: i_step
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
!$omp parallel
      if(evo_velo%iflag_scheme .gt.     id_no_evolution) then
        call cal_vorticity_eq_euler(ipol, itor,                         &
     &      sph_bc_U%kr_in, sph_bc_U%kr_out, evo_velo%coef_exp,         &
     &      rj_fld%n_point, sph_rj%nidx_rj(2), rj_fld%ntot_phys,        &
     &      rj_fld%d_fld)
      end if
!
      if(iflag_t_evo_4_temp .gt.     id_no_evolution) then
        call sel_scalar_diff_adv_src_euler                              &
     &     (sph_bc_T%kr_in, sph_bc_T%kr_out,                            &
     &      ipol%i_t_diffuse, ipol%i_h_advect, ipol%i_heat_source,      &
     &      ipol%i_temp, coef_exp_t, coef_temp, coef_h_src,             &
     &      sph_rj, rj_fld)
      end if
      if(evo_magne%iflag_scheme .gt.    id_no_evolution) then
        call cal_diff_induction_MHD_euler(evo_magne%coef_exp,           &
     &      ipol, itor, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(evo_comp%iflag_scheme .gt. id_no_evolution) then
        call sel_scalar_diff_adv_src_euler                              &
     &     (sph_bc_C%kr_in, sph_bc_C%kr_out,                            &
     &      ipol%i_c_diffuse, ipol%i_c_advect, ipol%i_light_source,     &
     &      ipol%i_light, evo_comp%coef_exp, coef_light, coef_c_src,    &
     &      sph_rj, rj_fld)
      end if
!
      if (i_step .eq. 1) then
        if(evo_velo%iflag_scheme .gt.     id_no_evolution) then
          call set_ini_adams_inertia(ipol, itor,                        &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(iflag_t_evo_4_temp .gt.     id_no_evolution) then
          call sel_ini_adams_scalar_w_src                               &
     &       (sph_bc_T%kr_in, sph_bc_T%kr_out, ipol%i_h_advect,         &
     &        ipol%i_heat_source, ipol%i_pre_heat,                      &
     &        coef_h_src, sph_rj, rj_fld)
        end if
        if(evo_magne%iflag_scheme .gt.    id_no_evolution) then
          call set_ini_adams_mag_induct(ipol, itor,                     &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(evo_comp%iflag_scheme .gt. id_no_evolution) then
          call sel_ini_adams_scalar_w_src                               &
     &       (sph_bc_C%kr_in, sph_bc_C%kr_out, ipol%i_c_advect,         &
     &        ipol%i_light_source, ipol%i_pre_composit,                 &
     &        coef_c_src, sph_rj, rj_fld)
        end if
      end if
!$omp end parallel
!
      end subroutine cal_expricit_sph_euler
!
! ----------------------------------------------------------------------
!
      end module cal_momentum_eq_explicit
