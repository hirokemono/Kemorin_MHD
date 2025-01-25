!>@file   cal_sph_pol_hdiv_vscs_ICB.f90
!!@brief  module cal_sph_pol_hdiv_vscs_ICB
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Substitute viscousity matrix at ICB
!!
!!@verbatim
!!      subroutine sph_exp_FDM2_vpol_viscosity_ICB                      &
!!     &        (sph_rj, fl_prop, radial_variation, sph_bc_U,           &
!!     &         g_sph_rj, coef_p, coef_d, fdm_e1,                      &
!!     &         fdm2_fix_dr_ICB, fdm2_free_ICB,                        &
!!     &         fdm3e_vp0_ICB, fdm3e_free_ICB, d_vpol, press_e,        &
!!     &         mat2_viscous_ICB, hdiv_visous_mat_ICB,                 &
!!     &         d_grad_p, d_viscous_p, hdiv_viscous_e)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        type(fdm_matrices), intent(in) :: fdm_e1
!!        type(fdm2_ICB_free_slip), intent(in) :: fdm2_free_ICB
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!!        type(fdm3_n2e_ICB_free_vpol), intent(in) :: fdm3e_free_ICB
!!        type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
!!        real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat2_viscous_ICB(sph_rj%nidx_rj(2),-1:1)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: hdiv_visous_mat_ICB(sph_rj%nidx_rj(2),-2:1)
!!        real(kind = kreal), intent(inout) :: d_grad_p(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &                    :: d_viscous_p(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: hdiv_viscous_e(sph_rj%nnod_rj)
!!      subroutine sph_exp_FDM4_vpol_viscosity_ICB                      &
!!     &        (sph_rj, fl_prop, radial_variation, sph_bc_U,           &
!!     &         g_sph_rj, coef_p, coef_d, fdm_e3,                      &
!!     &         fdm4_noslip_ICB, fdm4_free_ICB,                        &
!!     &         fdm3e_vp0_ICB, fdm3e_free_ICB, d_vpol, press_e,        &
!!     &         mat4_viscous_ICB, hdiv_visous_mat_ICB,                 &
!!     &         d_grad_p, d_viscous_p, hdiv_viscous_e)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        type(fdm_matrices), intent(in) :: fdm_e3
!!        type(fdm4_ICB_free_vpol), intent(in) :: fdm4_free_ICB
!!        type(fdm4_ICB_zero_vpol), intent(in) :: fdm4_noslip_ICB
!!        type(fdm3_n2e_ICB_free_vpol), intent(in) :: fdm3e_free_ICB
!!        type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
!!        real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat4_viscous_ICB(sph_rj%nidx_rj(2),-2:2)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: hdiv_visous_mat_ICB(sph_rj%nidx_rj(2),-2:1)
!!        real(kind = kreal), intent(inout) :: d_grad_p(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &                    :: d_viscous_p(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: hdiv_viscous_e(sph_rj%nnod_rj)
!!
!!      subroutine sph_FDM2_vpol_viscosity_mat_ICB(sph_rj, fl_prop,     &
!!     &          radial_variation, sph_bc_U, g_sph_rj, coef_d,         &
!!     &          fdm3e_vp0_ICB, fdm3e_free_ICB, hdiv_visous_mat_ICB,   &
!!     &          mat7)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        type(fdm3_n2e_ICB_free_vpol), intent(in) :: fdm3e_free_ICB
!!        type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: hdiv_visous_mat_ICB(sph_rj%nidx_rj(2),-2:1)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat7(7,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!!      subroutine sph_FDM4_vpol_viscosity_mat_ICB                      &
!!     &        (sph_rj, fl_prop, radial_variation, sph_bc_U,           &
!!     &         g_sph_rj, coef_p, coef_d, fdm_3e, fdm_e3,              &
!!     &         fdm4_noslip_ICB, fdm4_free_ICB,                        &
!!     &         fdm3e_vp0_ICB, fdm3e_free_ICB,                         &
!!     &         mat4_viscous_CMB1, hdiv_visous_mat_ICB, mat9)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        type(fdm_matrices), intent(in) :: fdm_3e
!!        type(fdm_matrices), intent(in) :: fdm_e3
!!        type(fdm4_ICB_free_vpol), intent(in) :: fdm4_free_ICB
!!        type(fdm4_ICB_zero_vpol), intent(in) :: fdm4_noslip_ICB
!!        type(fdm3_n2e_ICB_free_vpol), intent(in) :: fdm3e_free_ICB
!!        type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat4_viscous_CMB1(sph_rj%nidx_rj(2),-2:2)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: hdiv_visous_mat_ICB(sph_rj%nidx_rj(2),-2:1)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat9(9,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!!@endverbatim
!!
      module cal_sph_pol_hdiv_vscs_ICB
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_data
      use t_physical_property
      use t_fdm_coefs
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine sph_exp_FDM2_vpol_viscosity_ICB                        &
     &        (sph_rj, fl_prop, radial_variation, sph_bc_U,             &
     &         g_sph_rj, coef_p, coef_d, fdm_e1,                        &
     &         fdm2_fix_dr_ICB, fdm2_free_ICB,                          &
     &         fdm3e_vp0_ICB, fdm3e_free_ICB, d_vpol, press_e,          &
     &         mat2_viscous_ICB, hdiv_visous_mat_ICB,                   &
     &         d_grad_p, d_viscous_p, hdiv_viscous_e)
!
      use t_boundary_params_sph_MHD
      use t_coef_fdm2_free_slip_ICB
      use t_coef_fdm3_n2e_zero_vp_ICB
      use t_coef_fdm3_n2e_free_vp_ICB
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use cal_sph_FDM_viscosity_mat
      use sum_sph_pol_vscs_FDM2_exp
      use sum_sph_pol_grad_p_FDM2_exp
      use sum_sph_hdiv_vscs_FDM_exp
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
      type(sph_boundary_type), intent(in) :: sph_bc_U
!
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
!
      type(fdm_matrices), intent(in) :: fdm_e1
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
      type(fdm2_ICB_free_slip), intent(in) :: fdm2_free_ICB
      type(fdm3_n2e_ICB_free_vpol), intent(in) :: fdm3e_free_ICB
      type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
      real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat2_viscous_ICB(sph_rj%nidx_rj(2),-1:1)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat_ICB(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout) :: d_grad_p(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout)                                 &
     &                    :: d_viscous_p(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout)                                 &
     &                    :: hdiv_viscous_e(sph_rj%nnod_rj)
!
      real(kind = kreal) :: mat1_grad_p_ICB(sph_rj%nidx_rj(2),0:1)
!
      integer(kind = kint) :: kr
!
!
      call set_sph_FDM_pressure_grad_mat                                &
     &   (fdm_e1%n_minus, fdm_e1%n_plus, sph_rj%nidx_rj(2),             &
     &    sph_rj%radius_1d_rj_r(sph_bc_U%kr_in), g_sph_rj, coef_p,      &
     &    fdm_e1%dmat(fdm_e1%n_minus,sph_bc_U%kr_in,1),                 &
     &    mat1_grad_p_ICB)
      call sum_exp2_sph_pol_grad_p_ICB                                  &
     &   (sph_bc_U%kr_in, sph_rj%nnod_rj, sph_rj%nidx_rj(2),            &
     &    press_e, mat1_grad_p_ICB(1,1), d_grad_p)
!
      kr = sph_bc_U%kr_in
      if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call set_sph_FDM_viscosity_mat(izero, ione, sph_rj%nidx_rj(2),  &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%ar_1d_rj(kr,1), sph_rj%ar_1d_rj(kr,2),               &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_nu),                 &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_dnu_norm),           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_drho_norm),          &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_d2rho_norm),         &
     &      fdm2_free_ICB%dmat_vp( 0,2),                                &
     &      fdm2_free_ICB%dmat_vp( 0,3), mat2_viscous_ICB)
      else
        call set_sph_FDM_viscosity_mat(izero, ione, sph_rj%nidx_rj(2),  &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%ar_1d_rj(kr,1), sph_rj%ar_1d_rj(kr,2),               &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_nu),                 &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_dnu_norm),           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_drho_norm),          &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_d2rho_norm),         &
     &      fdm2_fix_dr_ICB( 0,2), fdm2_fix_dr_ICB(-1,3),               &
     &      mat2_viscous_ICB)
      end if
      call sum_exp2_sph_pol_viscous_ICB                                 &
     &   (sph_bc_U%kr_in, sph_rj%nnod_rj, sph_rj%nidx_rj(2),            &
     &    d_vpol, mat2_viscous_ICB(1,0), d_viscous_p)
!
!
      kr = sph_bc_U%kr_in+1
      if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call set_sph_FDM_hdiv_viscosity_mat(-ione, ione,                &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                  &
     &      sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3),             &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr,fl_prop%ir_nu),                   &
     &      radial_variation%d_fld(kr,fl_prop%ir_dnu_norm),             &
     &      radial_variation%d_fld(kr,fl_prop%ir_drho_norm),            &
     &      radial_variation%d_fld(kr,fl_prop%ir_d2rho_norm),           &
     &      fdm3e_free_ICB%dmat_vp0(-1,1),                              &
     &      fdm3e_free_ICB%dmat_vp0(-1,2),                              &
     &      fdm3e_free_ICB%dmat_vp0(-1,3),                              &
     &      fdm3e_free_ICB%dmat_vp0(-1,4), hdiv_visous_mat_ICB)
      else
        call set_sph_FDM_hdiv_viscosity_mat(-ione, ione,                &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                  &
     &      sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3),             &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr,fl_prop%ir_nu),                   &
     &      radial_variation%d_fld(kr,fl_prop%ir_dnu_norm),             &
     &      radial_variation%d_fld(kr,fl_prop%ir_drho_norm),            &
     &      radial_variation%d_fld(kr,fl_prop%ir_d2rho_norm),           &
     &      fdm3e_vp0_ICB%dmat_vp0(-1,1), fdm3e_vp0_ICB%dmat_vp0(-1,2), &
     &      fdm3e_vp0_ICB%dmat_vp0(-1,3), fdm3e_vp0_ICB%dmat_vp0(-1,4), &
     &      hdiv_visous_mat_ICB)
      end if
      call sum_exp_sph_hdiv_viscous_ICB((sph_bc_U%kr_in+1),             &
     &    sph_rj%nnod_rj, sph_rj%nidx_rj(2), d_vpol,                    &
     &    hdiv_visous_mat_ICB, hdiv_viscous_e)
!
!
      end subroutine sph_exp_FDM2_vpol_viscosity_ICB
!
!  -------------------------------------------------------------------
!
      subroutine sph_exp_FDM4_vpol_viscosity_ICB                        &
     &        (sph_rj, fl_prop, radial_variation, sph_bc_U,             &
     &         g_sph_rj, coef_p, coef_d, fdm_e3,                        &
     &         fdm4_noslip_ICB, fdm4_free_ICB,                          &
     &         fdm3e_vp0_ICB, fdm3e_free_ICB, d_vpol, press_e,          &
     &         mat4_viscous_ICB, hdiv_visous_mat_ICB,                   &
     &         d_grad_p, d_viscous_p, hdiv_viscous_e)
!
      use t_boundary_params_sph_MHD
      use t_coef_fdm3_n2e_zero_vp_ICB
      use t_coef_fdm3_n2e_free_vp_ICB
      use t_coef_fdm4_zero_vpol_ICB
      use t_coef_fdm4_free_vpol_ICB
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use cal_sph_FDM_viscosity_mat
      use sum_sph_pol_vscs_FDM4_exp
      use sum_sph_pol_grad_p_FDM4_exp
      use sum_sph_hdiv_vscs_FDM_exp
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
      type(sph_boundary_type), intent(in) :: sph_bc_U
!
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
!
      type(fdm_matrices), intent(in) :: fdm_e3
      type(fdm4_ICB_free_vpol), intent(in) :: fdm4_free_ICB
      type(fdm4_ICB_zero_vpol), intent(in) :: fdm4_noslip_ICB
      type(fdm3_n2e_ICB_free_vpol), intent(in) :: fdm3e_free_ICB
      type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
      real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat4_viscous_ICB(sph_rj%nidx_rj(2),-2:2)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat_ICB(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout) :: d_grad_p(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout)                                 &
     &                    :: d_viscous_p(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_viscous_e(sph_rj%nnod_rj)
!
      real(kind = kreal) :: mat3_grad_p_ICB(sph_rj%nidx_rj(2),-1:2)
!
      integer(kind = kint) :: kr
!
!
      call set_sph_FDM_pressure_grad_mat                                &
     &   (izero, fdm_e3%n_plus, sph_rj%nidx_rj(2),                      &
     &    sph_rj%radius_1d_rj_r(sph_bc_U%kr_in), g_sph_rj, coef_p,      &
     &    fdm_e3%dmat(0,sph_bc_U%kr_in,1), mat3_grad_p_ICB)
      call sum_exp4_sph_pol_grad_p_ICB(sph_bc_U%kr_in,                  &
     &    sph_rj%nnod_rj, sph_rj%nidx_rj(2), press_e,                   &
     &    mat3_grad_p_ICB(1,1), d_grad_p)
!
      call set_sph_FDM_pressure_grad_mat                                &
     &   (-ione, fdm_e3%n_plus, sph_rj%nidx_rj(2),                      &
     &    sph_rj%radius_1d_rj_r(sph_bc_U%kr_in+1), g_sph_rj, coef_p,    &
     &    fdm_e3%dmat(-1,sph_bc_U%kr_in+1,1), mat3_grad_p_ICB)
      call sum_exp4_sph_pol_grad_p_ICB1((sph_bc_U%kr_in+1),             &
     &    sph_rj%nnod_rj, sph_rj%nidx_rj(2), press_e,                   &
     &    mat3_grad_p_ICB(1,0), d_grad_p)
!
      kr = sph_bc_U%kr_in
      if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call set_sph_FDM_viscosity_mat(izero, itwo, sph_rj%nidx_rj(2),  &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%ar_1d_rj(kr,1), sph_rj%ar_1d_rj(kr,2),               &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_nu),                 &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_dnu_norm),           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_drho_norm),          &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_d2rho_norm),         &
     &      fdm4_free_ICB%dmat_vp0(0,2),                                &
     &      fdm4_free_ICB%dmat_vp0(0,3), mat4_viscous_ICB)
      else
        call set_sph_FDM_viscosity_mat(izero, ione, sph_rj%nidx_rj(2),  &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%ar_1d_rj(kr,1), sph_rj%ar_1d_rj(kr,2),               &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_nu),                 &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_dnu_norm),           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_drho_norm),          &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_d2rho_norm),         &
     &      fdm4_noslip_ICB%dmat_vp0(0,2),                              &
     &      fdm4_noslip_ICB%dmat_vp0(0,3), mat4_viscous_ICB)
      end if
      call sum_exp4_sph_pol_viscous_ICB(sph_bc_U%kr_in,                 &
     &    sph_rj%nnod_rj, sph_rj%nidx_rj(2), d_vpol,                    &
     &    mat4_viscous_ICB(1,0), d_viscous_p)
!
!
      kr = sph_bc_U%kr_in + 1
      if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call set_sph_FDM_hdiv_viscosity_mat(-ione, ione,                &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                  &
     &      sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3),             &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr,fl_prop%ir_nu),                   &
     &      radial_variation%d_fld(kr,fl_prop%ir_dnu_norm),             &
     &      radial_variation%d_fld(kr,fl_prop%ir_drho_norm),            &
     &      radial_variation%d_fld(kr,fl_prop%ir_d2rho_norm),           &
     &      fdm3e_free_ICB%dmat_vp0(-1,1),                              &
     &      fdm3e_free_ICB%dmat_vp0(-1,2),                              &
     &      fdm3e_free_ICB%dmat_vp0(-1,3),                              &
     &      fdm3e_free_ICB%dmat_vp0(-1,4), hdiv_visous_mat_ICB)
      else
        call set_sph_FDM_hdiv_viscosity_mat(-ione, ione,                &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                  &
     &      sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3),             &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr,fl_prop%ir_nu),                   &
     &      radial_variation%d_fld(kr,fl_prop%ir_dnu_norm),             &
     &      radial_variation%d_fld(kr,fl_prop%ir_drho_norm),            &
     &      radial_variation%d_fld(kr,fl_prop%ir_d2rho_norm),           &
     &      fdm3e_vp0_ICB%dmat_vp0(-1,1), fdm3e_vp0_ICB%dmat_vp0(-1,2), &
     &      fdm3e_vp0_ICB%dmat_vp0(-1,3), fdm3e_vp0_ICB%dmat_vp0(-1,4), &
     &      hdiv_visous_mat_ICB)
      end if
      call sum_exp_sph_hdiv_viscous_ICB((sph_bc_U%kr_in+1),             &
     &    sph_rj%nnod_rj, sph_rj%nidx_rj(2), d_vpol,                    &
     &    hdiv_visous_mat_ICB, hdiv_viscous_e)
!
!
      kr = sph_bc_U%kr_in + 1
      if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call set_sph_FDM_viscosity_mat(-ione, itwo, sph_rj%nidx_rj(2),  &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%ar_1d_rj(kr,1), sph_rj%ar_1d_rj(kr,2),               &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_nu),                 &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_dnu_norm),           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_drho_norm),          &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_d2rho_norm),         &
     &      fdm4_free_ICB%dmat_vp1(-1,2),                               &
     &      fdm4_free_ICB%dmat_vp1(-1,3), mat4_viscous_ICB)
      else
        call set_sph_FDM_viscosity_mat(-ione, itwo, sph_rj%nidx_rj(2),  &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%ar_1d_rj(kr,1), sph_rj%ar_1d_rj(kr,2),               &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_nu),                 &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_dnu_norm),           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_drho_norm),          &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_d2rho_norm),         &
     &      fdm4_noslip_ICB%dmat_vp1(-1,2),                             &
     &      fdm4_noslip_ICB%dmat_vp1(-1,3), mat4_viscous_ICB)
      end if
      call sum_exp4_sph_pol_viscous_ICB1((sph_bc_U%kr_in+1),            &
     &    sph_rj%nnod_rj, sph_rj%nidx_rj(2), d_vpol,                    &
     &    mat4_viscous_ICB(1,-1), d_viscous_p)
!
      end subroutine sph_exp_FDM4_vpol_viscosity_ICB
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine sph_FDM2_vpol_viscosity_mat_ICB(sph_rj, fl_prop,       &
     &          radial_variation, sph_bc_U, g_sph_rj, coef_d,           &
     &          fdm3e_vp0_ICB, fdm3e_free_ICB, hdiv_visous_mat_ICB,     &
     &          mat7)
!
      use t_boundary_params_sph_MHD
      use t_coef_fdm3_n2e_zero_vp_ICB
      use t_coef_fdm3_n2e_free_vp_ICB
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use set_sph_pol_vscs_FDM2_mat
      use set_sph_hdiv_vscs_FDM_mat7
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
      type(sph_boundary_type), intent(in) :: sph_bc_U
!
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
!
      type(fdm3_n2e_ICB_free_vpol), intent(in) :: fdm3e_free_ICB
      type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
!
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat_ICB(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: kr
!
!
      do kr = 1, sph_bc_U%kr_in
        call set_sph_pol_viscous_mat7_ICB                               &
     &     (kr, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), mat7)
      end do
!
      call set_sph_ele_pressure_FDM_mat7((sph_bc_U%kr_in+1),            &
     &    sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), fl_prop%coef_press,     &
     &    mat7)
!
      kr = sph_bc_U%kr_in+1
      if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call set_sph_FDM_hdiv_viscosity_mat(-ione, ione,                &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                  &
     &      sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3),             &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr,fl_prop%ir_nu),                   &
     &      radial_variation%d_fld(kr,fl_prop%ir_dnu_norm),             &
     &      radial_variation%d_fld(kr,fl_prop%ir_drho_norm),            &
     &      radial_variation%d_fld(kr,fl_prop%ir_d2rho_norm),           &
     &      fdm3e_free_ICB%dmat_vp0(-1,1),                              &
     &      fdm3e_free_ICB%dmat_vp0(-1,2),                              &
     &      fdm3e_free_ICB%dmat_vp0(-1,3),                              &
     &      fdm3e_free_ICB%dmat_vp0(-1,4), hdiv_visous_mat_ICB(1,-1))
      else
        call set_sph_FDM_hdiv_viscosity_mat(-ione, ione,                &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                  &
     &      sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3),             &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr,fl_prop%ir_nu),                   &
     &      radial_variation%d_fld(kr,fl_prop%ir_dnu_norm),             &
     &      radial_variation%d_fld(kr,fl_prop%ir_drho_norm),            &
     &      radial_variation%d_fld(kr,fl_prop%ir_d2rho_norm),           &
     &      fdm3e_vp0_ICB%dmat_vp0(-1,1), fdm3e_vp0_ICB%dmat_vp0(-1,2), &
     &      fdm3e_vp0_ICB%dmat_vp0(-1,3), fdm3e_vp0_ICB%dmat_vp0(-1,4), &
     &      hdiv_visous_mat_ICB(1,-1))
      end if
      call sub_sph_hdiv_viscous_mat7_ICB1((sph_bc_U%kr_in+1),           &
     &    sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    hdiv_visous_mat_ICB(1,-1), mat7)
!
      end subroutine sph_FDM2_vpol_viscosity_mat_ICB
!
!  -------------------------------------------------------------------
!
      subroutine sph_FDM4_vpol_viscosity_mat_ICB                        &
     &        (sph_rj, fl_prop, radial_variation, sph_bc_U,             &
     &         g_sph_rj, coef_p, coef_d, fdm_3e, fdm_e3,                &
     &         fdm4_noslip_ICB, fdm4_free_ICB,                          &
     &         fdm3e_vp0_ICB, fdm3e_free_ICB,                           &
     &         mat4_viscous_CMB1, hdiv_visous_mat_ICB, mat9)
!
      use t_boundary_params_sph_MHD
      use t_coef_fdm3_n2e_zero_vp_ICB
      use t_coef_fdm3_n2e_free_vp_ICB
      use t_coef_fdm4_zero_vpol_ICB
      use t_coef_fdm4_free_vpol_ICB
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use cal_sph_FDM_viscosity_mat
      use set_sph_pol_vscs_FDM4_mat
      use set_sph_hdiv_vscs_FDM_mat9
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
      type(sph_boundary_type), intent(in) :: sph_bc_U
!
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
!
      type(fdm_matrices), intent(in) :: fdm_3e
      type(fdm_matrices), intent(in) :: fdm_e3
      type(fdm4_ICB_free_vpol), intent(in) :: fdm4_free_ICB
      type(fdm4_ICB_zero_vpol), intent(in) :: fdm4_noslip_ICB
      type(fdm3_n2e_ICB_free_vpol), intent(in) :: fdm3e_free_ICB
      type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat4_viscous_CMB1(sph_rj%nidx_rj(2),-2:2)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat_ICB(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat9(9,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!
      real(kind = kreal) :: mat3_grad_p_ICB(sph_rj%nidx_rj(2),-1:2)
!
      integer(kind = kint) :: kr
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
      else
        call set_sph_hdiv_viscous_mat9_ICB                              &
     &     (sph_bc_U%kr_in, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), mat9)
        do kr = 1, sph_bc_U%kr_in
          call set_sph_pol_viscous_mat9_ICB                             &
     &       (kr, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), mat9)
        end do
      end if
!
!
      kr = sph_bc_U%kr_in+1
      if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call set_sph_FDM_hdiv_viscosity_mat(-itwo, ione,                &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                  &
     &      sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3),             &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr,fl_prop%ir_nu),                   &
     &      radial_variation%d_fld(kr,fl_prop%ir_dnu_norm),             &
     &      radial_variation%d_fld(kr,fl_prop%ir_drho_norm),            &
     &      radial_variation%d_fld(kr,fl_prop%ir_d2rho_norm),           &
     &      fdm3e_free_ICB%dmat_vp0(-2,1),                              &
     &      fdm3e_free_ICB%dmat_vp0(-2,2),                              &
     &      fdm3e_free_ICB%dmat_vp0(-2,3),                              &
     &      fdm3e_free_ICB%dmat_vp0(-2,4), hdiv_visous_mat_ICB)
      else
        call set_sph_FDM_hdiv_viscosity_mat(-itwo, ione,                &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                  &
     &      sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3),             &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr,fl_prop%ir_nu),                   &
     &      radial_variation%d_fld(kr,fl_prop%ir_dnu_norm),             &
     &      radial_variation%d_fld(kr,fl_prop%ir_drho_norm),            &
     &      radial_variation%d_fld(kr,fl_prop%ir_d2rho_norm),           &
     &      fdm3e_vp0_ICB%dmat_vp0(-2,1), fdm3e_vp0_ICB%dmat_vp0(-2,2), &
     &      fdm3e_vp0_ICB%dmat_vp0(-2,3), fdm3e_vp0_ICB%dmat_vp0(-2,4), &
     &      hdiv_visous_mat_ICB)
      end if
      call add_sph_ele_pressure_FDM_mat9((sph_bc_U%kr_in+1),            &
     &    sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), coef_p, mat9)
      call sub_sph_hdiv_viscous_mat9_ICB1((sph_bc_U%kr_in+1),           &
     &    sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    hdiv_visous_mat_ICB(1,-1), mat9)
!
!
      call set_sph_FDM_pressure_grad_mat                                &
     &   (-ione, fdm_e3%n_plus, sph_rj%nidx_rj(2),                      &
     &    sph_rj%radius_1d_rj_r(sph_bc_U%kr_in+1), g_sph_rj, coef_p,    &
     &    fdm_e3%dmat(-1,sph_bc_U%kr_in+1,1), mat3_grad_p_ICB)
!
      kr = sph_bc_U%kr_in + 1
      if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call set_sph_FDM_viscosity_mat(-ione, itwo, sph_rj%nidx_rj(2),  &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%ar_1d_rj(kr,1), sph_rj%ar_1d_rj(kr,2),               &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_nu),                 &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_dnu_norm),           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_drho_norm),          &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_d2rho_norm),         &
     &      fdm4_free_ICB%dmat_vp1(-1,2),                               &
     &      fdm4_free_ICB%dmat_vp1(-1,3), mat4_viscous_CMB1)
      else
        call set_sph_FDM_viscosity_mat(-ione, itwo, sph_rj%nidx_rj(2),  &
     &      fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%ar_1d_rj(kr,1), sph_rj%ar_1d_rj(kr,2),               &
     &      g_sph_rj, coef_d,                                           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_nu),                 &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_dnu_norm),           &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_drho_norm),          &
     &      radial_variation%d_fld(kr+1,fl_prop%ir_d2rho_norm),         &
     &      fdm4_noslip_ICB%dmat_vp1(-1,2),                             &
     &      fdm4_noslip_ICB%dmat_vp1(-1,3), mat4_viscous_CMB1)
      end if
      call sub_sph_pol_viscous_mat9_ICB1((sph_bc_U%kr_in+1),            &
     &    sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    mat3_grad_p_ICB(1,0), mat4_viscous_CMB1(1,-1), mat9)
!
      kr = sph_bc_U%kr_in+2
      call set_sph_FDM_hdiv_viscosity_mat                               &
     &   (fdm_e3%n_minus, fdm_e3%n_plus,                                &
     &    fl_prop%flag_viscous_variation,                               &
     &    fl_prop%flag_ref_density_valiation,                           &
     &    sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                    &
     &    sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3),               &
     &    g_sph_rj, coef_d,                                             &
     &    radial_variation%d_fld(kr,fl_prop%ir_nu),                     &
     &    radial_variation%d_fld(kr,fl_prop%ir_dnu_norm),               &
     &    radial_variation%d_fld(kr,fl_prop%ir_drho_norm),              &
     &    radial_variation%d_fld(kr,fl_prop%ir_d2rho_norm),             &
     &    fdm_3e%dmat(-fdm_e3%n_minus,kr,0),                            &
     &    fdm_3e%dmat(-fdm_e3%n_minus,kr,1),                            &
     &    fdm_3e%dmat(-fdm_e3%n_minus,kr,2),                            &
     &    fdm_3e%dmat(-fdm_e3%n_minus,kr,3), hdiv_visous_mat_ICB)
      call sub_sph_hdiv_viscous_mat9_ICB1(sph_bc_U%kr_in,               &
     &    sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    hdiv_visous_mat_ICB(1,-1), mat9)
!
      end subroutine sph_FDM4_vpol_viscosity_mat_ICB
!
!  -------------------------------------------------------------------
!
      end module cal_sph_pol_hdiv_vscs_ICB
