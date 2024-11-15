!>@file   set_sph_pol_hdiv_viscs_CTR.f90
!!@brief  module set_sph_pol_hdiv_viscs_CTR
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set FDM matrix and explicit horizontal diffusivity
!!      for Valuable density
!!
!!@verbatim
!!      subroutine sph_exp_FDM2_vpol_viscosity_CTR                      &
!!     &        (sph_rj, fl_prop, radial_variation,                     &
!!     &         g_sph_rj, coef_p, coef_d, fdm_e1,                      &
!!     &         fdm2_center, fdm3e_center, d_vpol, press_e,            &
!!     &         mat2_viscous_CTR, hdiv_visous_mat_CTR,                 &
!!     &         d_viscous_p, hdiv_viscous_e)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        type(fdm_matrix), intent(in) :: fdm_e1(0:1)
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(fdm3e_BC_hdiv), intent(in) :: fdm3e_center
!!        real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat2_viscous_CTR(sph_rj%nidx_rj(2),-1:1)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!!        real(kind = kreal), intent(inout)                             &
!!     &                    :: d_viscous_p(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &                    :: hdiv_viscous_e(sph_rj%nnod_rj)
!!      subroutine sph_exp_FDM4_vpol_viscosity_ICB                      &
!!     &        (sph_rj, fl_prop, radial_variation,                     &
!!     &         g_sph_rj, coef_p, coef_d, fdm_e3,                      &
!!     &         fdm4_pol_CTR, fdm3e_center, d_vpol, press_e,           &
!!     &         mat4_viscous_CTR, hdiv_visous_mat_CTR,                 &
!!     &         d_viscous_p, hdiv_viscous_e)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        type(fdm_matrix), intent(in) :: fdm_e3(0:1)
!!        type(fdm4_centre_vpol), intent(in) :: fdm4_pol_CTR
!!        type(fdm3e_BC_hdiv), intent(in) :: fdm3e_center
!!        real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat4_viscous_CTR(sph_rj%nidx_rj(2),-2:2)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: d_viscous_p(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: hdiv_viscous_e(sph_rj%nnod_rj)
!!
!!      subroutine sph_FDM2_vpol_viscosity_mat_ICB                      &
!!     &        (sph_rj, fl_prop, radial_variation,  g_sph_rj,          &
!!     &         coef_p, coef_d, fdm_e1, fdm2_pol_CTR, fdm3e_center,    &
!!     &         mat2_viscous_CMB1, hdiv_visous_mat_CTR, mat7)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        type(fdm_matrix), intent(in) :: fdm_e1(0:1)
!!        type(fdm3e_BC_hdiv), intent(in) :: fdm3e_center
!!        type(fdm2_center_mat), intent(in) :: fdm2_pol_CTR
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat2_viscous_CMB1(sph_rj%nidx_rj(2),-1:1)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat7(7,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!!      subroutine sph_FDM4_vpol_viscosity_mat_CMB                      &
!!     &        (sph_rj, fl_prop, radial_variation,                     &
!!     &         g_sph_rj, coef_p, coef_d, fdm_e3,                      &
!!     &         fdm4_pol_CTR, fdm3e_center, fdm3e_center1,             &
!!     &         mat4_viscous_CMB1, hdiv_visous_mat_CTR, mat9)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        type(fdm_matrix), intent(in) :: fdm_e3(0:1)
!!        type(fdm4_centre_vpol), intent(in) :: fdm4_pol_CTR
!!        type(fdm3e_BC_hdiv), intent(in) :: fdm3e_center
!!        type(fdm3e_BC_hdiv), intent(in) :: fdm3e_center1
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat4_viscous_CMB1(sph_rj%nidx_rj(2),-2:2)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!!      real(kind = kreal), intent(inout)                               &
!!     &           :: mat9(9,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!!@endverbatim
!
      module set_sph_pol_hdiv_viscs_CTR
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sph_exp_FDM2_vpol_viscosity_CTR                        &
     &        (sph_rj, fl_prop, radial_variation,                       &
     &         g_sph_rj, coef_p, coef_d, fdm_e1,                        &
     &         fdm2_center, fdm3e_center, d_vpol, press_e,              &
     &         mat2_viscous_CTR, hdiv_visous_mat_CTR,                   &
     &         d_viscous_p, hdiv_viscous_e)
!
      use t_coef_fdm2_MHD_boundaries
      use t_coef_fdm3e_MHD_boundaries
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use set_sph_pol_viscousity
      use set_sph_hdiv_viscousity
      use set_sph_pol_viscous_CTR
      use set_sph_hdiv_viscous_CTR
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
!
      type(fdm_matrix), intent(in) :: fdm_e1(0:1)
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(fdm3e_BC_hdiv), intent(in) :: fdm3e_center
      real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat2_viscous_CTR(sph_rj%nidx_rj(2),-1:1)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &                    :: d_viscous_p(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout)                                 &
     &                    :: hdiv_viscous_e(sph_rj%nnod_rj)
!
      real(kind = kreal) :: mat3_grad_p_CTR(-1:2)
!
!
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (ione, izero, ione, sph_rj, fl_prop,                         &
     &      radial_variation, g_sph_rj, coef_d, ione,                   &
     &      fdm3e_center%dmat_vp0(-2,1),                                &
     &      fdm3e_center%dmat_vp0(-2,2),                                &
     &      fdm3e_center%dmat_vp0(-2,3),                                &
     &      fdm3e_center%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,0))
      call add_exp_sph_hdiv_viscous_CTR                                 &
     &   (sph_rj%nnod_rj, sph_rj%nidx_rj(2),                            &
     &    coef_p, d_vpol, hdiv_visous_mat_CTR(1,0), hdiv_viscous_e)
!
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (itwo, -ione, ione, sph_rj, fl_prop,                         &
     &      radial_variation, g_sph_rj, coef_d, ione,                   &
     &      fdm3e_center%dmat_vp0(-2,1),                                &
     &      fdm3e_center%dmat_vp0(-2,2),                                &
     &      fdm3e_center%dmat_vp0(-2,3),                                &
     &      fdm3e_center%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,-1))
      call add_exp_sph_hdiv_viscous_CTR1                                &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), coef_p,                 &
     &    hdiv_visous_mat_CTR(1,-1), d_vpol, press_e, hdiv_viscous_e)
!
      call sph_FDM_layer_p_grad_mat(izero, itwo, ione, coef_p,          &
     &    fdm_e1(1)%nri_mat, fdm_e1(1)%dmat, mat3_grad_p_CTR)
      call set_sph_FDM_viscosity_mat(-ione, itwo, itwo,                 &
     &    sph_rj, fl_prop, radial_variation, g_sph_rj, coef_d,          &
     &    ione, fdm2_center%dmat_fix_dr(-1,2),                          &
     &    fdm2_center%dmat_fix_dr(-1,3), mat2_viscous_CTR)
      call add_exp2_sph_viscous_CTR1(sph_rj%nnod_rj, sph_rj%nidx_rj(2), &
     &    d_vpol, press_e, mat3_grad_p_CTR(0), mat2_viscous_CTR(1,0),   &
     &    d_viscous_p)
!
      end subroutine sph_exp_FDM2_vpol_viscosity_CTR
!
!  -------------------------------------------------------------------
!
      subroutine sph_exp_FDM4_vpol_viscosity_ICB                        &
     &        (sph_rj, fl_prop, radial_variation,                       &
     &         g_sph_rj, coef_p, coef_d, fdm_e3,                        &
     &         fdm4_pol_CTR, fdm3e_center, d_vpol, press_e,             &
     &         mat4_viscous_CTR, hdiv_visous_mat_CTR,                   &
     &         d_viscous_p, hdiv_viscous_e)
!
      use t_coef_fdm4_MHD_boundaries
      use t_coef_fdm3e_MHD_boundaries
      use t_coef_fdm4_vpol_centre
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use set_sph_pol_viscousity
      use set_sph_hdiv_viscousity
      use set_sph_pol_viscous_CTR
      use set_sph_hdiv_viscous_CTR
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
!
      type(fdm_matrix), intent(in) :: fdm_e3(0:1)
      type(fdm4_centre_vpol), intent(in) :: fdm4_pol_CTR
      type(fdm3e_BC_hdiv), intent(in) :: fdm3e_center
      real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat4_viscous_CTR(sph_rj%nidx_rj(2),-2:2)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_viscous_p(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_viscous_e(sph_rj%nnod_rj)
!
      real(kind = kreal) :: mat3_grad_p_CTR(-1:2)
!
!
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (ione, izero, ione, sph_rj, fl_prop,                         &
     &      radial_variation, g_sph_rj, coef_d, ione,                   &
     &      fdm3e_center%dmat_vp0(-2,1),                                &
     &      fdm3e_center%dmat_vp0(-2,2),                                &
     &      fdm3e_center%dmat_vp0(-2,3),                                &
     &      fdm3e_center%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,0))
      call add_exp_sph_hdiv_viscous_CTR                                 &
     &   (sph_rj%nnod_rj, sph_rj%nidx_rj(2),                            &
     &    coef_p, d_vpol, hdiv_visous_mat_CTR(1,0), hdiv_viscous_e)
!
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (itwo, -ione, ione, sph_rj, fl_prop,                         &
     &      radial_variation, g_sph_rj, coef_d, ione,                   &
     &      fdm3e_center%dmat_vp0(-2,1),                                &
     &      fdm3e_center%dmat_vp0(-2,2),                                &
     &      fdm3e_center%dmat_vp0(-2,3),                                &
     &      fdm3e_center%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,-1))
      call add_exp_sph_hdiv_viscous_CTR1                                &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), coef_p,                 &
     &    hdiv_visous_mat_CTR(1,-1), d_vpol, press_e, hdiv_viscous_e)
!
      call sph_FDM_layer_p_grad_mat(izero, itwo, ione, coef_p,          &
     &    fdm_e3(1)%nri_mat, fdm_e3(1)%dmat, mat3_grad_p_CTR)
      call set_sph_FDM_viscosity_mat(-ione, itwo, itwo,                 &
     &    sph_rj, fl_prop, radial_variation, g_sph_rj, coef_d,          &
     &    ione, fdm4_pol_CTR%dmat_vp1(-2,2),                            &
     &    fdm4_pol_CTR%dmat_vp1(-2,3), mat4_viscous_CTR)
      call add_exp4_sph_viscous_CTR1                                    &
     &   (sph_rj%nnod_rj, sph_rj%nidx_rj(2), d_vpol, press_e,           &
     &    mat3_grad_p_CTR(0), mat4_viscous_CTR(1,0), d_viscous_p)
!
      call sph_FDM_layer_p_grad_mat(-ione, itwo, itwo, coef_p,          &
     &    fdm_e3(1)%nri_mat, fdm_e3(1)%dmat, mat3_grad_p_CTR)
      call set_sph_FDM_viscosity_mat(-ione, itwo, itwo,                 &
     &    sph_rj, fl_prop, radial_variation, g_sph_rj, coef_d,          &
     &    ione, fdm4_pol_CTR%dmat_vp1(-2,2),                            &
     &    fdm4_pol_CTR%dmat_vp1(-2,3), mat4_viscous_CTR)
      call add_exp4_sph_viscous_CTR2                                    &
     &   (sph_rj%nnod_rj, sph_rj%nidx_rj(2), d_vpol, press_e,           &
     &    mat3_grad_p_CTR(-1), mat4_viscous_CTR(1,-1), d_viscous_p)
!
      end subroutine sph_exp_FDM4_vpol_viscosity_ICB
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine sph_FDM2_vpol_viscosity_mat_ICB                        &
     &        (sph_rj, fl_prop, radial_variation,  g_sph_rj,  &
     &         coef_p, coef_d, fdm_e1, fdm2_pol_CTR, fdm3e_center,      &
     &         mat2_viscous_CMB1, hdiv_visous_mat_CTR, mat7)
!
      use t_coef_fdm3e_MHD_boundaries
      use t_coef_fdm2_MHD_boundaries
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use set_sph_pol_viscousity
      use set_sph_hdiv_viscousity
      use set_sph_pol_viscous_CTR
      use set_sph_hdiv_viscous_CTR
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
!
      type(fdm_matrix), intent(in) :: fdm_e1(0:1)
      type(fdm3e_BC_hdiv), intent(in) :: fdm3e_center
      type(fdm2_center_mat), intent(in) :: fdm2_pol_CTR
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat2_viscous_CMB1(sph_rj%nidx_rj(2),-1:1)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!
      real(kind = kreal) :: mat3_grad_p_CTR(-1:2)
!
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (ione, izero, ione, sph_rj, fl_prop,                         &
     &      radial_variation, g_sph_rj, coef_d, ione,                   &
     &      fdm3e_center%dmat_vp0(-2,1),                                &
     &      fdm3e_center%dmat_vp0(-2,2),                                &
     &      fdm3e_center%dmat_vp0(-2,3),                                &
     &      fdm3e_center%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,0))
      call sub_sph_hdiv_viscous_mat7_CTR                                &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    coef_p, hdiv_visous_mat_CTR(1,0), mat7)
!
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (itwo, -ione, ione, sph_rj, fl_prop,                         &
     &      radial_variation, g_sph_rj, coef_d, ione,                   &
     &      fdm3e_center%dmat_vp0(-2,1),                                &
     &      fdm3e_center%dmat_vp0(-2,2),                                &
     &      fdm3e_center%dmat_vp0(-2,3),                                &
     &      fdm3e_center%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,-1))
      call sub_sph_hdiv_viscous_mat7_CTR1                               &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), coef_p,                 &
     &    hdiv_visous_mat_CTR(1,-1), mat7)
!
      call sph_FDM_layer_p_grad_mat(izero, itwo, ione, coef_p,          &
     &    fdm_e1(1)%nri_mat, fdm_e1(1)%dmat, mat3_grad_p_CTR)
      call set_sph_FDM_viscosity_mat(-ione, itwo, itwo,                 &
     &    sph_rj, fl_prop, radial_variation, g_sph_rj, coef_d,          &
     &    ione, fdm2_pol_CTR%dmat_fix_dr(-1,2),                         &
     &    fdm2_pol_CTR%dmat_fix_dr(-1,3), mat2_viscous_CMB1)
      call sub_sph_pol_viscous_mat7_CTR1                                &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    mat3_grad_p_CTR(0), mat2_viscous_CMB1(1,0), mat7)
!
      end subroutine sph_FDM2_vpol_viscosity_mat_ICB
!
!  -------------------------------------------------------------------
!
      subroutine sph_FDM4_vpol_viscosity_mat_CMB                        &
     &        (sph_rj, fl_prop, radial_variation,                       &
     &         g_sph_rj, coef_p, coef_d, fdm_e3,                        &
     &         fdm4_pol_CTR, fdm3e_center, fdm3e_center1,               &
     &         mat4_viscous_CMB1, hdiv_visous_mat_CTR, mat9)
!
      use t_coef_fdm4_MHD_boundaries
      use t_coef_fdm3e_MHD_boundaries
      use t_coef_fdm4_vpol_centre
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use set_sph_pol_viscousity
      use set_sph_hdiv_viscousity
      use set_sph_pol_viscous_CTR
      use set_sph_hdiv_viscous_CTR
      use set_sph_pol_vscs_FDM4_mat
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
!
      type(fdm_matrix), intent(in) :: fdm_e3(0:1)
      type(fdm4_centre_vpol), intent(in) :: fdm4_pol_CTR
      type(fdm3e_BC_hdiv), intent(in) :: fdm3e_center
      type(fdm3e_BC_hdiv), intent(in) :: fdm3e_center1
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat4_viscous_CMB1(sph_rj%nidx_rj(2),-2:2)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat9(9,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!
      real(kind = kreal) :: mat3_grad_p_CTR(-1:2)
!
!
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (ione, izero, ione, sph_rj, fl_prop,                         &
     &      radial_variation, g_sph_rj, coef_d, ione,                   &
     &      fdm3e_center%dmat_vp0(-2,1),                                &
     &      fdm3e_center%dmat_vp0(-2,2),                                &
     &      fdm3e_center%dmat_vp0(-2,3),                                &
     &      fdm3e_center%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,0))
      call sub_sph_hdiv_viscous_mat9_CTR                                &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    coef_p, hdiv_visous_mat_CTR(1,0), mat9)
!
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (itwo, -ione, ione, sph_rj, fl_prop,                         &
     &      radial_variation, g_sph_rj, coef_d, ione,                   &
     &      fdm3e_center1%dmat_vp0(-2,1),                               &
     &      fdm3e_center1%dmat_vp0(-2,2),                               &
     &      fdm3e_center1%dmat_vp0(-2,3),                               &
     &      fdm3e_center1%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,-1))
      call sub_sph_hdiv_viscous_mat9_CTR1                               &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    coef_p, hdiv_visous_mat_CTR(1,-1), mat9)
!
      call sph_FDM_layer_p_grad_mat(izero, itwo, ione, coef_p,          &
     &    fdm_e3(1)%nri_mat, fdm_e3(1)%dmat, mat3_grad_p_CTR)
      call set_sph_FDM_viscosity_mat(-ione, itwo, itwo,                 &
     &    sph_rj, fl_prop, radial_variation, g_sph_rj, coef_d,          &
     &    ione, fdm4_pol_CTR%dmat_vp1(-2,2),                            &
     &    fdm4_pol_CTR%dmat_vp1(-2,3), mat4_viscous_CMB1)
      call sub_sph_pol_viscous_mat9_CTR1                                &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    mat3_grad_p_CTR(0), mat4_viscous_CMB1(1,0), mat9)
!
      call sph_FDM_layer_p_grad_mat(-ione, itwo, itwo, coef_p,          &
     &    fdm_e3(1)%nri_mat, fdm_e3(1)%dmat, mat3_grad_p_CTR)
      call set_sph_FDM_viscosity_mat(-ione, itwo, itwo,                 &
     &    sph_rj, fl_prop, radial_variation, g_sph_rj, coef_d,          &
     &    ione, fdm4_pol_CTR%dmat_vp1(-2,2),                            &
     &    fdm4_pol_CTR%dmat_vp1(-2,3), mat4_viscous_CMB1)
      call sub_sph_pol_viscous_mat9_CTR2                                &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    mat3_grad_p_CTR(-1), mat4_viscous_CMB1(1,-1), mat9)
!
      end subroutine sph_FDM4_vpol_viscosity_mat_CMB
!
!  -------------------------------------------------------------------
!
      end module set_sph_pol_hdiv_viscs_CTR

