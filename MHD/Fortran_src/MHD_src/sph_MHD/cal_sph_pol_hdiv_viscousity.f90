!>@file   cal_sph_pol_hdiv_viscousity.f90
!!@brief  module cal_sph_pol_hdiv_viscousity
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Substitute viscousity matrix in each layer
!!
!!@verbatim
!!@endverbatim
!!
      module cal_sph_pol_hdiv_viscousity
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
      subroutine sph_exp_FDM2_vpol_viscosity                            &
     &        (kr_st, kr_ed, sph_rj, fl_prop, radial_variation,         &
     &         g_sph_rj, coef_p, coef_d,                                &
     &         nri_fdm, fdm1_e2n_d1_mat, fdm2_d1_mat, fdm2_d2_mat,      &
     &         fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat, fdm3e_d3_mat,  &
     &         d_vpol, press_e, mat1_grad_p,                            &
     &         mat2_viscous, hdiv_visous_mat,                           &
     &         d_viscous_p, hdiv_viscous_e)
!
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use set_sph_pol_hdiv_viscous
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: nri_fdm
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
!
      real(kind = kreal), intent(in) :: fdm1_e2n_d1_mat(0:1)
      real(kind = kreal), intent(in) :: fdm2_d1_mat(nri_fdm,-1:1)
      real(kind = kreal), intent(in) :: fdm2_d2_mat(nri_fdm,-1:1)
      real(kind = kreal), intent(in) :: fdm3e_d0_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d1_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d2_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d3_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout) :: mat1_grad_p(0:1)
      real(kind = kreal), intent(inout)                                 &
     &           :: mat2_viscous(sph_rj%nidx_rj(2),-1:1)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_viscous_e(sph_rj%nnod_rj)
!
      integer(kind = kint) :: kr
!
!
!$omp parallel do private(kr,mat1_grad_p,mat2_viscous,hdiv_visous_mat)
      do kr = kr_st+2, kr_ed-2
        call sph_FDM_layer_p_grad_mat(ione, kr, coef_p, nri_fdm,        &
     &                                fdm1_e2n_d1_mat, mat1_grad_p)
        call set_sph_FDM_viscosity_mat(ione, kr,                        &
     &      sph_rj, fl_prop, radial_variation, g_sph_rj,                &
     &      coef_d, nri_fdm, fdm2_d1_mat, fdm2_d2_mat, mat2_viscous)
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (kr, sph_rj, fl_prop, radial_variation, g_sph_rj,            &
     &      coef_d, nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat,                &
     &      fdm3e_d2_mat, fdm3e_d3_mat, hdiv_visous_mat)
!
        call add_exp2_sph_pol_viscous                                   &
     &     (kr, sph_rj%nnod_rj, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),   &
     &      mat1_grad_p, mat2_viscous, d_vpol, press_e, d_viscous_p)
        call add_exp_sph_hdiv_viscous                                   &
     &     (kr, sph_rj%nnod_rj, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),   &
     &      coef_p, hdiv_visous_mat, d_vpol, press_e,                   &
     &      hdiv_viscous_e)
      end do
!$omp end parallel do
!
      end subroutine sph_exp_FDM2_vpol_viscosity
!
! -----------------------------------------------------------------------
!
      subroutine sph_exp_FDM4_vpol_viscosity                            &
     &        (kr_st, kr_ed, sph_rj, fl_prop, radial_variation,         &
     &         g_sph_rj, coef_p, coef_d,                                &
     &         nri_fdm, fdm3_e2n_d1_mat, fdm4_d1_mat, fdm4_d2_mat,      &
     &         fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat, fdm3e_d3_mat,  &
     &         d_vpol, press_e,                                         &
     &         mat3_grad_p, mat4_viscous, hdiv_visous_mat,              &
     &         d_viscous_p, hdiv_viscous_e)
!
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use set_sph_pol_hdiv_viscous
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: nri_fdm
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
!
      real(kind = kreal), intent(in) :: fdm3_e2n_d1_mat(-1:2)
      real(kind = kreal), intent(in) :: fdm4_d1_mat(nri_fdm,-2:2)
      real(kind = kreal), intent(in) :: fdm4_d2_mat(nri_fdm,-2:2)
      real(kind = kreal), intent(in) :: fdm3e_d0_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d1_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d2_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d3_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout) :: mat3_grad_p(-1:2)
      real(kind = kreal), intent(inout)                                 &
     &           :: mat4_viscous(sph_rj%nidx_rj(2),-2:2)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_viscous_e(sph_rj%nnod_rj)
!
      integer(kind = kint) :: kr
!
!
!$omp parallel do private(kr,mat3_grad_p,mat4_viscous,hdiv_visous_mat)
      do kr = kr_st+2, kr_ed-2
        call sph_FDM_layer_p_grad_mat(itwo, kr, coef_p, nri_fdm,        &
     &                                fdm3_e2n_d1_mat, mat3_grad_p)
        call set_sph_FDM_viscosity_mat(itwo, kr,                        &
     &      sph_rj, fl_prop, radial_variation, g_sph_rj,                &
     &      coef_d, nri_fdm, fdm4_d1_mat, fdm4_d2_mat, mat4_viscous)
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (kr, sph_rj, fl_prop, radial_variation, g_sph_rj,            &
     &      coef_d, nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat,                &
     &      fdm3e_d2_mat, fdm3e_d3_mat, hdiv_visous_mat)
!
        call add_exp4_sph_pol_viscous                                   &
     &     (kr, sph_rj%nnod_rj, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),   &
     &      mat3_grad_p, mat4_viscous, d_vpol, press_e, d_viscous_p)
        call add_exp_sph_hdiv_viscous                                   &
     &     (kr, sph_rj%nnod_rj, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),   &
     &      coef_p, hdiv_visous_mat, d_vpol, press_e,                   &
     &      hdiv_viscous_e)
      end do
!$omp end parallel do
!
      end subroutine sph_exp_FDM4_vpol_viscosity
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_FDM2_vpol_viscosity_mat                            &
     &        (kr_st, kr_ed, sph_rj, fl_prop, radial_variation,         &
     &         g_sph_rj, coef_p, coef_d,                                &
     &         nri_fdm, fdm1_e2n_d1_mat, fdm2_d1_mat, fdm2_d2_mat,      &
     &         fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat, fdm3e_d3_mat,  &
     &         mat1_grad_p, mat2_viscous, hdiv_visous_mat, mat7)
!
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use set_sph_pol_hdiv_viscous
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: nri_fdm
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
!
      real(kind = kreal), intent(in) :: fdm1_e2n_d1_mat(0:1)
      real(kind = kreal), intent(in) :: fdm2_d1_mat(nri_fdm,-1:1)
      real(kind = kreal), intent(in) :: fdm2_d2_mat(nri_fdm,-1:1)
      real(kind = kreal), intent(in) :: fdm3e_d0_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d1_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d2_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d3_mat(nri_fdm,-2:1)
!
      real(kind = kreal), intent(inout) :: mat1_grad_p(0:1)
      real(kind = kreal), intent(inout)                                 &
     &           :: mat2_viscous(sph_rj%nidx_rj(2),-1:1)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: kr
!
!
!$omp parallel do private(kr,mat1_grad_p,mat2_viscous,hdiv_visous_mat)
      do kr = kr_st+2, kr_ed-2
        call sph_FDM_layer_p_grad_mat(ione, kr, coef_p, nri_fdm,        &
     &                                fdm1_e2n_d1_mat, mat1_grad_p)
        call set_sph_FDM_viscosity_mat(ione, kr,                        &
     &      sph_rj, fl_prop, radial_variation, g_sph_rj,                &
     &      coef_d, nri_fdm, fdm2_d1_mat, fdm2_d2_mat, mat2_viscous)
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (kr, sph_rj, fl_prop, radial_variation, g_sph_rj,            &
     &      coef_d, nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat,                &
     &      fdm3e_d2_mat, fdm3e_d3_mat, hdiv_visous_mat)
!
        call sub_sph_pol_viscous_FDM2_mat                               &
     &     (kr, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), coef_p,           &
     &      mat1_grad_p, mat2_viscous, hdiv_visous_mat, mat7)
      end do
!$omp end parallel do
!
      end subroutine sph_FDM2_vpol_viscosity_mat
!
! -----------------------------------------------------------------------
!
      subroutine sph_FDM4_vpol_viscosity_mat                            &
     &        (kr_st, kr_ed, sph_rj, fl_prop, radial_variation,         &
     &         g_sph_rj, coef_p, coef_d,                                &
     &         nri_fdm, fdm3_e2n_d1_mat, fdm4_d1_mat, fdm4_d2_mat,      &
     &         fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat, fdm3e_d3_mat,  &
     &         mat3_grad_p, mat4_viscous, hdiv_visous_mat, mat9)
!
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use set_sph_pol_hdiv_viscous
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: nri_fdm
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
!
      real(kind = kreal), intent(in) :: fdm3_e2n_d1_mat(-1:2)
      real(kind = kreal), intent(in) :: fdm4_d1_mat(nri_fdm,-2:2)
      real(kind = kreal), intent(in) :: fdm4_d2_mat(nri_fdm,-2:2)
      real(kind = kreal), intent(in) :: fdm3e_d0_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d1_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d2_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d3_mat(nri_fdm,-2:1)
!
      real(kind = kreal), intent(inout) :: mat3_grad_p(-1:2)
      real(kind = kreal), intent(inout)                                 &
     &           :: mat4_viscous(sph_rj%nidx_rj(2),-2:2)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat9(9,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: kr
!
!
!$omp parallel do private(kr,mat3_grad_p,mat4_viscous,hdiv_visous_mat)
      do kr = kr_st+2, kr_ed-2
        call sph_FDM_layer_p_grad_mat(itwo, kr, coef_p, nri_fdm,        &
     &                                fdm3_e2n_d1_mat, mat3_grad_p)
        call set_sph_FDM_viscosity_mat(itwo, kr,                        &
     &      sph_rj, fl_prop, radial_variation, g_sph_rj,                &
     &      coef_d, nri_fdm, fdm4_d1_mat, fdm4_d2_mat, mat4_viscous)
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (kr, sph_rj, fl_prop, radial_variation, g_sph_rj,            &
     &      coef_d, nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat,                &
     &      fdm3e_d2_mat, fdm3e_d3_mat, hdiv_visous_mat)
!
        call sub_sph_pol_viscous_FDM4_mat                               &
     &     (kr, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), coef_p,           &
     &      mat3_grad_p, mat4_viscous, hdiv_visous_mat, mat9)
      end do
!$omp end parallel do
!
      end subroutine sph_FDM4_vpol_viscosity_mat
!
! -----------------------------------------------------------------------
!
      end module cal_sph_pol_hdiv_viscousity
