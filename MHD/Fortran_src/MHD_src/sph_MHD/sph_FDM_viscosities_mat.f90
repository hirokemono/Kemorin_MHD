!>@file   sph_FDM_viscosities_mat.f90
!!@brief  module sph_FDM_viscosities_mat
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Forth order FDM on nodes
!!
!!@verbatim
!!      subroutine sph_FDM_whole_hdiv_viscousity(kr_st, kr_ed,          &
!!     &          sph_rj, fl_prop, radial_variation, fdm_3e,            &
!!     &          g_sph_rj, coef_d, hdiv_visous_mat)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        type(fdm_matrix), intent(in) :: fdm_nth(2)
!!        type(fdm_matrix), intent(in) :: fdm_3e(0:3)
!!        integer(kind = kint), intent(in) :: n_next
!!        integer(kind = kint), intent(in) :: kr_st, kr_ed
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in)                                &
!!     &            :: fdm_e2n_d1_mat(sph_rj%nidx_rj(1),-n_next+1:n_next)
!!        real(kind = kreal), intent(inout)                             &
!!     &            :: mat_grad_p(-n_next+1:n_next,sph_rj%nidx_rj(1))
!!        real(kind = kreal), intent(inout)                             &
!!     &            :: mat_viscous(-n_next:n_next,                      &
!!     &                           sph_rj%nidx_rj(2),sph_rj%nidx_rj(1))
!!        real(kind = kreal), intent(inout)                             &
!!     &     :: hdiv_visous_mat(-2:1,sph_rj%nidx_rj(2),sph_rj%nidx_rj(1))
!!
!!      subroutine sph_FDM_layer_p_grad_mat(n_next, coef_p,             &
!!     &                                    fdm_e2n_d1_mat, mat_grad_p)
!!      subroutine sph_FDM_layer_viscosity(n_next, kr,                  &
!!     &          sph_rj, fl_prop, radial_variation, g_sph_rj,          &
!!     &          coef_d, fdm_d1_mat, fdm_d2_mat, mat_viscous)
!!      subroutine sph_FDM_layer_hdiv_viscousity                        &
!!     &         (kr, sph_rj, fl_prop, radial_variation,                &
!!     &          g_sph_rj, coef_d, fdm3e_d0_mat, fdm3e_d1_mat,         &
!!     &          fdm3e_d2_mat, fdm3e_d3_mat, hdiv_visous_mat)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        integer(kind = kint), intent(in) :: n_next
!!        integer(kind = kint), intent(in) :: kr
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: fdm_e2n_d1_mat(-n_next+1:n_next)
!!        real(kind = kreal), intent(in) :: fdm_d1_mat(-n_next:n_next)
!!        real(kind = kreal), intent(in) :: fdm_d2_mat(-n_next:n_next)
!!        real(kind = kreal), intent(in) :: fdm3e_d0_mat(-2:1)
!!        real(kind = kreal), intent(in) :: fdm3e_d1_mat(-2:1)
!!        real(kind = kreal), intent(in) :: fdm3e_d2_mat(-2:1)
!!        real(kind = kreal), intent(in) :: fdm3e_d3_mat(-2:1)
!!        real(kind = kreal), intent(inout)                             &
!!     &            :: mat_grad_p(-n_next+1:n_next)
!!        real(kind = kreal), intent(inout)                             &
!!     &            :: mat_viscous(-n_next:n_next,sph_rj%nidx_rj(2))
!!        real(kind = kreal), intent(inout)                             &
!!     &            :: hdiv_visous_mat(-2:1,sph_rj%nidx_rj(2))
!!@endverbatim
!!
      module sph_FDM_viscosities_mat
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
      subroutine sph_FDM_layer_p_grad_mat(n_next, kr, coef_p, nri_fdm,  &
     &                                    fdm_e2n_d1_mat, mat_grad_p)
!
      use cal_sph_FDM_viscosity_mat
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr, nri_fdm
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in)                                    &
     &                   :: fdm_e2n_d1_mat(-n_next+1:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &            :: mat_grad_p(-n_next+1:n_next)
!
!
      call set_sph_FDM_pressure_grad_mat(n_next, kr, coef_p,            &
     &    nri_fdm, fdm_e2n_d1_mat(-n_next+1), mat_grad_p)
!
      end subroutine sph_FDM_layer_p_grad_mat
!
!  -------------------------------------------------------------------
!
      subroutine set_sph_FDM_viscosity_mat(n_next, kr,                  &
     &          sph_rj, fl_prop, radial_variation, g_sph_rj,            &
     &          coef_d, nri_fdm, fdm_d1_mat, fdm_d2_mat, mat_viscous)
!
      use cal_sph_FDM_viscosity_mat
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr, nri_fdm
      real(kind = kreal), intent(in)                                    &
     &                   :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in)                                    &
     &                   :: fdm_d1_mat(nri_fdm,-n_next:n_next)
      real(kind = kreal), intent(in)                                    &
     &                   :: fdm_d2_mat(nri_fdm,-n_next:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(sph_rj%nidx_rj(2),-n_next:n_next)
!
!
      call set_sph_FDM_fix_viscous_mat(n_next, kr, sph_rj%nidx_rj(2),   &
     &    sph_rj%ar_1d_rj(kr,2), g_sph_rj,                              &
     &    sph_rj%nidx_rj(1), fdm_d2_mat, mat_viscous)
!
      call add_sph_FDM_val_viscous_mat(n_next, kr,                      &
     &    fl_prop%flag_viscous_variation,                               &
     &    fl_prop%flag_ref_density_valiation,                           &
     &    sph_rj%nidx_rj(2), sph_rj%ar_1d_rj(kr,1),                     &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_nu),                   &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_dnu_norm),             &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_drho_norm),            &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_d2rho_norm),           &
     &    sph_rj%nidx_rj(1), fdm_d1_mat, mat_viscous)
!
!$omp parallel workshare
      mat_viscous(1:sph_rj%nidx_rj(2),-n_next:n_next)                   &
     &       = coef_d * mat_viscous(1:sph_rj%nidx_rj(2),-n_next:n_next)
!$omp end parallel workshare
!
      end subroutine set_sph_FDM_viscosity_mat
!
!  -------------------------------------------------------------------
!
      subroutine sph_FDM_layer_hdiv_viscousity                          &
     &         (kr, sph_rj, fl_prop, radial_variation, g_sph_rj,        &
     &          coef_d, nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat,            &
     &          fdm3e_d2_mat, fdm3e_d3_mat, hdiv_visous_mat)
!
      use cal_sph_FDM3e_hdiv_viscous
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      integer(kind = kint), intent(in) :: kr, nri_fdm
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: fdm3e_d0_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d1_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d2_mat(nri_fdm,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d3_mat(nri_fdm,-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: hdiv_visous_mat(sph_rj%nidx_rj(2),-2:1)
!
!
      call set_sph_FDM_fix_hdiv_viscs_mat(kr, sph_rj%nidx_rj(2),        &
     &    sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3), g_sph_rj,     &
     &    nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d3_mat,            &
     &    hdiv_visous_mat)
!
      call add_sph_FDM_val_hdiv_viscs_mat                               &
     &   (kr, fl_prop%flag_viscous_variation,                           &
     &    fl_prop%flag_ref_density_valiation,                           &
     &    sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                    &
     &    sph_rj%ar_ele_rj(kr,2), g_sph_rj,                             &
     &    radial_variation%d_fld(kr,fl_prop%ir_nu),                     &
     &    radial_variation%d_fld(kr,fl_prop%ir_dnu_norm),               &
     &    radial_variation%d_fld(kr,fl_prop%ir_drho_norm),              &
     &    radial_variation%d_fld(kr,fl_prop%ir_d2rho_norm),             &
     &    nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,            &
     &    hdiv_visous_mat)
!
!$omp parallel workshare
      hdiv_visous_mat(1:sph_rj%nidx_rj(2),-2:1)                         &
     &       = coef_d * hdiv_visous_mat(1:sph_rj%nidx_rj(2),-2:1)
!$omp end parallel workshare
!
      end subroutine sph_FDM_layer_hdiv_viscousity
!
! -----------------------------------------------------------------------
!
      end module sph_FDM_viscosities_mat
