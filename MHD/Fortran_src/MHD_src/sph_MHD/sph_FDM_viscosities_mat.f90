!>@file   sph_FDM_viscosities_mat.f90
!!@brief  module sph_FDM_viscosities_mat
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Forth order FDM on nodes
!!
!!@verbatim
!!      subroutine sph_FDM_whole_p_grad_mat(n_next, kr_st, kr_ed,       &
!!     &          sph_rj, coef_p, fdm_e2n_d1_mat, mat_grad_p)
!!      subroutine sph_FDM_whole_viscosity(n_next, kr_st, kr_ed,        &
!!     &          sph_rj, fl_prop, radial_variation, fdm_nth,           &
!!     &          g_sph_rj, coef_d, mat_viscous)
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
      subroutine sph_FDM_whole_p_grad_mat(n_next, kr_st, kr_ed,         &
     &          sph_rj, coef_p, fdm_e2n_d1_mat, mat_grad_p)
!
      use cal_whole_sph_FDM_viscosity
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in)                                    &
     &            :: fdm_e2n_d1_mat(sph_rj%nidx_rj(1),-n_next+1:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &            :: mat_grad_p(-n_next+1:n_next,sph_rj%nidx_rj(1))
!
!
      call set_FDM_pressure_grad_mat(n_next, kr_st, kr_ed,              &
     &    sph_rj%nidx_rj(1), coef_p, fdm_e2n_d1_mat, mat_grad_p)
!
      end subroutine sph_FDM_whole_p_grad_mat
!
!  -------------------------------------------------------------------
!
      subroutine sph_FDM_whole_viscosity(n_next, kr_st, kr_ed,          &
     &          sph_rj, fl_prop, radial_variation, fdm_nth,             &
     &          g_sph_rj, coef_d, mat_viscous)
!
      use cal_whole_sph_FDM_viscosity
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
      type(fdm_matrix), intent(in) :: fdm_nth(2)
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      real(kind = kreal), intent(in)                                    &
     &                   :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
!
      real(kind = kreal), intent(inout)                                 &
     &            :: mat_viscous(-n_next:n_next,                        &
     &                           sph_rj%nidx_rj(2),sph_rj%nidx_rj(1))
!
!
      call sph_FDM_viscosity_mat(n_next, kr_st, kr_ed,                  &
     &    fl_prop%flag_viscous_variation,                               &
     &    fl_prop%flag_ref_density_valiation,                           &
     &    sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj(1,1),   &
     &    sph_rj%ar_1d_rj(1,2), g_sph_rj, coef_d,                       &
     &    radial_variation%d_fld(2,fl_prop%ir_nu),                      &
     &    radial_variation%d_fld(2,fl_prop%ir_dnu_norm),                &
     &    radial_variation%d_fld(2,fl_prop%ir_drho_norm),               &
     &    radial_variation%d_fld(2,fl_prop%ir_d2rho_norm),              &
     &    fdm_nth(1)%dmat, fdm_nth(2)%dmat, mat_viscous)
!
      end subroutine sph_FDM_whole_viscosity
!
!  -------------------------------------------------------------------
!
      subroutine sph_FDM_whole_hdiv_viscousity(kr_st, kr_ed,            &
     &          sph_rj, fl_prop, radial_variation, fdm_3e,              &
     &          g_sph_rj, coef_d, hdiv_visous_mat)
!
      use cal_sph_FDM3e_hdiv_viscous
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
      type(fdm_matrix), intent(in) :: fdm_3e(0:3)
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
!
      real(kind = kreal), intent(inout)                                 &
     &     :: hdiv_visous_mat(-2:1,sph_rj%nidx_rj(2),sph_rj%nidx_rj(1))
!
!
      call cal_sph_hdiv_viscousity(kr_st, kr_ed,                        &
     &    fl_prop%flag_viscous_variation,                               &
     &    fl_prop%flag_ref_density_valiation,                           &
     &    sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(1,1),  &
     &    sph_rj%ar_ele_rj(1,2), sph_rj%ar_ele_rj(1,3), g_sph_rj,       &
     &    coef_d, radial_variation%d_fld(2,fl_prop%ir_nu),              &
     &    radial_variation%d_fld(2,fl_prop%ir_dnu_norm),                &
     &    radial_variation%d_fld(2,fl_prop%ir_drho_norm),               &
     &    radial_variation%d_fld(2,fl_prop%ir_d2rho_norm),              &
     &    fdm_3e(0)%dmat, fdm_3e(1)%dmat, fdm_3e(2)%dmat,               &
     &    fdm_3e(3)%dmat, hdiv_visous_mat)
!
      end subroutine sph_FDM_whole_hdiv_viscousity
!
! -----------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine sph_FDM_layer_p_grad_mat(n_next, coef_p,               &
     &                                    fdm_e2n_d1_mat, mat_grad_p)
!
      use cal_whole_sph_FDM_viscosity
!
      integer(kind = kint), intent(in) :: n_next
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in)                                    &
     &                   :: fdm_e2n_d1_mat(-n_next+1:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &            :: mat_grad_p(-n_next+1:n_next)
!
!
      call set_FDM_pressure_grad_mat(n_next, ione, ione, ione,          &
     &    coef_p, fdm_e2n_d1_mat(-n_next+1), mat_grad_p(-n_next+1))
!
      end subroutine sph_FDM_layer_p_grad_mat
!
!  -------------------------------------------------------------------
!
      subroutine sph_FDM_layer_viscosity(n_next, kr,                    &
     &          sph_rj, fl_prop, radial_variation, g_sph_rj,            &
     &          coef_d, fdm_d1_mat, fdm_d2_mat, mat_viscous)
!
      use cal_whole_sph_FDM_viscosity
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr
      real(kind = kreal), intent(in)                                    &
     &                   :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: fdm_d1_mat(-n_next:n_next)
      real(kind = kreal), intent(in) :: fdm_d2_mat(-n_next:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(-n_next:n_next,sph_rj%nidx_rj(2))
!
!
      call sph_FDM_viscosity_mat(n_next, ione, ione,                    &
     &    fl_prop%flag_viscous_variation,                               &
     &    fl_prop%flag_ref_density_valiation,                           &
     &    ione, sph_rj%nidx_rj(2), sph_rj%ar_1d_rj(kr,1),               &
     &    sph_rj%ar_1d_rj(kr,2), g_sph_rj, coef_d,                      &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_nu),                   &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_dnu_norm),             &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_drho_norm),            &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_d2rho_norm),           &
     &    fdm_d1_mat(-n_next), fdm_d2_mat(-n_next),                     &
     &    mat_viscous(-n_next,1))
!
      end subroutine sph_FDM_layer_viscosity
!
!  -------------------------------------------------------------------
!
      subroutine sph_FDM_layer_hdiv_viscousity                          &
     &         (kr, sph_rj, fl_prop, radial_variation,                  &
     &          g_sph_rj, coef_d, fdm3e_d0_mat, fdm3e_d1_mat,           &
     &          fdm3e_d2_mat, fdm3e_d3_mat, hdiv_visous_mat)
!
      use cal_sph_FDM3e_hdiv_viscous
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      integer(kind = kint), intent(in) :: kr
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: fdm3e_d0_mat(-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d1_mat(-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d2_mat(-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d3_mat(-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: hdiv_visous_mat(-2:1,sph_rj%nidx_rj(2))
!
!
      call cal_sph_hdiv_viscousity(ione, ione,                          &
     &    fl_prop%flag_viscous_variation,                               &
     &    fl_prop%flag_ref_density_valiation,                           &
     &    ione, sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),              &
     &    sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3), g_sph_rj,     &
     &    coef_d, radial_variation%d_fld(kr,fl_prop%ir_nu),             &
     &    radial_variation%d_fld(kr,fl_prop%ir_dnu_norm),               &
     &    radial_variation%d_fld(kr,fl_prop%ir_drho_norm),              &
     &    radial_variation%d_fld(kr,fl_prop%ir_d2rho_norm),             &
     &    fdm3e_d0_mat(-2), fdm3e_d1_mat(-2),                           &
     &    fdm3e_d2_mat(-2), fdm3e_d3_mat(-2), hdiv_visous_mat(-2,1))
!
      end subroutine sph_FDM_layer_hdiv_viscousity
!
! -----------------------------------------------------------------------
!
      end module sph_FDM_viscosities_mat
