!> @file  select_exp_scalar_bc.f90
!!      module select_exp_scalar_bc
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate radial delivatives
!!
!!@verbatim
!!      subroutine sel_bc_radial_grad_scalar(sph_bc, is_fld, is_grad,   &
!!     &          ntot_phys_rj, d_rj)
!!        Input:    is_fld
!!        Solution: is_grad
!!
!!      subroutine sel_bc_sph_scalar_diffusion(sph_bc, coef_diffuse,    &
!!     &          is_fld, is_diffuse, ntot_phys_rj, d_rj)
!!        Input:    is_fld
!!        Solution: is_diffusee
!!
!!      subroutine sel_bc_sph_scalar_advect(sph_bc, is_flux, is_advect, &
!!     &          ntot_phys_rj, d_rj)
!!        Input:    is_flux
!!        Solution: is_advect
!!@endverbatim
!!
!!@param sph_bc  Structure for basic boundary condition parameters
!!
!!@param coef_diffuse   Diffusion coefficient
!!
!!@param is_fld       Spherical hermonics data address for input vector
!!@param is_grad      Spherical hermonics data address for gradient
!!@param is_diffuse   Input spectr diffusiton term address
!!@param is_flux    Spherical hermonics data address for input flux
!!@param is_advect  Spherical hermonics data address for advection
!
      module select_exp_scalar_bc
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_phys_address
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_bc_radial_grad_scalar(sph_bc, is_fld, is_grad,     &
     &          ntot_phys_rj, d_rj)
!
      use m_coef_fdm_to_center
      use t_boundary_params_sph_MHD
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
      use cal_sph_exp_rotation
      use cal_sph_exp_center
!
      type(sph_boundary_type), intent(in) :: sph_bc
      integer(kind = kint), intent(in) :: is_fld, is_grad
      integer(kind = kint), intent(in) :: ntot_phys_rj
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_center1_grad22(nnod_rj, nidx_rj(2), sph_bc%r_ICB,  &
     &      fdm2_fix_fld_ctr1, is_fld, is_grad, ntot_phys_rj, d_rj)
        call sph0_scalar_fill_ctr_grad2                                 &
     &     (inod_rj_center, sph_rj1%idx_rj_degree_zero,                 &
     &      nnod_rj, nidx_rj(2),    &
     &      fdm2_fix_fld_ctr1, is_fld, is_grad, ntot_phys_rj, d_rj)
      else if(sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call cal_sph_center1_grad22(nnod_rj, nidx_rj(2), sph_bc%r_ICB,  &
     &      fdm2_fix_fld_ctr1, is_fld, is_grad, ntot_phys_rj, d_rj)
        call dsdr_sph_lm0_fixed_ctr_2                                   &
     &     (inod_rj_center, sph_rj1%idx_rj_degree_zero,                 &
     &      nnod_rj, nidx_rj(2),    &
     &      sph_bc%r_ICB, sph_bc%CTR_fld, fdm2_fix_fld_ctr1,            &
     &      fdm2_fixed_center, is_fld, is_grad, ntot_phys_rj, d_rj)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        call dsdr_sph_in_fix_flux_2(nnod_rj, nidx_rj(2),                &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%ICB_flux,                &
     &      is_fld, is_grad, ntot_phys_rj, d_rj)
        call dsdr_sph_lm0_in_fix_flux_2                                 &
     &     (sph_rj1%idx_rj_degree_zero, nnod_rj, nidx_rj(2),            &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%ICB_flux, is_grad,       &
     &      ntot_phys_rj, d_rj)
      else
        call dsdr_sph_fix_scalar_in_2(nnod_rj, nidx_rj(2),              &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,        &
     &      sph_bc%ICB_fld, is_fld, is_grad, ntot_phys_rj, d_rj)
        call dsdr_sph_lm0_fix_scalar_in_2                               &
     &     (sph_rj1%idx_rj_degree_zero, nnod_rj, nidx_rj(2),            &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,        &
     &      sph_bc%ICB_fld, is_fld, is_grad, ntot_phys_rj, d_rj)
      end if
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux) then
        call dsdr_sph_out_fix_flux_2(nnod_rj, nidx_rj(2),               &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%CMB_flux,               &
     &      is_fld, is_grad, ntot_phys_rj, d_rj)
        call dsdr_sph_lm0_out_fix_flux_2(sph_rj1%idx_rj_degree_zero,    &
     &      nnod_rj, nidx_rj(2), sph_bc%kr_out, sph_bc%r_CMB,           &
     &      sph_bc%CMB_flux, is_grad, ntot_phys_rj, d_rj)
      else
        call dsdr_sph_fix_scalar_out_2(nnod_rj, nidx_rj(2),             &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      sph_bc%CMB_fld, is_fld, is_grad, ntot_phys_rj, d_rj)
        call dsdr_sph_lm0_fix_scalar_out_2                              &
     &     (sph_rj1%idx_rj_degree_zero, nnod_rj, nidx_rj(2),      &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      sph_bc%CMB_fld, is_fld, is_grad, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_bc_radial_grad_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_bc_sph_scalar_diffusion(sph_bc, coef_diffuse,      &
     &          is_fld, is_diffuse, ntot_phys_rj, d_rj)
!
      use t_boundary_params_sph_MHD
      use m_coef_fdm_to_center
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
      use cal_sph_exp_center
!
      type(sph_boundary_type), intent(in) :: sph_bc
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      integer(kind = kint), intent(in) :: ntot_phys_rj
      real(kind = kreal), intent(in) :: coef_diffuse
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_filled_center_diffuse2                             &
     &     (inod_rj_center, sph_rj1%idx_rj_degree_zero,         &
     &      nnod_rj, nidx_rj(2),    &
     &      sph_bc%r_ICB,fdm2_fix_fld_ctr1, fdm2_fix_dr_center,         &
     &      coef_diffuse, is_fld, is_diffuse, ntot_phys_rj, d_rj)
      else if(sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call cal_sph_fixed_center1_diffuse2(nnod_rj, nidx_rj(2),        &
     &     sph_bc%r_ICB, fdm2_fix_fld_ctr1, sph_bc%ICB_fld,             &
     &     coef_diffuse, is_fld, is_diffuse, ntot_phys_rj, d_rj)
        call cal_sph_fixed_center_diffuse2                              &
     &     (inod_rj_center, sph_rj1%idx_rj_degree_zero,        &
     &      nnod_rj, is_diffuse, ntot_phys_rj, d_rj)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        call cal_sph_in_fix_flux_diffuse2(nnod_rj, nidx_rj(2),          &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_dr_ICB,         &
     &      sph_bc%ICB_flux, coef_diffuse, is_fld, is_diffuse,          &
     &      ntot_phys_rj, d_rj)
      else
        call cal_sph_fix_scalar_in_diffuse2(nnod_rj, nidx_rj(2),        &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,        &
     &      sph_bc%ICB_fld, coef_diffuse, is_fld, is_diffuse,           &
     &      ntot_phys_rj, d_rj)
      end if
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux) then
        call cal_sph_out_fix_flux_diffuse2(nnod_rj, nidx_rj(2),         &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_dr_CMB,        &
     &      sph_bc%CMB_flux, coef_diffuse, is_fld, is_diffuse,          &
     &      ntot_phys_rj, d_rj)
      else
        call cal_sph_out_fix_scalar_diffuse2(nnod_rj, nidx_rj(2),       &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      sph_bc%CMB_fld, coef_diffuse, is_fld, is_diffuse,           &
     &      ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_bc_sph_scalar_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine sel_bc_sph_scalar_advect(sph_bc, is_flux, is_advect,   &
     &          ntot_phys_rj, d_rj)
!
      use t_boundary_params_sph_MHD
      use m_coef_fdm_to_center
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
      use cal_sph_exp_center
!
      type(sph_boundary_type), intent(in) :: sph_bc
      integer(kind = kint), intent(in) :: is_flux, is_advect
      integer(kind = kint), intent(in) :: ntot_phys_rj
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_filled_center_diffuse2                             &
     &     (inod_rj_center, sph_rj1%idx_rj_degree_zero,   &
     &      nnod_rj, nidx_rj(2),    &
     &      sph_bc%r_ICB,fdm2_fix_fld_ctr1, fdm2_fix_dr_center,         &
     &      dminus, is_flux, is_advect, ntot_phys_rj, d_rj)
      else if(sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call cal_sph_div_flux_4_fix_ctr(nnod_rj, nidx_rj(2),            &
     &      sph_bc%r_ICB, sph_bc%ICB_fld, fdm2_fix_fld_ctr1,            &
     &      is_flux, is_advect, ntot_phys_rj, d_rj)
        call cal_sph_fixed_center_diffuse2                              &
     &     (inod_rj_center, sph_rj1%idx_rj_degree_zero, &
     &      nnod_rj, is_advect,     &
     &      ntot_phys_rj, d_rj)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        call cal_div_sph_in_fix_flux_2                                  &
     &     (nnod_rj, nidx_rj(2), sph_bc%kr_in,                          &
     &      sph_bc%r_ICB, sph_bc%ICB_flux, is_flux, is_advect,          &
     &      ntot_phys_rj, d_rj)
      else
        call cal_sph_div_flux_4_fix_in(nnod_rj, nidx_rj(2),             &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,        &
     &      sph_bc%ICB_fld, is_flux, is_advect, ntot_phys_rj, d_rj)
      end if
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux) then
        call cal_div_sph_out_fix_flux_2                                 &
     &     (nnod_rj, nidx_rj(2), sph_bc%kr_out,                         &
     &      sph_bc%r_CMB, sph_bc%CMB_flux, is_flux, is_advect,          &
     &      ntot_phys_rj, d_rj)
      else
        call cal_sph_div_flux_4_fix_out(nnod_rj, nidx_rj(2),            &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      sph_bc%CMB_fld, is_flux, is_advect, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_bc_sph_scalar_advect
!
! -----------------------------------------------------------------------
!
      end module select_exp_scalar_bc
