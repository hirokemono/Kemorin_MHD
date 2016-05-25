!>@file   const_r_mat_4_scalar_sph.f90
!!@brief  module const_r_mat_4_scalar_sph
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for time evolution of scalar fields
!!
!!@verbatim
!!      subroutine const_radial_mat_4_temp_sph(sph_rj)
!!      subroutine const_radial_mat_4_composit_sph(sph_rj)
!!      subroutine const_radial_mat_4_press_sph(sph_rj)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!@endverbatim
!
      module const_r_mat_4_scalar_sph
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_t_int_parameter
!
      use t_spheric_rj_data
      use t_sph_matrices
!
      implicit none
!
      private :: const_radial_mat_4_scalar_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_4_temp_sph(sph_rj)
!
      use m_boundary_params_sph_MHD
      use m_radial_matrices_sph
      use m_physical_property
!
      type(sph_rj_grid), intent(in) ::  sph_rj
!
!
      call alloc_band_mat_sph(ifive, sph_rj, band_temp_evo)
      call set_unit_on_diag(band_temp_evo)
!
      call const_radial_mat_4_scalar_sph(sph_rj, sph_bc_T,              &
     &    coef_imp_t, coef_temp, coef_d_temp, band_temp_evo)
!
      if(i_debug .eq. iflag_full_msg)                                   &
     &     call check_temp_matrices_sph(my_rank, sph_rj)
!
      end subroutine const_radial_mat_4_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_4_composit_sph(sph_rj)
!
      use m_boundary_params_sph_MHD
      use m_radial_matrices_sph
      use m_physical_property
!
      type(sph_rj_grid), intent(in) ::  sph_rj
!
!
      call const_radial_mat_4_scalar_sph(sph_rj, sph_bc_C,              &
     &    coef_imp_c, coef_light, coef_d_light, band_comp_evo)
!
      if(i_debug .eq. iflag_full_msg)                                   &
     &       call check_composit_matrix_sph(my_rank, sph_rj)
!
      end subroutine const_radial_mat_4_composit_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_4_press_sph(sph_rj)
!
      use m_physical_property
      use m_boundary_params_sph_MHD
      use m_coef_fdm_to_center
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use m_ludcmp_3band
      use set_sph_scalar_mat_bc
      use cal_inner_core_rotation
      use center_sph_matrices
      use mat_product_3band_mul
      use set_radial_mat_sph
!
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      real(kind = kreal) :: coef_p
!
!
      coef_p = - coef_press
      call set_unit_mat_4_poisson                                       &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out, band_p_poisson%mat)
      call add_scalar_poisson_mat_sph                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj,        &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out, coef_p, band_p_poisson%mat)
!
!   Boundary condition for ICB
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call add_scalar_poisson_mat_ctr1                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc_U%r_ICB, fdm2_fix_fld_ctr1, coef_p,                  &
     &      band_p_poisson%mat)
      else
        call add_icb_scalar_poisson_mat                                 &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_dr_ICB,   &
     &      coef_p, band_p_poisson%mat)
      end if
!
!   Boundary condition for CMB
!
      call add_cmb_scalar_poisson_mat                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_dr_CMB,    &
     &    coef_p, band_p_poisson%mat)
!
      call ludcmp_3band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_p_poisson)
!
      end subroutine const_radial_mat_4_press_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_4_scalar_sph                          &
     &         (sph_rj, sph_bc, coef_imp, coef_f, coef_d, band_s_evo)
!
      use m_coef_fdm_to_center
      use m_ludcmp_3band
      use t_boundary_params_sph_MHD
      use center_sph_matrices
      use set_radial_mat_sph
      use set_sph_scalar_mat_bc
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      real(kind = kreal), intent(in) :: coef_imp, coef_f, coef_d
!
      type(band_matrix_type), intent(inout) :: band_s_evo
!
      real(kind = kreal) :: coef
!
!
      if(coef_f .eq. zero) then
        coef = one
        call set_unit_mat_4_poisson                                     &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc%kr_in, sph_bc%kr_out, band_s_evo%mat)
      else
        coef = coef_imp * coef_d * dt
        call set_unit_mat_4_time_evo                                    &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), band_s_evo%mat)
      end if
!
      call add_scalar_poisson_mat_sph                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj,        &
     &    sph_bc%kr_in, sph_bc%kr_out, coef, band_s_evo%mat)
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center               &
     &   .or. sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call add_scalar_poisson_mat_ctr1                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc%r_ICB, fdm2_fix_fld_ctr1, coef, band_s_evo%mat)
      else if (sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        call add_fix_flux_icb_poisson_mat                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_bc%kr_in,         &
     &      sph_bc%r_ICB, sph_bc%fdm2_fix_dr_ICB, coef, band_s_evo%mat)
      else
        call set_fix_fld_icb_poisson_mat                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc%kr_in, band_s_evo%mat)
      end if
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux) then
        call add_fix_flux_cmb_poisson_mat                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_bc%kr_out,        &
     &      sph_bc%r_CMB, sph_bc%fdm2_fix_dr_CMB, coef, band_s_evo%mat)
      else
        call set_fix_fld_cmb_poisson_mat                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc%kr_out, band_s_evo%mat)
      end if
!
      call ludcmp_3band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_s_evo)
!
      end subroutine const_radial_mat_4_scalar_sph
!
! -----------------------------------------------------------------------
!
      end module const_r_mat_4_scalar_sph
