!>@file   const_r_mat_w_center_sph.f90
!!@brief  module const_r_mat_w_center_sph
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for time evolution of scalar fields
!!
!!@verbatim
!!      subroutine const_radial_mat_press00_sph                         &
!!     &         (sph_rj, n_vect, n_comp, p_poisson_mat)
!!      subroutine const_radial_mat_temp00_sph                          &
!!     &         (sph_rj, n_vect, n_comp, temp_evo_mat)
!!      subroutine const_radial_mat_comp00_sph                          &
!!     &         (sph_rj, n_vect, n_comp, comp_evo_mat)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!@endverbatim
!
      module const_r_mat_w_center_sph
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_t_int_parameter
!
      use t_spheric_rj_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_press00_sph                           &
     &         (sph_rj, n_vect, n_comp, p_poisson_mat)
!
      use m_boundary_params_sph_MHD
      use m_radial_mat_sph_w_center
      use m_physical_property
      use m_coef_fdm_to_center
      use m_ludcmp_3band
      use center_sph_matrices
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind= kint), intent(in) :: n_vect, n_comp
      real(kind = kreal), intent(in) :: p_poisson_mat(3,n_vect,n_comp)
!
      integer(kind = kint) :: ierr, nri1
      real(kind = kreal) :: coef_p
!
!
      nri1 = sph_rj%nidx_rj(1) + 1
      coef_p = - coef_press
!
      call allocate_press00_mat_sph(sph_rj)
!
      call copy_to_band3_mat_w_center(sph_rj%nidx_rj(1), zero,          &
      &   p_poisson_mat(1,1,sph_rj%idx_rj_degree_zero),                 &
      &   p00_poisson_mat(1,0))
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call add_scalar_poisson_mat_fill_ctr(sph_rj%nidx_rj(1),         &
     &      sph_bc_U%r_ICB, fdm2_fix_dr_center, fdm2_fix_fld_ctr1,      &
     &      coef_p, p00_poisson_mat)
      else
        call add_scalar_poisson_mat_no_fld                              &
     &     (sph_rj%nidx_rj(1), p00_poisson_mat)
      end if
!
      call ludcmp_3band(nri1, p00_poisson_mat(1,0), i_p00_pivot(0),     &
     &    ierr, p00_poisson_lu(1,0), p00_poisson_det(0))
!
      if(i_debug .eq. iflag_full_msg)                                   &
     &     call check_press00_mat_sph(my_rank, sph_rj)
!
      end subroutine const_radial_mat_press00_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_temp00_sph                            &
     &         (sph_rj, n_vect, n_comp, temp_evo_mat)
!
      use m_boundary_params_sph_MHD
      use m_radial_mat_sph_w_center
      use m_physical_property
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind= kint), intent(in) :: n_vect, n_comp
      real(kind = kreal), intent(in) :: temp_evo_mat(3,n_vect,n_comp)
!
!
      call const_radial_mat_scalar00_sph(sph_rj%nidx_rj(1), sph_bc_T,   &
     &    coef_imp_t, coef_temp, coef_d_temp,                           &
     &    temp_evo_mat(1,1,sph_rj%idx_rj_degree_zero), t00_evo_mat,     &
     &    t00_evo_lu, t00_evo_det, i_t00_pivot)
!
      if(i_debug .eq. iflag_full_msg)                                   &
     &     call check_temp00_mat_sph(my_rank, sph_rj)
!
      end subroutine const_radial_mat_temp00_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_comp00_sph                            &
     &         (sph_rj, n_vect, n_comp, comp_evo_mat)
!
      use m_boundary_params_sph_MHD
      use m_radial_mat_sph_w_center
      use m_physical_property
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind= kint), intent(in) :: n_vect, n_comp
      real(kind = kreal), intent(in) :: comp_evo_mat(3,n_vect,n_comp)
!
!
      call const_radial_mat_scalar00_sph(sph_rj%nidx_rj(1), sph_bc_C,   &
     &    coef_imp_c, coef_light, coef_d_light,                         &
     &    comp_evo_mat(1,1,sph_rj%idx_rj_degree_zero),                  &
     &    c00_evo_mat, c00_evo_lu, c00_evo_det, i_c00_pivot)
!
      if(i_debug .eq. iflag_full_msg)                                   &
     &       call check_comp00_mat_sph(my_rank, sph_rj)
!
      end subroutine const_radial_mat_comp00_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_scalar00_sph                          &
     &         (nri, sph_bc, coef_imp, coef_f, coef_d, evo_mat_j0,      &
     &          evo00_mat, evo00_lu, evo00_det, i00_pivot)
!
      use m_coef_fdm_to_center
      use m_ludcmp_3band
      use t_boundary_params_sph_MHD
      use center_sph_matrices
!
      integer(kind = kint), intent(in) :: nri
      type(sph_boundary_type), intent(in) :: sph_bc
      real(kind = kreal) :: coef_imp, coef_f, coef_d
      real(kind = kreal), intent(in) :: evo_mat_j0(3,nri)
!
      real(kind = kreal), intent(inout) :: evo00_mat(3,0:nri)
      real(kind = kreal), intent(inout) :: evo00_lu(5,0:nri)
      real(kind = kreal), intent(inout) :: evo00_det(0:nri)
      integer(kind = kint), intent(inout) :: i00_pivot(0:nri)
!
      integer(kind = kint) :: ierr, nri1
!
      real(kind = kreal) :: coef
!
!
      nri1 = nri + 1
      if(coef_f .eq. zero) then
        coef = one
      else  
        coef = coef_imp * coef_d * dt
      end if
!
      call copy_to_band3_mat_w_center(nri, coef_f,                      &
      &   evo_mat_j0(1,1), evo00_mat(1,0))
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center) then
        call add_scalar_poisson_mat_fill_ctr(nri, sph_bc%r_ICB,         &
     &      fdm2_fix_dr_center, fdm2_fix_fld_ctr1, coef, evo00_mat)
      else if(sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call add_scalar_poisson_mat_fix_ctr(nri, sph_bc%r_ICB,          &
     &      fdm2_fix_fld_ctr1, coef, evo00_mat)
      else
        call add_scalar_poisson_mat_no_fld(nri, evo00_mat)
      end if
!
      call ludcmp_3band(nri1, evo00_mat(1,0), i00_pivot(0), ierr,       &
     &    evo00_lu(1,0), evo00_det(0))
!
      end subroutine const_radial_mat_scalar00_sph
!
! -----------------------------------------------------------------------
!
      end module const_r_mat_w_center_sph
