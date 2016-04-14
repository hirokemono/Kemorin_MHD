!> @file  const_sph_radial_grad.f90
!!      module const_sph_radial_grad
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate radial delivatives
!!
!!@verbatim
!!      subroutine const_radial_grad_scalar                             &
!!     &         (sph_bc, is_fld, is_grad, rj_fld)
!!        Input:    is_fld
!!        Solution: is_grad
!!
!!      subroutine const_grad_vp_and_vorticity(is_velo, is_vort, rj_fld)
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: idpdr%i_velo, ipol%i_vort, itor%i_vort, idpdr%i_vort
!!
!!      subroutine const_grad_bp_and_current(sph_bc_B,                  &
!!     &           is_magne, is_current, rj_fld)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: idpdr%i_magne,
!!                  ipol%i_current, itor%i_current, idpdr%i_current
!!
!!      subroutine const_grad_poloidal_moment(is_fld, rj_fld)
!!        Input:    is_fld, is_fld+2
!!        Solution: is_fld+1
!!
!!      subroutine const_grad_poloidal_magne(sph_bc_B, is_magne, rj_fld)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: idpdr%i_magne
!!
!!      subroutine const_pressure_gradient                              &
!!     &         (sph_bc_U, is_press, is_grad, rj_fld)
!!        Input:    ipol%i_press
!!        Solution: ipol%i_press_grad
!!
!!      subroutine const_sph_gradient_no_bc                             &
!!     &         (sph_bc, is_fld, is_grad, rj_fld)
!!        Input:    is_fld
!!        Solution: is_grad, it_grad, ids_grad
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc  Structure for basic boundary condition parameters
!!@param sph_bc_B  Structure for basic boundary condition parameters
!!                 for magnetic field
!!@param sph_bc_U  Structure for basic boundary condition parameters
!!                 for velocity
!!
!!@param is_fld      Spherical hermonics data address for input vector
!!@param is_grad     Spherical hermonics data address for gradient
!!@param is_velo     Spherical hermonics data address
!!                   for poloidal velocity
!!@param is_vort     Spherical hermonics data address
!!                   for poloidal vorticity
!!@param is_magne    Spherical hermonics data address
!!                   for poloidal magnetic field
!!@param is_current  Spherical hermonics data address
!!                   for poloidal current density
!!@param is_press    Spherical hermonics data address
!!                   for pressure
!!@param rj_fld     Spectr data structure
!
      module const_sph_radial_grad
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_phys_address
!
      use t_phys_data
!
      use cal_sph_exp_1st_diff
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_grad_scalar                               &
     &         (sph_bc, is_fld, is_grad, rj_fld)
!
      use t_boundary_params_sph_MHD
      use select_exp_scalar_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc
      integer(kind = kint), intent(in) :: is_fld, is_grad
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sph_nod_gradient_2(sph_bc%kr_in, sph_bc%kr_out,          &
     &    is_fld, is_grad, rj_fld%ntot_phys, rj_fld%d_fld)
      call sel_bc_radial_grad_scalar(sph_bc, is_fld, is_grad,           &
     &    rj_fld%ntot_phys, rj_fld%d_fld)
      call normalize_sph_average_grad(is_grad,                          &
     &    rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_radial_grad_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_grad_vp_and_vorticity(is_velo, is_vort, rj_fld)
!
      use m_boundary_params_sph_MHD
      use cal_sph_exp_rotation
      use select_exp_velocity_bc
!
      integer(kind = kint), intent(in) :: is_velo, is_vort
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_bc_grad_vp_and_vorticity                                 &
     &   (is_velo, is_vort, rj_fld%ntot_phys, rj_fld%d_fld)
      call cal_sph_diff_pol_and_rot2(sph_bc_U%kr_in, sph_bc_U%kr_out,   &
     &    nidx_rj, sph_rj1%ar_1d_rj, is_velo, is_vort,                  &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_grad_vp_and_vorticity
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_bp_and_current(sph_bc_B,                    &
     &           is_magne, is_current, rj_fld)
!
      use t_boundary_params_sph_MHD
      use extend_potential_field
      use cal_sph_exp_rotation
      use select_exp_magne_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne, is_current
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_bc_grad_bp_and_current(sph_bc_B, is_magne, is_current,   &
     &    rj_fld%ntot_phys, rj_fld%d_fld)
      call cal_sph_diff_pol_and_rot2(sph_bc_B%kr_in, sph_bc_B%kr_out,   &
     &    nidx_rj, sph_rj1%ar_1d_rj, is_magne, is_current,              &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
!      Extend potential field
      call ext_outside_potential_with_j                                 &
     &   (sph_bc_B%kr_out, is_magne, is_current,                        &
     &      nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r,       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call ext_inside_potential_with_j                                &
     &     (sph_bc_B%kr_in, is_magne, is_current,                       &
     &      nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r,       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine const_grad_bp_and_current
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_grad_poloidal_moment(is_fld, rj_fld)
!
      use m_boundary_params_sph_MHD
      use cal_sph_exp_rotation
      use select_exp_velocity_bc
!
      integer(kind = kint), intent(in) :: is_fld
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_bc_grad_poloidal_moment                                  &
     &   (is_fld, rj_fld%ntot_phys, rj_fld%d_fld)
      call cal_sph_diff_poloidal2                                       &
     &   (sph_bc_U%kr_in, sph_bc_U%kr_out, nidx_rj, &
     &    is_fld, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_grad_poloidal_moment
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_poloidal_magne(sph_bc_B, is_magne, rj_fld)
!
      use t_boundary_params_sph_MHD
      use extend_potential_field
      use cal_sph_exp_rotation
      use select_exp_magne_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_bc_grad_poloidal_magne                                   &
     &   (sph_bc_B, is_magne, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call cal_sph_diff_poloidal2                                       &
     &   (sph_bc_B%kr_in, sph_bc_B%kr_out, nidx_rj,                     &
     &    is_magne, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
!      Extend potential field
      call ext_outside_potential(sph_bc_B%kr_out, is_magne,             &
     &      nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r,       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call ext_inside_potential(sph_bc_B%kr_in, is_magne,             &
     &      nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r,       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine const_grad_poloidal_magne
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_pressure_gradient                                &
     &         (sph_bc_U, is_press, is_grad, rj_fld)
!
      use m_physical_property
      use t_boundary_params_sph_MHD
      use cal_sph_exp_nod_none_bc
      use const_wz_coriolis_rtp
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_press, is_grad
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sph_nod_gradient_2(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
     &    is_press, is_grad, rj_fld%ntot_phys, rj_fld%d_fld)
      call normalize_sph_average_grad                                   &
     &   (is_grad, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call delete_bc_rj_vector(nnod_rj, nidx_rj(2), sph_bc_U%kr_in,     &
     &    is_grad, rj_fld%ntot_phys, rj_fld%d_fld)
      call delete_bc_rj_vector(nnod_rj, nidx_rj(2), sph_bc_U%kr_out,    &
     &    is_grad, rj_fld%ntot_phys, rj_fld%d_fld)
!
!$omp parallel
      call ovwrt_rj_coef_prod_vect_smp                                  &
     &   ( (-coef_press), is_grad, rj_fld%ntot_phys, rj_fld%d_fld)
!$omp end parallel
!
      end subroutine const_pressure_gradient
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_gradient_no_bc                               &
     &         (sph_bc, is_fld, is_grad, rj_fld)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_nod_none_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc
      integer(kind = kint), intent(in) :: is_fld, is_grad
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sph_nod_nobc_in_grad2(nnod_rj, nidx_rj(2),               &
     &    sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,          &
     &    is_fld, is_grad, rj_fld%ntot_phys, rj_fld%d_fld)
      call cal_sph_nod_nobc_out_grad2(nnod_rj, nidx_rj(2),              &
     &    sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,         &
     &    is_fld, is_grad, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call cal_sph_nod_gradient_2(sph_bc%kr_in, sph_bc%kr_out,          &
     &    is_fld, is_grad, rj_fld%ntot_phys, rj_fld%d_fld)
      call normalize_sph_average_grad                                   &
     &   (is_grad, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_sph_gradient_no_bc
!
! -----------------------------------------------------------------------
!
      end module const_sph_radial_grad
