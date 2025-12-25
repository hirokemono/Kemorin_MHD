!>@file   cal_vpol_press_sph_fdm_mat.f90
!!@brief  module cal_vpol_press_sph_fdm_mat
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set boundary conditions for MHD dynamo simulation
!!
!!@verbatim
!!@endverbatim
!
      module cal_vpol_press_sph_fdm_mat
!
      use m_precision
      use m_constants
      use t_spheric_rj_data
      use t_schmidt_poly_on_rtm
      use t_fdm_coefs
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &           :: vsp_evo_name = 'velocity_pressure_evolution'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_vpol_press(id_file, dt,               &
     &          flag_viscous_variation, flag_ref_density_valiation,     &
     &          sph_rj, Plm_WK, fl_prop, r_2nd, r_n2e_3rd, r_e2n_1st,   &
     &          sph_bc_U, bcs_U, fdm3e_center, fdm3e_vp0_ICB,           &
     &          fdm3e_free_ICB, fdm3e_vp0_CMB, fdm3e_free_CMB,          &
     &          relative_d, h_nu, h_rho, band_vsp_evo)
!
      use t_physical_property
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_coef_fdm3_n2e_zero_vp_ICB
      use t_coef_fdm3_n2e_free_vp_ICB
      use t_coef_fdm3_n2e_zero_vp_CMB
      use t_coef_fdm3_n2e_free_vp_CMB
      use t_coef_fdm3_n2e_zero_vp_CTR
      use t_sph_matrices
      use m_ludcmp_band
      use check_sph_radial_mat
      use cal_sph_pol_hdiv_viscousity
      use center_sph_matrices
!
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      type(sph_rj_grid), intent(in) :: sph_rj
      type(legendre_4_sph_trans), intent(in) :: Plm_WK
      type(fdm_matrices), intent(in) :: r_2nd
      type(fdm_matrices), intent(in) :: r_n2e_3rd
      type(fdm_matrices), intent(in) :: r_e2n_1st
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_vector_boundary_data), intent(in) :: bcs_U
      type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center
      type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
      type(fdm3_n2e_ICB_free_vpol), intent(in) :: fdm3e_free_ICB
      type(fdm3_n2e_CMB_zero_vpol), intent(in) :: fdm3e_vp0_CMB
      type(fdm3_n2e_CMB_free_vpol), intent(in) :: fdm3e_free_CMB
!
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(in) :: dt
!
      type(band_matrices_type), intent(inout) :: band_vsp_evo
!
!      integer(kind = kint) :: j
      real(kind = kreal) :: coef_dvt
!
!
      band_vsp_evo%mat_name = vsp_evo_name
!
      call alloc_band_matrices_type(iseven, (2*sph_rj%nidx_rj(1)),      &
     &                              sph_rj%nidx_rj(2), band_vsp_evo)
!
      call set_unit_on_diag(band_vsp_evo)
!
      if(fl_prop%coef_velo .eq. zero) then
        coef_dvt = one
      else
        coef_dvt = fl_prop%coef_imp * fl_prop%coef_diffuse * dt
      end if
      call sph_FDM2_vpol_viscosity_mat(sph_bc_U%kr_in, sph_bc_U%kr_out, &
     &    sph_rj, fl_prop, radial_variation,                            &
     &    Plm_WK%g_sph_rj, fl_prop%coef_press, coef_dvt,                &
     &    r_2nd%fdm(1), r_n2e_3rd%fdm(0), r_e2n_1st%fdm(0),             &
     &    mat1_grad_p, mat2_viscous, hdiv_visous_mat,                   &
     &    band_vsp_evo%mat)
!
!   Boundary condition for ICB
!
      if(     (sph_bc_U%iflag_icb .eq. iflag_sph_fill_center)           &
     &   .or. (sph_bc_U%iflag_icb .eq. iflag_sph_filter_center)) then
        call cal_sph_vpol_press_sph_mat_CTR                             &
     &     (fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj, Plm_WK, sph_bc_U, fdm3e_center,                     &
     &      fl_prop%coef_press, coef_dvt, relative_d, h_nu, h_rho,      &
     &      band_vsp_evo)
!
        if(sph_bc_U%iflag_icb .eq. iflag_sph_filter_center) then
          call set_unit_mat7_filter_to_center                           &
     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                     &
     &        bcs_U%ICB_Vspec%Vp_BC, band_vsp_evo%mat)
        end if
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_vpol_press_sph_mat_ICB                             &
     &     (fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj, Plm_WK, sph_bc_U, fdm3e_free_ICB,                   &
     &      fl_prop%coef_press, coef_dvt, relative_d, h_nu, h_rho,      &
     &      band_vsp_evo)
!
!      else if(sph_bc_U%iflag_icb .eq. iflag_evolve_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_non_slip) then
      else
        call cal_sph_vpol_press_sph_mat_ICB                             &
     &     (fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj, Plm_WK, sph_bc_U, fdm3e_vp0_ICB,                    &
     &      fl_prop%coef_press, coef_dvt, relative_d, h_nu, h_rho,      &
     &      band_vsp_evo)
      end if
!
!   Boundary condition for CMB
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_vpol_press_sph_mat_CMB                             &
     &     (fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj, Plm_WK, sph_bc_U, fdm3e_free_CMB,                   &
     &      fl_prop%coef_press, coef_dvt, relative_d, h_nu, h_rho,      &
     &      band_vsp_evo)
!      else if(sph_bc_U%iflag_cmb .eq. iflag_evolve_field) then
!      else if(sph_bc_U%iflag_cmb .eq. iflag_fixed_field) then
!      else if(sph_bc_U%iflag_cmb .eq. iflag_non_slip) then
      else
        call cal_sph_vpol_press_sph_mat_CMB                             &
     &     (fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj, Plm_WK, sph_bc_U, fdm3e_vp0_CMB,                    &
     &      fl_prop%coef_press, coef_dvt, relative_d, h_nu, h_rho,      &
     &      band_vsp_evo)
      end if
!
!
      call ludcmp_7band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_vsp_evo)
!
      if(i_debug .eq. iflag_full_msg) then
        call check_radial_band_mat(id_file, sph_rj, band_vsp_evo)
      end if
!
!      do j = 1, sph_rj%nidx_rj(2)
!        do k = 1, sph_rj%nidx_rj(1)
!          band_vsp_evo%det(j)                                          &
!     &                = band_vsp_evo%det(j) * band_vsp_evo%lu(5,k,j)
!        end do
!        write(id_file,*) 'det vsp', j, band_vsp_evo%det(j)
!      end do
!
      end subroutine const_radial_mat_vpol_press
!
! -----------------------------------------------------------------------
!
      subroutine const_hdiv_vpol_diffusion(dt, sph_rj, Plm_WK,          &
     &          fl_prop, r_2nd, r_n2e_3rd, r_e2n_1st, sph_bc_U,         &
     &          fdm3e_center, fdm3e_vp0_ICB, fdm3e_free_ICB,            &
     &          fdm3e_vp0_CMB, fdm3e_free_CMB, relative_d, h_nu, h_rho, &
     &          press_e, ipol_base, ipol_force, ipol_diffusion,         &
     &          rj_fld, e_hdiv_viscous)
!
      use t_base_field_labels
      use t_base_force_labels
      use t_phys_data
      use t_physical_property
      use t_boundary_params_sph_MHD
      use t_coef_fdm3_n2e_zero_vp_ICB
      use t_coef_fdm3_n2e_free_vp_ICB
      use t_coef_fdm3_n2e_zero_vp_CMB
      use t_coef_fdm3_n2e_free_vp_CMB
      use t_coef_fdm3_n2e_zero_vp_CTR
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(legendre_4_sph_trans), intent(in) :: Plm_WK
      type(fdm_matrices), intent(in) :: r_2nd
      type(fdm_matrices), intent(in) :: r_n2e_3rd
      type(fdm_matrices), intent(in) :: r_e2n_1st
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center
      type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
      type(fdm3_n2e_ICB_free_vpol), intent(in) :: fdm3e_free_ICB
      type(fdm3_n2e_CMB_zero_vpol), intent(in) :: fdm3e_vp0_CMB
      type(fdm3_n2e_CMB_free_vpol), intent(in) :: fdm3e_free_CMB
!
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(in) :: dt
!
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_force
      type(diffusion_address), intent(in) :: ipol_diffusion
      real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!
      type(phys_data), intent(inout) :: rj_fld
      real(kind = kreal), intent(inout)                                 &
     &                   :: e_hdiv_viscous(sph_rj%nnod_rj)
!
!      integer(kind = kint) :: j
      real(kind = kreal) :: coef_dvt
!
!
      if(fl_prop%coef_velo .eq. zero) then
        coef_dvt = one
      else
        coef_dvt = (one - fl_prop%coef_imp) * fl_prop%coef_diffuse * dt
      end if
      call sph_exp_FDM2_vpol_viscosity(sph_bc_U%kr_in, sph_bc_U%kr_out, &
     &   (sph_rj, fl_prop, radial_variation,                            &
     &    Plm_WK%g_sph_rj, fl_prop%coef_press, coef_dvt,                &
     &    r_2nd%fdm(1), r_n2e_3rd%fdm(0), r_e2n_1st%fdm(0),             &
     &    rj_fld%d_fld(1,ipol_base%i_velo), press_e,                    &
     &    mat1_grad_p, mat2_viscous, hdiv_visous_mat,                   &
     &    rj_fld%d_fld(1,ipol_base%ipol_diffusion), e_hdiv_viscous)
!
!   Boundary condition for ICB
!
      if(     (sph_bc_U%iflag_icb .eq. iflag_sph_fill_center)           &
     &   .or. (sph_bc_U%iflag_icb .eq. iflag_sph_filter_center)) then
        call cal_exp_sph_vp_val_diffuse_CTR                             &
     &     (fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj, Plm_WK, sph_bc_U, fdm3e_center,                     &
     &      fl_prop%coef_press, coef_dvt, relative_d, h_nu, h_rho,      &
     &      ipol_base, rj_fld, e_hdiv_viscous)
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_exp_sph_vp_val_diffuse_ICB                             &
     &     (fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj, Plm_WK, sph_bc_U, fdm3e_free_ICB,                   &
     &      fl_prop%coef_press, coef_dvt, relative_d, h_nu, h_rho,      &
     &      ipol_base, rj_fld, e_hdiv_viscous)
!
!      else if(sph_bc_U%iflag_icb .eq. iflag_evolve_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_non_slip) then
      else
        call cal_exp_sph_vp_val_diffuse_ICB                             &
     &     (fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj, Plm_WK, sph_bc_U, fdm3e_vp0_ICB,                    &
     &      fl_prop%coef_press, coef_dvt, relative_d, h_nu, h_rho,      &
     &      ipol_base, rj_fld, e_hdiv_viscous)
      end if
!
!   Boundary condition for CMB
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_exp_sph_vp_val_diffuse_CMB                             &
     &     (fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj, Plm_WK, sph_bc_U, fdm3e_free_CMB,                   &
     &      fl_prop%coef_press, coef_dvt, relative_d, h_nu, h_rho,      &
     &      ipol_base, rj_fld, e_hdiv_viscous)
!      else if(sph_bc_U%iflag_cmb .eq. iflag_evolve_field) then
!      else if(sph_bc_U%iflag_cmb .eq. iflag_fixed_field) then
!      else if(sph_bc_U%iflag_cmb .eq. iflag_non_slip) then
      else
        call cal_exp_sph_vp_val_diffuse_CMB                             &
     &     (fl_prop%flag_viscous_variation,                             &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj, Plm_WK, sph_bc_U, fdm3e_vp0_CMB,                    &
     &      fl_prop%coef_press, coef_dvt, relative_d, h_nu, h_rho,      &
     &      ipol_base, rj_fld, e_hdiv_viscous)
      end if
!
      end subroutine const_hdiv_vpol_diffusion
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_vpol_press_sph_matrix                          &
     &         (flag_viscous_variation, flag_ref_density_valiation,     &
     &          sph_rj, Plm_WK, sph_bc_U, coef_p, coef_d, relative_d,   &
     &          h_nu, h_rho, r_2nd, r_n2e_3rd, r_e2n_1st,               &
     &          band_vsp_evo)
!
      use t_boundary_params_sph_MHD
      use t_sph_matrices
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: Plm_WK
      type(sph_boundary_type), intent(in) :: sph_bc_U
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      type(fdm_matrices), intent(in) :: r_2nd
      type(fdm_matrices), intent(in) :: r_n2e_3rd
      type(fdm_matrices), intent(in) :: r_e2n_1st
!
      type(band_matrices_type), intent(inout)  :: band_vsp_evo
!
!
!
!
      end subroutine cal_sph_vpol_press_sph_matrix
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_vpol_press_sph_mat_CMB                         &
     &         (flag_viscous_variation, flag_ref_density_valiation,     &
     &          sph_rj, Plm_WK, sph_bc_U, fdm3e_vp0_CMB,                &
     &          coef_p, coef_d, relative_d, h_nu, h_rho, band_vsp_evo)
!
      use t_boundary_params_sph_MHD
      use t_coef_fdm3_n2e_zero_vp_CMB
      use t_sph_matrices
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: Plm_WK
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm3_n2e_CMB_zero_vpol), intent(in) :: fdm3e_vp0_CMB
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
!
      type(band_matrices_type), intent(inout)  :: band_vsp_evo
!
!
      call set_vpol_press_sph_CMB_mat(sph_rj, sph_bc_U%kr_out,          &
     &    Plm_WK%g_sph_rj, coef_p, coef_d, fdm3e_vp0_CMB%dmat_vp0,      &
     &    band_vsp_evo%mat)
!
      if(flag_viscous_variation) then
        call add_val_viscosity_sph_CMB_mat(sph_rj, sph_bc_U%kr_out,     &
     &      Plm_WK%g_sph_rj, coef_d, relative_d,                        &
     &      h_nu, fdm3e_vp0_CMB%dmat_vp0, band_vsp_evo%mat)
      end if
!
      if(flag_ref_density_valiation) then
        call add_val_density_sph_CMB_mat(sph_rj, sph_bc_U%kr_out,       &
     &      Plm_WK%g_sph_rj, coef_d, relative_d,                        &
     &      h_nu, h_rho, fdm3e_vp0_CMB%dmat_vp0, band_vsp_evo%mat)
      end if
!
      end subroutine cal_sph_vpol_press_sph_mat_CMB
!
! -----------------------------------------------------------------------
!
      subroutine cal_exp_sph_vp_val_diffuse_CMB                         &
     &         (flag_viscous_variation, flag_ref_density_valiation,     &
     &          sph_rj, Plm_WK, sph_bc_U, fdm3e_vp0_CMB,                &
     &          coef_p, coef_d, relative_d, h_nu, h_rho,                &
     &          ipol_base, rj_fld, e_hdiv_viscous)
!
      use t_boundary_params_sph_MHD
      use t_coef_fdm3_n2e_zero_vp_CMB
      use t_base_field_labels
      use t_base_force_labels
      use t_phys_data
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: Plm_WK
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm3_n2e_CMB_zero_vpol), intent(in) :: fdm3e_vp0_CMB
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      type(base_field_address), intent(in) :: ipol_base
      type(phys_data), intent(in) :: rj_fld
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: e_hdiv_viscous(sph_rj%nnod_rj)
!
!
      call set_exp_sph_hdiv_viscous_CMB                                 &
     &   (sph_rj, sph_bc_U%kr_out, Plm_WK%g_sph_rj, coef_d,             &
     &    fdm3e_vp0_CMB%dmat_vp0, ipol_base%i_velo,                     &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld,               &
     &    e_hdiv_viscous)

      if(flag_viscous_variation) then
        call add_exp_sph_hdiv_val_nu_CMB(sph_rj, sph_bc_U%kr_out,       &
     &      Plm_WK%g_sph_rj, coef_d, relative_d,                        &
     &      h_nu, fdm3e_vp0_CMB%dmat_vp0, ipol_base%i_velo,             &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld,             &
     &      e_hdiv_viscous)
      end if
!
      if(flag_ref_density_valiation) then
        call add_exp_sph_hdiv_val_rho_CMB(sph_rj, sph_bc_U%kr_out,      &
     &      Plm_WK%g_sph_rj, coef_d, relative_d, h_nu, h_rho,           &
     &      fdm3e_vp0_CMB%dmat_vp0, ipol_base%i_velo,                   &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld,             &
     &      e_hdiv_viscous)
      end if
!
      end subroutine cal_exp_sph_vp_val_diffuse_CMB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_vpol_press_sph_mat_ICB                         &
     &         (flag_viscous_variation, flag_ref_density_valiation,     &
     &          sph_rj, Plm_WK, sph_bc_U, fdm3e_vp0_ICB,                &
     &          coef_p, coef_d, relative_d, h_nu, h_rho, band_vsp_evo)
!
      use t_boundary_params_sph_MHD
      use t_coef_fdm3_n2e_zero_vp_ICB
      use t_sph_matrices
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: Plm_WK
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
!
      type(band_matrices_type), intent(inout)  :: band_vsp_evo
!
!
      call set_vpol_press_sph_ICB_mat                                   &
     &   (sph_rj, sph_bc_U%kr_in, Plm_WK%g_sph_rj,                      &
     &    coef_p, coef_d, fdm3e_vp0_ICB%dmat_vp0, band_vsp_evo%mat)
!
      if(flag_viscous_variation) then
        call add_val_viscosity_sph_ICB_mat                              &
     &     (sph_rj, sph_bc_U%kr_in, Plm_WK%g_sph_rj, coef_d,            &
     &      relative_d, h_nu, fdm3e_vp0_ICB%dmat_vp0, band_vsp_evo%mat)
      end if
!
      if(flag_ref_density_valiation) then
        call add_val_density_sph_ICB_mat(sph_rj, sph_bc_U%kr_in,        &
     &      Plm_WK%g_sph_rj, coef_d, relative_d,                        &
     &      h_nu, h_rho, fdm3e_vp0_ICB%dmat_vp0, band_vsp_evo%mat)
      end if
!
      end subroutine cal_sph_vpol_press_sph_mat_ICB
!
! -----------------------------------------------------------------------
!
      subroutine cal_exp_sph_vp_val_diffuse_ICB                         &
     &         (flag_viscous_variation, flag_ref_density_valiation,     &
     &          sph_rj, Plm_WK, sph_bc_U, fdm3e_vp0_ICB,                &
     &          coef_p, coef_d, relative_d, h_nu, h_rho, ipol_base,     &
     &          rj_fld, e_hdiv_viscous)
!
      use t_boundary_params_sph_MHD
      use t_coef_fdm3_n2e_zero_vp_ICB
      use t_base_field_labels
      use t_base_force_labels
      use t_phys_data
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: Plm_WK
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      type(base_field_address), intent(in) :: ipol_base
!
      type(phys_data), intent(inout) :: rj_fld
      real(kind = kreal), intent(inout)                                 &
     &                   :: e_hdiv_viscous(sph_rj%nnod_rj)
!
!
      call set_exp_sph_hdiv_viscous_ICB                                 &
     &   (sph_rj, sph_bc_U%kr_in, Plm_WK%g_sph_rj, coef_d,              &
     &    fdm3e_vp0_ICB%dmat_vp0, ipol_base%i_velo,                     &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld,               &
     &    e_hdiv_viscous)

      if(flag_viscous_variation) then
        call add_exp_sph_hdiv_val_nu_ICB                                &
     &     (sph_rj, sph_bc_U%kr_in, Plm_WK%g_sph_rj, coef_d,            &
     &      relative_d, h_nu, fdm3e_vp0_ICB%dmat_vp0, ipol_base%i_velo, &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld,             &
     &      e_hdiv_viscous)
      end if
!
      if(flag_ref_density_valiation) then
        call add_exp_sph_hdiv_val_rho_ICB                               &
     &     (sph_rj, sph_bc_U%kr_in, Plm_WK%g_sph_rj,                    &
     &      coef_d, relative_d, h_nu, h_rho,                            &
     &      fdm3e_vp0_ICB%dmat_vp0, ipol_base%i_velo,                   &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld,             &
     &      e_hdiv_viscous)
      end if
!
      end subroutine cal_exp_sph_vp_val_diffuse_ICB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_vpol_press_sph_mat_CTR                         &
     &         (flag_viscous_variation, flag_ref_density_valiation,     &
     &          sph_rj, Plm_WK, sph_bc_U, fdm3e_center,                 &
     &          coef_p, coef_d, relative_d, h_nu, h_rho,                &
     &          band_vsp_evo)
!
      use t_boundary_params_sph_MHD
      use t_coef_fdm3_n2e_zero_vp_CTR
      use t_sph_matrices
      use sph_hdiv_viscousity_CTR
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: Plm_WK
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
!
      type(band_matrices_type), intent(inout)  :: band_vsp_evo
!
      real(kind = kreal) :: hdiv_visous_j(0:1,sph_rj%nidx_rj(2))
!
!
      call set_sph_vpol_press_CTR_mat7                                  &
     &  (flag_viscous_variation, flag_ref_density_valiation,            &
     &   sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                          &
     &   sph_rj%radius_1d_rj_r(1), Plm_WK%g_sph_rj, coef_p, coef_d,     &
     &   relative_d(1), h_nu(1), h_rho(1,0), h_rho(1,1),                &
     &   fdm3e_center%dmat_vp0, hdiv_visous_j, band_vsp_evo%mat)
!
      end subroutine cal_sph_vpol_press_sph_mat_CTR
!
! -----------------------------------------------------------------------
!
      subroutine cal_exp_sph_vp_val_diffuse_CTR                         &
     &         (flag_viscous_variation, flag_ref_density_valiation,     &
     &          sph_rj, Plm_WK, sph_bc_U, fdm3e_center,                 &
     &          coef_p, coef_d, relative_d, h_nu, h_rho,                &
     &          ipol_base, rj_fld, e_hdiv_viscous)
!
      use t_boundary_params_sph_MHD
      use t_coef_fdm3_n2e_zero_vp_CTR
      use t_base_field_labels
      use t_base_force_labels
      use t_phys_data
      use sph_hdiv_viscousity_CTR
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: Plm_WK
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      type(base_field_address), intent(in) :: ipol_base
!
      type(phys_data), intent(inout) :: rj_fld
      real(kind = kreal), intent(inout)                                 &
     &                   :: e_hdiv_viscous(sph_rj%nnod_rj)
!
      real(kind = kreal) :: hdiv_visous_j(0:1,sph_rj%nidx_rj(2))
!
!
      call set_sph_exp_hdiv_viscous_CTR                                 &
     &  (flag_viscous_variation, flag_ref_density_valiation,            &
     &   sph_rj%nnod_rj, sph_rj%nidx_rj(2),                             &
     &   sph_rj%radius_1d_rj_r(1), Plm_WK%g_sph_rj, coef_p, coef_d,     &
     &   relative_d(1), h_nu(1), h_rho(1,0), h_rho(1,1),                &
     &   fdm3e_center%dmat_vp0, rj_fld%d_fld(1,ipol_base%i_velo),       &
     &   hdiv_visous_j, band_vsp_evo%mat)
!
      end subroutine cal_exp_sph_vp_val_diffuse_CTR
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_vpol_press_sph_CMB_mat(sph_rj, kr_out, g_sph_rj,   &
     &          coef_p, coef_d, fdm3e_CMB_mat, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: fdm3e_CMB_mat(-2:1,4)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3)
      real(kind = kreal) :: c_d3, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_out
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d3 = -one
!$omp parallel do private (j,c_d1,c_d0,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          c_d1 =        g_sph_rj(j,3)*ar_mid(2)
          c_d0 = -two * g_sph_rj(j,3)*ar_mid(3)
          hdiv_visous(-2:1) = coef_d * (c_d3 * fdm3e_CMB_mat(-2:1,4)    &
     &                                + c_d1 * fdm3e_CMB_mat(-2:1,2)    &
     &                                + c_d0 * fdm3e_CMB_mat(-2:1,1))
!
          if((2*k-4) .gt. 0) then
            mat7(7,2*k-4,j) = - hdiv_visous(-2)
          end if
          mat7(6,2*k-3,j) = zero
          mat7(5,2*k-2,j) =   - hdiv_visous(-1)
          mat7(4,2*k-1,j) = coef_p
          mat7(3,2*k,  j) =   - hdiv_visous( 0)
!          mat7(2,2*k+1,j) = zero
!          mat7(1,2*k+2,j) =  - hdiv_visous( 1)
        end do
!$omp end parallel do
!
!$omp parallel do private(j)
        do j = 1, sph_rj%nidx_rj(2)
          if((2*kr_out-3) .gt. 0) mat7(7,2*kr_out-3,j) = zero
          if((2*kr_out-2) .gt. 0) mat7(6,2*kr_out-2,j) = zero
          mat7(5,2*kr_out-1,j) = zero
          mat7(4,2*kr_out,  j) = one
          if((2*kr_out+1) .le. 2*sph_rj%nidx_rj(1))                     &
     &                        mat7(3,2*kr_out+1,j) = zero
          if((2*kr_out+2) .le. 2*sph_rj%nidx_rj(1))                     &
     &                        mat7(2,2*kr_out+2,j) = zero
          if((2*kr_out+3) .le. 2*sph_rj%nidx_rj(1))                     &
     &                        mat7(1,2*kr_out+3,j) = zero
        end do
!$omp end parallel do
!
      end subroutine set_vpol_press_sph_CMB_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_exp_sph_hdiv_viscous_CMB(sph_rj, kr_out,           &
     &          g_sph_rj, coef_d, fdm3e_CMB_mat, is_velo,               &
     &          n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: fdm3e_CMB_mat(-2:1,4)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_n2, i_n1
      real(kind = kreal) :: r_mid, ar_mid(3)
      real(kind = kreal) :: c_d3, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_out
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d3 = -one
!$omp  parallel do                                                      &
!$omp& private(j,c_d1,c_d0,iele,inod,i_n1,i_n2,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          iele = j + (k-1) * sph_rj%nidx_rj(2)
          inod = iele
          i_n1 = iele - sph_rj%nidx_rj(2)
          i_n2 = i_n1 - sph_rj%nidx_rj(2)
!
          c_d1 =        g_sph_rj(j,3)*ar_mid(2)
          c_d0 = -two * g_sph_rj(j,3)*ar_mid(3)
          hdiv_visous(-2:1) = coef_d * (c_d3 * fdm3e_CMB_mat(-2:1,4)    &
     &                                + c_d1 * fdm3e_CMB_mat(-2:1,2)    &
     &                                + c_d0 * fdm3e_CMB_mat(-2:1,1))
!
          e_hdiv_viscous(iele) =  hdiv_visous(-2) * d_rj(i_n2,is_velo)  &
     &                          + hdiv_visous(-1) * d_rj(i_n1,is_velo)  &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)
        end do
!$omp end parallel do
!
      end subroutine set_exp_sph_hdiv_viscous_CMB
!
! -----------------------------------------------------------------------
!
      subroutine add_val_viscosity_sph_CMB_mat                          &
     &         (sph_rj, kr_out, g_sph_rj, coef_d, relative_d, h_nu,     &
     &          fdm3e_CMB_mat, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: fdm3e_CMB_mat(-2:1,4)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_out
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = - h_nu(k)
        c_d1 = two * ar_mid(1) * h_nu(k)
!$omp parallel do private(j,c_d0,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_nu(k)
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_CMB_mat(-2:1,4)    &
     &                                + c_d1 * fdm3e_CMB_mat(-2:1,2)    &
     &                                + c_d0 * fdm3e_CMB_mat(-2:1,1))
!
          if((2*k-4) .gt. 0) then
            mat7(7,2*k-4,j) = d_mid* mat7(7,2*k-4,j) - hdiv_visous(-2)
          end if
          mat7(5,2*k-2,j) =   d_mid* mat7(5,2*k-2,j) - hdiv_visous(-1)
!          mat7(4,2*k-1,j) =  coef_p
          mat7(3,2*k,  j) =   d_mid* mat7(3,2*k,  j) - hdiv_visous( 0)
!          mat7(1,2*k+2,j) =  d_mid* mat7(1,2*k+2,j) - hdiv_visous( 1)
        end do
!$omp end parallel do
!
      end subroutine add_val_viscosity_sph_CMB_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_exp_sph_hdiv_val_nu_CMB(sph_rj, kr_out, g_sph_rj,  &
     &          coef_d, relative_d, h_nu, fdm3e_CMB_mat,                &
     &          is_velo, n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: fdm3e_CMB_mat(-2:1,4)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_n2, i_n1
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_out
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = - h_nu(k)
        c_d1 = two * ar_mid(1) * h_nu(k)
!$omp parallel do private(j,c_d0,iele,inod,i_n1,i_n2,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          iele = j + (k-1) * sph_rj%nidx_rj(2)
          inod = iele
          i_n1 = iele - sph_rj%nidx_rj(2)
          i_n2 = i_n1 - sph_rj%nidx_rj(2)
!
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_nu(k)
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_CMB_mat(-2:1,4)    &
     &                                + c_d1 * fdm3e_CMB_mat(-2:1,2)    &
     &                                + c_d0 * fdm3e_CMB_mat(-2:1,1))
!
          e_hdiv_viscous(iele) =  d_mid * e_hdiv_viscous(iele)          &
     &                          + hdiv_visous(-2) * d_rj(i_n2,is_velo)  &
     &                          + hdiv_visous(-1) * d_rj(i_n1,is_velo)  &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)
        end do
!$omp end parallel do
!
      end subroutine add_exp_sph_hdiv_val_nu_CMB
!
! -----------------------------------------------------------------------
!
      subroutine add_val_density_sph_CMB_mat(sph_rj, kr_out, g_sph_rj,  &
     &          coef_d, relative_d, h_nu, h_rho, fdm3e_CMB_mat, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(in) :: fdm3e_CMB_mat(-2:1,4)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_out
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = h_rho(k,0)
        c_d1 = two * ar_mid(1) * h_rho(k,0)  + h_rho(k,1)               &
     &        + h_nu(k) * h_rho(k,0)
!$omp parallel do private(j,c_d0,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_rho(k,0) * two / three
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_CMB_mat(-2:1,4)    &
     &                                + c_d1 * fdm3e_CMB_mat(-2:1,2)    &
     &                                + c_d0 * fdm3e_CMB_mat(-2:1,1))
!
          if((2*k-4) .gt. 0) then
            mat7(7,2*k-4,j) = mat7(7,2*k-4,j) - hdiv_visous(-2)
          end if
          mat7(5,2*k-2,j) =   mat7(5,2*k-2,j) - hdiv_visous(-1)
!          mat7(4,2*k-1,j) =   coef_p
          mat7(3,2*k,  j) =   mat7(3,2*k,  j) - hdiv_visous( 0)
!          mat7(1,2*k+2,j) =  mat7(1,2*k+2,j) - hdiv_visous( 1)
        end do
!$omp end parallel do
!
      end subroutine add_val_density_sph_CMB_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_exp_sph_hdiv_val_rho_CMB(sph_rj, kr_out, g_sph_rj, &
     &          coef_d, relative_d, h_nu, h_rho, fdm3e_CMB_mat,         &
     &          is_velo, n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(in) :: fdm3e_CMB_mat(-2:1,4)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_n2, i_n1
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_out
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = h_rho(k,0)
        c_d1 = two * ar_mid(1) * h_rho(k,0)  + h_rho(k,1)               &
     &        + h_nu(k) * h_rho(k,0)
!$omp parallel do private(j,c_d0,iele,inod,i_n1,i_n2,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          iele = j + (k-1) * sph_rj%nidx_rj(2)
          inod = iele
          i_n1 = iele - sph_rj%nidx_rj(2)
          i_n2 = i_n1 - sph_rj%nidx_rj(2)
!
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_rho(k,0) * two / three
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_CMB_mat(-2:1,4)    &
     &                                + c_d1 * fdm3e_CMB_mat(-2:1,2)    &
     &                                + c_d0 * fdm3e_CMB_mat(-2:1,1))
!
          e_hdiv_viscous(iele) = e_hdiv_viscous(iele)                   &
     &                          + hdiv_visous(-2) * d_rj(i_n2,is_velo)  &
     &                          + hdiv_visous(-1) * d_rj(i_n1,is_velo)  &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)
        end do
!$omp end parallel do
!
      end subroutine add_exp_sph_hdiv_val_rho_CMB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_vpol_press_sph_ICB_mat(sph_rj, kr_in, g_sph_rj,    &
     &          coef_p, coef_d, fdm3e_ICB_mat, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: fdm3e_ICB_mat(-2:1,4)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3)
      real(kind = kreal) :: c_d3, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_in+1
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d3 = -one
!$omp parallel do private(j,c_d1,c_d0,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          c_d1 =        g_sph_rj(j,3)*ar_mid(2)
          c_d0 = -two * g_sph_rj(j,3)*ar_mid(3)
          hdiv_visous(-2:1) = coef_d * (c_d3 * fdm3e_ICB_mat(-2:1,4)    &
     &                                + c_d1 * fdm3e_ICB_mat(-2:1,2)    &
     &                                + c_d0 * fdm3e_ICB_mat(-2:1,1))
!
!          if((2*k-4) .gt. 0) mat7(7,2*k-4,j) = - hdiv_visous(-2)
          mat7(6,2*k-3,j) =   zero
          mat7(5,2*k-2,j) = - hdiv_visous(-1)
!
          mat7(4,2*k-1,j) =   coef_p
!
          mat7(3,2*k,  j) = - hdiv_visous(0)
          mat7(2,2*k+1,j) =   zero
          mat7(1,2*k+2,j) = - hdiv_visous(1)
        end do
!$omp end parallel do

!$omp parallel do private(j)
        do j = 1, sph_rj%nidx_rj(2)
          if((2*kr_in-3) .gt. 0) mat7(7,2*kr_in-3,j) = zero
          if((2*kr_in-2) .gt. 0) mat7(6,2*kr_in-2,j) = zero
          mat7(5,2*kr_in-1,j) = zero
          mat7(4,2*kr_in,  j) = one
          if((2*kr_in+1) .le. 2*sph_rj%nidx_rj(1))                      &
     &                       mat7(3,2*kr_in+1,j) = zero
          if((2*kr_in+2) .le. 2*sph_rj%nidx_rj(1))                      &
     &                       mat7(2,2*kr_in+2,j) = zero
          if((2*kr_in+3) .le. 2*sph_rj%nidx_rj(1))                      &
     &                       mat7(1,2*kr_in+3,j) = zero
        end do
!$omp end parallel do
!
      end subroutine set_vpol_press_sph_ICB_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_exp_sph_hdiv_viscous_ICB(sph_rj, kr_in,            &
     &          g_sph_rj, coef_d, fdm3e_ICB_mat, is_velo,               &
     &          n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: fdm3e_ICB_mat(-2:1,4)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_n1, i_p1
      real(kind = kreal) :: r_mid, ar_mid(3)
      real(kind = kreal) :: c_d3, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_in+1
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d3 = -one
!$omp parallel do private(j,c_d1,c_d0,iele,i_p1,inod,i_n1,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          iele = j + (k-1) * sph_rj%nidx_rj(2)
          i_p1 = iele + sph_rj%nidx_rj(2)
          inod = iele
          i_n1 = iele - sph_rj%nidx_rj(2)
!
          c_d1 =        g_sph_rj(j,3)*ar_mid(2)
          c_d0 = -two * g_sph_rj(j,3)*ar_mid(3)
          hdiv_visous(-2:1) = coef_d * (c_d3 * fdm3e_ICB_mat(-2:1,4)    &
     &                                + c_d1 * fdm3e_ICB_mat(-2:1,2)    &
     &                                + c_d0 * fdm3e_ICB_mat(-2:1,1))
!
          e_hdiv_viscous(iele) =  hdiv_visous(-1) * d_rj(i_n1,is_velo)  &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)  &
     &                          + hdiv_visous( 1) * d_rj(i_p1,is_velo)
        end do
!$omp end parallel do

      end subroutine set_exp_sph_hdiv_viscous_ICB
!
! -----------------------------------------------------------------------
!
      subroutine add_val_viscosity_sph_ICB_mat                          &
     &         (sph_rj, kr_in, g_sph_rj, coef_d, relative_d, h_nu,      &
     &          fdm3e_ICB_mat, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: fdm3e_ICB_mat(-2:1,4)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_in+1
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = - h_nu(k)
        c_d1 = two * ar_mid(1) * h_nu(k)
!$omp parallel do private(j,c_d0,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_nu(k)
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_ICB_mat(-2:1,3)    &
     &                                + c_d1 * fdm3e_ICB_mat(-2:1,2)    &
     &                                + c_d0 * fdm3e_ICB_mat(-2:1,1))
!
!          if((2*k-4) .gt. 0) then
!            mat7(7,2*k-4,j) = d_mid * mat7(7,2*k-4,j) - hdiv_visous(-2)
!          end if
          mat7(5,2*k-2,j) = d_mid * mat7(5,2*k-2,j) - hdiv_visous(-1)
!          mat7(4,2*k-1,j) = coef_p
          mat7(3,2*k,  j) = d_mid * mat7(3,2*k,  j) - hdiv_visous( 0)
          mat7(1,2*k+2,j) = d_mid * mat7(1,2*k+2,j) - hdiv_visous( 1)
        end do
!$omp end parallel do
!
      end subroutine add_val_viscosity_sph_ICB_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_exp_sph_hdiv_val_nu_ICB(sph_rj, kr_in, g_sph_rj,   &
     &          coef_d, relative_d, h_nu, fdm3e_ICB_mat, is_velo,       &
     &          n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: fdm3e_ICB_mat(-2:1,4)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_n1, i_p1
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_in+1
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = - h_nu(k)
        c_d1 = two * ar_mid(1) * h_nu(k)
!$omp parallel do private(j,c_d0,i_p1,iele,inod,i_n1,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          iele = j + (k-1) * sph_rj%nidx_rj(2)
          i_p1 = iele + sph_rj%nidx_rj(2)
          inod = iele
          i_n1 = iele - sph_rj%nidx_rj(2)
!
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_nu(k)
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_ICB_mat(-2:1,3)    &
     &                                + c_d1 * fdm3e_ICB_mat(-2:1,2)    &
     &                                + c_d0 * fdm3e_ICB_mat(-2:1,1))
!
          e_hdiv_viscous(iele) = d_mid * e_hdiv_viscous(iele)           &
     &                          + hdiv_visous(-1) * d_rj(i_n1,is_velo)  &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)  &
     &                          + hdiv_visous( 1) * d_rj(i_p1,is_velo)
        end do
!$omp end parallel do
!
      end subroutine add_exp_sph_hdiv_val_nu_ICB
!
! -----------------------------------------------------------------------
!
      subroutine add_val_density_sph_ICB_mat(sph_rj, kr_in, g_sph_rj,   &
     &          coef_d, relative_d, h_nu, h_rho, fdm3e_ICB_mat, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(in) :: fdm3e_ICB_mat(-2:1,4)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_in+1
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = h_rho(k,0)
        c_d1 = two * ar_mid(1) * h_rho(k,0)  + h_rho(k,1)               &
     &        + h_nu(k) * h_rho(k,0)
!$omp parallel do private(j,c_d0,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_rho(k,0) * two / three
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_ICB_mat(-2:1,3)    &
     &                                + c_d1 * fdm3e_ICB_mat(-2:1,2)    &
     &                                + c_d0 * fdm3e_ICB_mat(-2:1,1))
!
!          if((2*k-4) .gt. 0) then
!            mat7(7,2*k-4,j) = mat7(7,2*k-4,j) - hdiv_visous(-2)
!          end if
          mat7(5,2*k-2,j) = mat7(5,2*k-2,j) - hdiv_visous(-1)
!          mat7(4,2*k-1,j) = coef_p
          mat7(3,2*k,  j) = mat7(3,2*k,  j) - hdiv_visous( 0)
          mat7(1,2*k+2,j) = mat7(1,2*k+2,j) - hdiv_visous( 1)
        end do
!$omp end parallel do
!
      end subroutine add_val_density_sph_ICB_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_exp_sph_hdiv_val_rho_ICB(sph_rj, kr_in, g_sph_rj,  &
     &          coef_d, relative_d, h_nu, h_rho, fdm3e_ICB_mat,         &
     &          is_velo, n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(in) :: fdm3e_ICB_mat(-2:1,4)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_n1, i_p1
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_in+1
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = h_rho(k,0)
        c_d1 = two * ar_mid(1) * h_rho(k,0)  + h_rho(k,1)               &
     &        + h_nu(k) * h_rho(k,0)
!$omp parallel do private(j,c_d0,iele,i_p1,inod,i_n1,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          iele = j + (k-1) * sph_rj%nidx_rj(2)
          i_p1 = iele + sph_rj%nidx_rj(2)
          inod = iele
          i_n1 = iele - sph_rj%nidx_rj(2)
!
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_rho(k,0) * two / three
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_ICB_mat(-2:1,3)    &
     &                                + c_d1 * fdm3e_ICB_mat(-2:1,2)    &
     &                                + c_d0 * fdm3e_ICB_mat(-2:1,1))
!
          e_hdiv_viscous(iele) = e_hdiv_viscous(iele)                   &
     &                          + hdiv_visous(-1) * d_rj(i_n1,is_velo)  &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)  &
     &                          + hdiv_visous( 1) * d_rj(i_p1,is_velo)
        end do
!$omp end parallel do
!
      end subroutine add_exp_sph_hdiv_val_rho_ICB
!
! -----------------------------------------------------------------------
!
      end module cal_vpol_press_sph_fdm_mat
