!> @file  cal_energy_flux_rj.f90
!!      module cal_energy_flux_rj
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2009
!! @n    Modified in Apr., 2013
!
!> @brief Evaluate energy fluxes for MHD dynamo in physical space
!!
!!@verbatim
!!      subroutine s_cal_energy_flux_rj                                 &
!!     &         (ltr_crust, sph_rj, r_2nd, sph_MHD_bc, ipol, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module cal_energy_flux_rj
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_boundary_data_sph_MHD
!
      implicit  none
!
      private :: truncate_magnetic_field_4_view
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_energy_flux_rj                                   &
     &         (ltr_crust, sph_rj, r_2nd, sph_MHD_bc, ipol, rj_fld)
!
      use const_sph_radial_grad
      use copy_nodal_fields
!
      integer(kind = kint), intent(in) :: ltr_crust
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol%i_rot_Coriolis .gt. 0) then
        call const_grad_poloidal_moment                                 &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, sph_MHD_bc%bc_Uspec,    &
     &      sph_MHD_bc%fdm2_free_ICB, sph_MHD_bc%fdm2_free_CMB,         &
     &      ipol%i_rot_Coriolis, rj_fld)
      end if
!
!
      if(ipol%i_geostrophic .gt. 0) then
        call add_2_nod_vectors                                          &
     &     (rj_fld, ipol%i_coriolis, ipol%i_press_grad,                 &
     &      ipol%i_geostrophic)
      end if
!
!
      if(ipol%i_h_flux_w_sgs .gt. 0) then
        call add_2_nod_vectors                                          &
     &     (rj_fld, ipol%i_h_flux, ipol%i_SGS_h_flux,                   &
     &      ipol%i_h_flux_w_sgs)
      end if
!
      if(ipol%i_c_flux_w_sgs .gt. 0) then
        call add_2_nod_vectors                                          &
     &     (rj_fld, ipol%i_c_flux, ipol%i_SGS_c_flux,                   &
     &      ipol%i_c_flux_w_sgs)
      end if
!
      if(ipol%i_inertia_w_sgs .gt. 0) then
        call add_2_nod_vectors                                          &
     &     (rj_fld, ipol%i_m_advect, ipol%i_SGS_inertia,                &
     &      ipol%i_inertia_w_sgs)
      end if
!
      if(ipol%i_Lorentz_w_sgs .gt. 0) then
        call add_2_nod_vectors                                          &
     &     (rj_fld, ipol%i_lorentz, ipol%i_SGS_Lorentz,                 &
     &      ipol%i_Lorentz_w_sgs)
      end if
!
      if(ipol%i_vp_induct_w_sgs .gt. 0) then
        call add_2_nod_vectors                                          &
     &     (rj_fld, ipol%i_vp_induct, ipol%i_SGS_vp_induct,             &
     &      ipol%i_vp_induct_w_sgs)
      end if
!
      if(ipol%i_mag_induct_w_sgs .gt. 0) then
        call add_2_nod_vectors                                          &
     &     (rj_fld, ipol%i_induction, ipol%i_SGS_induction,             &
     &      ipol%i_mag_induct_w_sgs)
      end if
!
      if(ipol%i_mom_flux_w_sgs .gt. 0) then
        call add_2_nod_tensors                                          &
     &     (rj_fld, ipol%i_m_flux, ipol%i_SGS_m_flux,                   &
     &      ipol%i_mom_flux_w_sgs)
      end if
!
      if(ipol%i_maxwell_t_w_sgs .gt. 0) then
        call add_2_nod_tensors                                          &
     &     (rj_fld, ipol%i_maxwell, ipol%i_SGS_maxwell,                 &
     &      ipol%i_maxwell_t_w_sgs)
      end if
!
!
      if(ipol%i_truncated_B .gt. 0) then
        call truncate_magnetic_field_4_view(ltr_crust, sph_rj, rj_fld,  &
     &      ipol%i_magne, ipol%i_truncated_B)
      end if
!
!
      end subroutine s_cal_energy_flux_rj
!
!-----------------------------------------------------------------------
!
      subroutine truncate_magnetic_field_4_view(ltr_crust, sph_rj,      &
     &          rj_fld, i_magne, i_truncated_B)
!
      integer(kind = kint), intent(in) :: ltr_crust
      integer(kind = kint), intent(in) :: i_magne, i_truncated_B
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: inod, l_gl
!
!$omp parallel do private(inod, l_gl)
      do inod = 1, sph_rj%nnod_rj
        l_gl = aint(sqrt(real(sph_rj%idx_global_rj(inod,2))))
        if(l_gl .le. ltr_crust) then
          rj_fld%d_fld(inod,i_truncated_B  )                            &
     &          = rj_fld%d_fld(inod,i_magne  )
          rj_fld%d_fld(inod,i_truncated_B+1)                            &
     &          = rj_fld%d_fld(inod,i_magne+1)
          rj_fld%d_fld(inod,i_truncated_B+2)                            &
     &          = rj_fld%d_fld(inod,i_magne+2)
        else
          rj_fld%d_fld(inod,i_truncated_B  ) = zero
          rj_fld%d_fld(inod,i_truncated_B+1) = zero
          rj_fld%d_fld(inod,i_truncated_B+2) = zero
        end if
      end do
!$omp end parallel do
!
      end subroutine truncate_magnetic_field_4_view
!
!-----------------------------------------------------------------------
!
      end module cal_energy_flux_rj
