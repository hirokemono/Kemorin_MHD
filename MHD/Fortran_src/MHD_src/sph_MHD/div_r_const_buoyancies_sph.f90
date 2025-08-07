!>@file   div_r_const_buoyancies_sph.f90
!!@brief  module div_r_const_buoyancies_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate divergence of buoyancy under constant radial gravity
!!       for pressure evaluation
!!
!!@verbatim
!!      subroutine cal_div_r_const_buo_sph_mhd                          &
!!     &         (sph_rj, ipol_base, ipol_grd, ipol_div_frc,            &
!!     &         fl_prop, ref_param_T, ref_param_C, sph_bc_U, rj_fld)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(gradient_field_address), intent(in) :: ipol_grd
!!        type(base_force_address), intent(in) :: ipol_div_frc
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine cal_div_cst_buo_sph(kr_in, kr_out, coef,             &
!!     &          is_fld, ids_fld, is_div, nidx_rj, a_r_1d_rj_r,        &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic velocity
!!                 boundary condition parameters
!
      module div_r_const_buoyancies_sph
!
      use m_precision
      use m_machine_parameter
!
      use m_constants
!
      implicit  none
!
      private :: cal_div_cst_buo_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_r_const_buo_sph_mhd                            &
     &         (sph_rj, ipol_base, ipol_grd, ipol_div_frc,              &
     &         fl_prop, ref_param_T, ref_param_C, sph_bc_U, rj_fld)
!
      use t_physical_property
      use t_reference_scalar_param
      use t_spheric_rj_data
      use t_base_field_labels
      use t_base_force_labels
      use t_grad_field_labels
      use t_phys_data
      use t_boundary_params_sph_MHD
!
      type(fluid_property), intent(in) :: fl_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_base
      type(gradient_field_address), intent(in) :: ipol_grd
      type(base_force_address), intent(in) :: ipol_div_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ipol_scl, igrad_scl
!
!
      if (fl_prop%flag_thermal_buoyancy) then
        if(ref_param_T%flag_ref_field) then
          ipol_scl =  ipol_base%i_per_temp
          igrad_scl = ipol_grd%i_grad_per_t
        else
          ipol_scl =  ipol_base%i_temp
          igrad_scl = ipol_grd%i_grad_temp
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'cal_div_cst_buo_sph'
        call cal_div_cst_buo_sph                                        &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_buo,          &
     &      ipol_scl, igrad_scl, ipol_div_frc%i_thrm_buo,               &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj(1,1),                       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if (fl_prop%flag_comp_buoyancy) then
        if(ref_param_C%flag_ref_field) then
          ipol_scl =  ipol_base%i_per_light
          igrad_scl = ipol_grd%i_grad_per_c
        else
          ipol_scl =  ipol_base%i_light
          igrad_scl = ipol_grd%i_grad_composit
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'cal_div_cst_buo_sph'
        call cal_div_cst_buo_sph                                        &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_comp_buo,     &
     &      ipol_scl, igrad_scl, ipol_div_frc%i_comp_buo,               &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj(1,1),                       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine cal_div_r_const_buo_sph_mhd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_cst_buo_sph(kr_in, kr_out, coef,               &
     &          is_fld, ids_fld, is_div, nidx_rj, a_r_1d_rj_r,          &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_fld, ids_fld, is_div
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
        ist = (kr_in-1)*nidx_rj(2) + 1
        ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
        do inod = ist, ied
          j = mod((inod-1),nidx_rj(2)) + 1
          k = 1 + (inod- j) / nidx_rj(2)
          d_rj(inod,is_div) = coef * ( two * d_rj(inod,is_fld)        &
     &                       * a_r_1d_rj_r(k) + d_rj(inod,ids_fld))
        end do
!$omp end parallel do
!
      end subroutine cal_div_cst_buo_sph
!
!-----------------------------------------------------------------------
!
      end module div_r_const_buoyancies_sph
