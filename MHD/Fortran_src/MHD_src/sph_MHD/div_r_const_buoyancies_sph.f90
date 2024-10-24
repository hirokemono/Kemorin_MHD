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
      private :: cal_div_double_cst_buo_sph
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
      integer(kind = kint) :: ipol_temp,  ipol_comp
      integer(kind = kint) :: igrad_temp, igrad_comp
!
!
      if    (ref_param_T%iflag_reference .eq. id_sphere_ref_temp        &
     &  .or. ref_param_T%iflag_reference .eq. id_takepiro_temp) then
        ipol_temp =  ipol_base%i_per_temp
        igrad_temp = ipol_grd%i_grad_per_t
      else
        ipol_temp =  ipol_base%i_temp
        igrad_temp = ipol_grd%i_grad_temp
      end if
!
      if    (ref_param_C%iflag_reference .eq. id_sphere_ref_temp        &
     &  .or. ref_param_C%iflag_reference .eq. id_takepiro_temp) then
        ipol_comp =  ipol_base%i_per_light
        igrad_comp = ipol_grd%i_grad_per_c
      else
        ipol_comp =  ipol_base%i_light
        igrad_comp = ipol_grd%i_grad_composit
      end if
!
      if(fl_prop%iflag_4_gravity                                        &
     &   .and. fl_prop%iflag_4_composit_buo) then
        if (iflag_debug.eq.1)                                           &
     &      write(*,*)'cal_div_double_cst_buo_sph', ipol_temp
        call cal_div_double_cst_buo_sph                                 &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_buo,          &
     &      ipol_temp, igrad_temp, fl_prop%coef_comp_buo,               &
     &      ipol_comp, igrad_comp, ipol_div_frc%i_buoyancy,             &
     &      sph_rj%nidx_rj, sph_rj%a_r_1d_rj_r,                         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if (fl_prop%iflag_4_gravity) then
        if (iflag_debug.eq.1) write(*,*) 'cal_div_cst_buo_sph'
        call cal_div_cst_buo_sph                                        &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_buo,          &
     &      ipol_temp, igrad_temp, ipol_div_frc%i_buoyancy,             &
     &      sph_rj%nidx_rj, sph_rj%a_r_1d_rj_r,                         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if (fl_prop%iflag_4_composit_buo) then
        if (iflag_debug.eq.1) write(*,*) 'cal_div_cst_buo_sph'
        call cal_div_cst_buo_sph                                        &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_comp_buo,     &
     &      ipol_comp, igrad_comp, ipol_div_frc%i_comp_buo,             &
     &      sph_rj%nidx_rj, sph_rj%a_r_1d_rj_r,                         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine cal_div_r_const_buo_sph_mhd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_double_cst_buo_sph(kr_in, kr_out,              &
     &          coef_t_buo, is_t, ids_t, coef_c_buo,                    &
     &          is_c, ids_c, is_div, nidx_rj, a_r_1d_rj_r,              &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_t, ids_t
      integer(kind= kint), intent(in) :: is_c, ids_c
      integer(kind= kint), intent(in) :: is_div
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef_t_buo, coef_c_buo
!
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
!
          d_rj(inod,is_div) = two * a_r_1d_rj_r(k)                      &
     &                       * (coef_t_buo * d_rj(inod,is_t)            &
     &                        + coef_c_buo * d_rj(inod,is_c))           &
     &                     +  ( coef_t_buo * d_rj(inod,ids_t)           &
     &                        + coef_c_buo * d_rj(inod,ids_c))
        end do
!$omp end parallel do
!
      end subroutine cal_div_double_cst_buo_sph
!
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
