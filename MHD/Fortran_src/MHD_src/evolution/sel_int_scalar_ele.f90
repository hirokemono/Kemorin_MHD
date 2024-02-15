!>@file   sel_int_scalar_ele.f90
!!@brief  module sel_int_scalar_ele
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2005
!
!>@brief  Time integration routine select for scalar
!!
!!@verbatim
!!      subroutine s_sel_int_scalar_ele(i_field, dt,                    &
!!     &          iflag_supg, n_int_evo, iflag_SGS_flux, ifilter_final, &
!!     &          iflag_commute_flux, iflag_commute_field, i_scalar,    &
!!     &          i_velo, i_gref, i_tensor, i_diff_SGS,                 &
!!     &          mesh, group, fluid, property, ref_param, sf_bcs,      &
!!     &          ref_fld, iphys_ele_base, ele_fld, jacs, rhs_tbl,      &
!!     &          FEM_elens, Cdiff_scalar, Cdiff_scalar, diff_coefs,    &
!!     &          ak_diffuse,  mhd_fem_wk, rhs_mat, nod_fld)
!!        integer(kind = kint), intent(in) :: i_field
!!        real(kind = kreal), intent(in) :: dt
!!        integer(kind = kint), intent(in) :: iflag_supg
!!        integer(kind = kint), intent(in) :: n_int_evo
!!        integer(kind = kint), intent(in) :: iflag_SGS_flux
!!        integer(kind = kint), intent(in) :: ifilter_final
!!        integer(kind = kint), intent(in) :: iflag_commute_flux
!!        integer(kind = kint), intent(in) :: iflag_commute_field
!!        integer(kind = kint), intent(in) :: i_scalar
!!        integer(kind = kint), intent(in) :: i_velo
!!        integer(kind = kint), intent(in) :: i_gref
!!        integer(kind = kint), intent(in) :: i_tensor
!!        integer(kind = kint), intent(in) :: i_diff_SGS
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(scalar_property), intent(in) :: property
!!        type(reference_scalar_param), intent(in) :: ref_param
!!        type(scaler_surf_bc_type), intent(in) :: sf_bcs
!!        type(phys_data), intent(in) :: ref_fld
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        real(kind = kreal), intent(in) :: ak_diffuse(mesh%ele%numele)
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module sel_int_scalar_ele
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_reference_scalar_param
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_mesh_data
      use t_geometry_data_MHD
      use t_surface_data
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_base_field_labels
      use t_grad_field_labels
      use t_explicit_term_labels
      use t_SGS_term_labels
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_layering_ele_list
      use t_filter_elength
      use t_filtering_data
      use t_bc_data_temp
      use t_surface_bc_scalar
      use t_material_property
      use t_SGS_model_coefs
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
      use t_vector_for_solver
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_sel_int_scalar_ele(i_field, dt,                      &
     &          iflag_supg, n_int_evo, iflag_SGS_flux, ifilter_final,   &
     &          iflag_commute_flux, iflag_commute_field, i_scalar,      &
     &          i_velo, i_gref, i_tensor, i_diff_SGS,                   &
     &          mesh, group, fluid, property, ref_param, sf_bcs,        &
     &          ref_fld, iphys_ele_base, ele_fld, jacs, rhs_tbl,        &
     &          FEM_elens, Cdiff_scalar, diff_coefs,      &
     &          ak_diffuse,  mhd_fem_wk, rhs_mat, nod_fld)
!
      use nod_phys_send_recv
      use cal_sgs_fluxes
      use set_boundary_scalars
      use int_vol_diffusion_ele
      use int_vol_thermal_ele
      use int_surf_div_fluxes_sgs
      use int_surf_fixed_gradients
      use cal_stratification_by_temp
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: iflag_supg
      integer(kind = kint), intent(in) :: n_int_evo
      integer(kind = kint), intent(in) :: iflag_SGS_flux
      integer(kind = kint), intent(in) :: ifilter_final
      integer(kind = kint), intent(in) :: iflag_commute_flux
      integer(kind = kint), intent(in) :: iflag_commute_field
      integer(kind = kint), intent(in) :: i_scalar
      integer(kind = kint), intent(in) :: i_velo
      integer(kind = kint), intent(in) :: i_gref
      integer(kind = kint), intent(in) :: i_tensor
      integer(kind = kint), intent(in) :: i_diff_SGS
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(reference_scalar_param), intent(in) :: ref_param
      type(scaler_surf_bc_type), intent(in) :: sf_bcs
!
      type(phys_data), intent(in) :: ref_fld
!
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(SGS_model_coefficient), intent(in) :: Cdiff_scalar
!
      real(kind = kreal), intent(in) :: ak_diffuse(mesh%ele%numele)
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_ff_smps(mesh%node, rhs_mat%f_l, rhs_mat%f_nl)
!
!  ----------  lead diffusion term
!
      if (property%coef_advect .gt. zero                                &
     &     .and. property%coef_exp .gt. zero) then
        call int_vol_scalar_diffuse_ele                                 &
     &    (ifilter_final, fluid%istack_ele_fld_smp, n_int_evo,          &
     &     mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,       &
     &     rhs_tbl, FEM_elens, Cdiff_scalar, property%coef_exp,         &
     &     ak_diffuse, i_field, rhs_mat%fem_wk, rhs_mat%f_l)
      end if
!
!  ----------  lead advection term
!
      if (iflag_supg .gt. id_turn_OFF) then
        call int_vol_temp_ele_upw                                       &
     &     (iflag_SGS_flux, iflag_commute_flux, ifilter_final,          &
     &      n_int_evo, dt, i_scalar, i_velo, i_tensor,                  &
     &      mesh%node, mesh%ele, fluid, property, nod_fld,              &
     &      jacs%g_FEM, jacs%jac_3d, rhs_tbl, FEM_elens,                &
     &      diff_coefs%ak(1,i_diff_SGS), ele_fld%ntot_phys,             &
     &      iphys_ele_base%i_velo, ele_fld%d_fld,                       &
     &      mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_nl)
      else
        call int_vol_temp_ele                                           &
     &     (iflag_SGS_flux, iflag_commute_flux, ifilter_final,          &
     &      n_int_evo, i_scalar, i_velo, i_tensor,                      &
     &      mesh%node, mesh%ele, fluid, property, nod_fld,              &
     &      jacs%g_FEM, jacs%jac_3d, rhs_tbl, FEM_elens,                &
     &      diff_coefs%ak(1,i_diff_SGS), ele_fld%ntot_phys,             &
     &      iphys_ele_base%i_velo, ele_fld%d_fld,                       &
     &      mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_nl)
      end if
!
!      call check_ff_smp(my_rank, n_scalar, mesh%node, rhs_mat%f_l)
!      call check_ff_smp(my_rank, n_scalar, mesh%node, frhs_mat%_nl)
!
      call int_sf_scalar_flux                                           &
     &   (mesh%node, mesh%ele, mesh%surf, group%surf_grp,               &
     &    jacs%g_FEM, jacs%jac_sf_grp, rhs_tbl, sf_bcs%flux,            &
     &    n_int_evo, ak_diffuse, rhs_mat%fem_wk, rhs_mat%f_l)
!
      if(iflag_commute_field .ne. id_SGS_commute_OFF                    &
          .and. iflag_SGS_flux .ne. id_SGS_none) then
        call int_sf_skv_sgs_div_v_flux(mesh%node, mesh%ele, mesh%surf,  &
     &      group%surf_grp, nod_fld, jacs%g_FEM, jacs%jac_sf_grp,       &
     &      rhs_tbl, FEM_elens, n_int_evo,                              &
     &      sf_bcs%sgs%ngrp_sf_dat, sf_bcs%sgs%id_grp_sf_dat,           &
     &      ifilter_final, i_tensor, i_velo, i_scalar,                  &
     &      diff_coefs%ak(1,i_diff_SGS), property%coef_advect,          &
     &      rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_nl)
      end if
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_scalar, i_field)
!      call check_nodal_data                                            &
!     &   ((50+my_rank), ele_fld, n_vector, iphys_ele_base%i_velo)
!      call check_ff_smp(my_rank, n_scalar, mesh%node, rhs_mat%f_l)
!      call check_ff_smp(my_rank, n_scalar, mesh%node, rhs_mat%f_nl)
!
      if (ref_param%iflag_reference .eq. id_takepiro_temp) then
        if (iflag_supg .gt. id_turn_OFF) then
          call cal_stratified_layer_upw(i_gref, n_int_evo,              &
     &        dt, mesh%node, mesh%ele, fluid, ref_fld,                  &
     &        ele_fld%ntot_phys, iphys_ele_base%i_velo, ele_fld%d_fld,  &
     &        jacs%g_FEM, jacs%jac_3d, rhs_tbl, mhd_fem_wk,             &
     &        rhs_mat%fem_wk, rhs_mat%f_nl)
        else
          call cal_stratified_layer                                     &
     &       (i_gref, n_int_evo, mesh%node, mesh%ele, fluid, ref_fld,   &
     &        ele_fld%ntot_phys, iphys_ele_base%i_velo, ele_fld%d_fld,  &
     &        jacs%g_FEM, jacs%jac_3d, rhs_tbl, mhd_fem_wk,             &
     &        rhs_mat%fem_wk, rhs_mat%f_nl)
        end if
      end if
!
      end subroutine s_sel_int_scalar_ele
!
! ----------------------------------------------------------------------
!
      end module sel_int_scalar_ele
