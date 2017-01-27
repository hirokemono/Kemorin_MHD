!
!      module int_surf_temp
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine int_surf_temp_ele(iak_diff_hf, ak_d_temp,            &
!!     &          node, ele, surf, sf_grp, property, iphys, nod_fld,    &
!!     &          surf_bc, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,  &
!!     &          fem_wk, surf_wk, f_l, f_nl)
!!      subroutine int_surf_temp_monitor                                &
!!     &         (i_field, iak_diff_hf, ak_d_temp,                      &
!!     &          node, ele, surf, sf_grp, property, iphys, nod_fld,    &
!!     &          surf_bc, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,  &
!!     &          fem_wk, surf_wk, f_l, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(scalar_property), intent(in) :: property
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(scaler_surf_bc_type), intent(in) :: surf_bc
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      module int_surf_temp
!
      use m_precision
      use m_control_parameter
!
      use t_physical_property
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_surface_bc_data
      use t_material_property
      use t_SGS_model_coefs
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_temp_ele(iak_diff_hf, ak_d_temp,              &
     &          node, ele, surf, sf_grp, property, iphys, nod_fld,      &
     &          surf_bc, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,    &
     &          fem_wk, surf_wk, f_l, f_nl)
!
      use int_surf_div_fluxes_sgs
      use int_surf_fixed_gradients
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_property), intent(in) :: property
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(scaler_surf_bc_type), intent(in) :: surf_bc
!
      integer(kind = kint), intent(in)  :: iak_diff_hf
      real(kind = kreal), intent(in) :: ak_d_temp(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      integer(kind = kint)  :: num_int
!
!
      num_int = intg_point_t_evo
!
      call int_sf_h_flux(node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl,  &
     &    surf_bc%flux, num_int, ak_d_temp, fem_wk, f_l)
!
      if (iflag_SGS_heat .ne. id_SGS_none                               &
     &     .and. cmt_param1%iflag_c_temp .eq. id_SGS_commute_ON) then
        call int_sf_skv_sgs_div_v_flux(node, ele, surf, sf_grp,         &
     &      nod_fld, jac_sf_grp, rhs_tbl, FEM_elens, num_int,           &
     &      surf_bc%sgs%ngrp_sf_dat, surf_bc%sgs%id_grp_sf_dat,         &
     &      ifilter_final, iphys%i_SGS_h_flux, iphys%i_velo,            &
     &      iphys%i_temp, diff_coefs%num_field, iak_diff_hf,            &
     &      diff_coefs%ak, property%coef_advect, fem_wk, surf_wk, f_nl)
      end if
!
      end subroutine int_surf_temp_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_temp_monitor                                  &
     &         (i_field, iak_diff_hf, ak_d_temp,                        &
     &          node, ele, surf, sf_grp, property, iphys, nod_fld,      &
     &          surf_bc, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,    &
     &          fem_wk, surf_wk, f_l, f_nl)
!
      use int_surf_div_fluxes_sgs
      use int_surf_fixed_gradients
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_property), intent(in) :: property
      integer(kind= kint), intent(in) :: i_field
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(scaler_surf_bc_type), intent(in) :: surf_bc
!
      integer(kind = kint), intent(in)  :: iak_diff_hf
      real(kind = kreal), intent(in) :: ak_d_temp(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      integer(kind = kint)  :: num_int
!
!
      num_int = intg_point_t_evo
!
      if (i_field .eq. iphys%i_t_diffuse) then
        call int_sf_h_flux(node, ele, surf, sf_grp, jac_sf_grp,         &
     &      rhs_tbl, surf_bc%flux, num_int, ak_d_temp, fem_wk, f_l)
      end if
!
      if (cmt_param1%iflag_c_hf .eq. id_SGS_commute_ON                  &
        .and. i_field .eq. iphys%i_SGS_div_h_flux) then
        call int_sf_skv_sgs_div_v_flux(node, ele, surf, sf_grp,         &
     &      nod_fld, jac_sf_grp, rhs_tbl, FEM_elens, num_int,           &
     &      surf_bc%sgs%ngrp_sf_dat, surf_bc%sgs%id_grp_sf_dat,         &
     &      ifilter_final, iphys%i_SGS_h_flux, iphys%i_velo,            &
     &      iphys%i_temp, diff_coefs%num_field, iak_diff_hf,            &
     &      diff_coefs%ak, property%coef_advect, fem_wk, surf_wk, f_nl)
      end if
!
      end subroutine int_surf_temp_monitor
!
! ----------------------------------------------------------------------
!
      end module int_surf_temp
