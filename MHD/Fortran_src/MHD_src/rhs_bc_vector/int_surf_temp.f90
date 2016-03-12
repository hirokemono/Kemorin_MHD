!
!      module int_surf_temp
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine int_surf_temp_ele                                    &
!!     &         (iak_diff_hf, node, ele, surf, sf_grp, iphys, nod_fld, &
!!     &          jac_sf_grp, rhs_tbl, FEM_elens, fem_wk, f_l, f_nl)
!!      subroutine int_surf_temp_monitor(i_field, iak_diff_hf,          &
!!     &          node, ele, surf, sf_grp, iphys, nod_fld,              &
!!     &          jac_sf_grp, rhs_tbl, FEM_elens, fem_wk, f_l, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      module int_surf_temp
!
      use m_precision
      use m_control_parameter
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
!
      use int_surf_div_fluxes_sgs
      use int_surf_fixed_gradients
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_temp_ele                                      &
     &         (iak_diff_hf, node, ele, surf, sf_grp, iphys, nod_fld,   &
     &          jac_sf_grp, rhs_tbl, FEM_elens,               &
     &          fem_wk, f_l, f_nl)
!
      use m_SGS_model_coefs
      use m_surf_data_temp
!
      integer(kind = kint), intent(in)  :: iak_diff_hf
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!      type(scaler_surf_bc_type), intent(in) :: Tsf1_bcs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      integer(kind = kint)  :: num_int
!
!
      num_int = intg_point_t_evo
!
      call int_sf_h_flux(node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl,  &
     &    Tsf1_bcs%flux, num_int, ak_d_temp, fem_wk, f_l)
!
      if (iflag_SGS_heat .ne. id_SGS_none                               &
     &     .and. iflag_commute_temp .eq. id_SGS_commute_ON) then
        call int_sf_skv_sgs_div_v_flux(node, ele, surf, sf_grp,         &
     &      nod_fld, jac_sf_grp, rhs_tbl, FEM_elens, num_int,           &
     &      Tsf1_bcs%sgs%ngrp_sf_dat, Tsf1_bcs%sgs%id_grp_sf_dat,       &
     &      ifilter_final, iphys%i_SGS_h_flux, iphys%i_velo,            &
     &      iphys%i_temp, ak_diff(1,iak_diff_hf), coef_temp,            &
     &      fem_wk, f_nl)
      end if
!
      end subroutine int_surf_temp_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_temp_monitor(i_field, iak_diff_hf,            &
     &          node, ele, surf, sf_grp, iphys, nod_fld,                &
     &          jac_sf_grp, rhs_tbl, FEM_elens, fem_wk, f_l, f_nl)
!
      use m_SGS_model_coefs
      use m_surf_data_temp
!
      integer(kind = kint), intent(in)  :: iak_diff_hf
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind= kint), intent(in) :: i_field
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      integer(kind = kint)  :: num_int
!
!
      num_int = intg_point_t_evo
!
      if (i_field .eq. iphys%i_t_diffuse) then
        call int_sf_h_flux(node, ele, surf, sf_grp, jac_sf_grp,         &
     &      rhs_tbl, Tsf1_bcs%flux, num_int, ak_d_temp, fem_wk, f_l)
      end if
!
      if (iflag_commute_heat .eq. id_SGS_commute_ON                     &
        .and. i_field .eq. iphys%i_SGS_div_h_flux) then
        call int_sf_skv_sgs_div_v_flux(node, ele, surf, sf_grp,         &
     &      nod_fld, jac_sf_grp, rhs_tbl, FEM_elens, num_int,           &
     &      Tsf1_bcs%sgs%ngrp_sf_dat, Tsf1_bcs%sgs%id_grp_sf_dat,       &
     &      ifilter_final, iphys%i_SGS_h_flux, iphys%i_velo,            &
     &      iphys%i_temp, ak_diff(1,iak_diff_hf), coef_temp,            &
     &      fem_wk, f_nl)
      end if
!
      end subroutine int_surf_temp_monitor
!
! ----------------------------------------------------------------------
!
      end module int_surf_temp
