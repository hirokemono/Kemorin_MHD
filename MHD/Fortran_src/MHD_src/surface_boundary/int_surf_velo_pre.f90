!
!      module int_surf_velo_pre
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine int_surf_velo_pre_ele                                &
!!     &         (iak_diff_mf, iak_diff_lor, ak_d_velo,                 &
!!     &          node, ele, surf, sf_grp, Vsf_bcs, Bsf_bcs,            &
!!     &          iphys, nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,       &
!!     &          diff_coefs, fem_wk, surf_wk, f_l, f_nl)
!!      subroutine int_surf_velo_monitor                                &
!!     &         (i_field, iak_diff_mf, iak_diff_lor, ak_d_velo,        &
!!     &          node, ele, surf, sf_grp, Vsf_bcs, Bsf_bcs,            &
!!     &          iphys, nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,       &
!!     &          fem_wk, surf_wk, f_l, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      module int_surf_velo_pre
!
      use m_precision
!
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
!
      use m_control_parameter
!
      use int_surf_div_fluxes_sgs
      use int_surf_fixed_gradients
      use int_free_slip_surf_sph
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_velo_pre_ele                                  &
     &         (iak_diff_mf, iak_diff_lor, ak_d_velo,                   &
     &          node, ele, surf, sf_grp, Vsf_bcs, Bsf_bcs,              &
     &          iphys, nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,         &
     &          diff_coefs, fem_wk, surf_wk, f_l, f_nl)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      integer(kind= kint), intent(in) :: iak_diff_mf, iak_diff_lor
      real(kind = kreal), intent(in) :: ak_d_velo(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      if (iflag_SGS_inertia  .ne. id_SGS_none) then
        if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
          call int_sf_skv_sgs_div_t_flux(node, ele, surf, sf_grp,       &
     &        nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,                  &
     &        Vsf_bcs%sgs, intg_point_t_evo, ifilter_final,             &
     &        iphys%i_SGS_m_flux, iphys%i_velo, iphys%i_velo,           &
     &        diff_coefs%num_field, iak_diff_mf, diff_coefs%ak,         &
     &        coef_velo, fem_wk, surf_wk, f_nl)
        end if
      end if
!
      if (iflag_SGS_lorentz .ne. id_SGS_none) then
        if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
          call int_sf_skv_sgs_div_t_flux(node, ele, surf, sf_grp,       &
     &        nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,                  &
     &        Bsf_bcs%sgs, intg_point_t_evo, ifilter_final,             &
     &        iphys%i_SGS_maxwell, iphys%i_magne, iphys%i_magne,        &
     &        diff_coefs%num_field, iak_diff_lor, diff_coefs%ak,        &
     &        (-coef_lor), fem_wk, surf_wk, f_nl)
        end if
      end if
!
!
        call int_sf_grad_velocity(node, ele, surf, sf_grp,              &
     &      jac_sf_grp, rhs_tbl, Vsf_bcs%grad,                          &
     &      intg_point_t_evo, ak_d_velo, fem_wk, f_l)
        call int_free_slip_surf_sph_in(node, ele, surf, sf_grp,         &
     &      nod_fld, jac_sf_grp, rhs_tbl, intg_point_t_evo,             &
     &      Vsf_bcs%free_sph_in%ngrp_sf_dat,                            &
     &      Vsf_bcs%free_sph_in%id_grp_sf_dat,                          &
     &      iphys%i_velo, ak_d_velo, fem_wk, surf_wk, f_l)
        call int_free_slip_surf_sph_out(node, ele, surf, sf_grp,        &
     &      nod_fld, jac_sf_grp, rhs_tbl, intg_point_t_evo,             &
     &      Vsf_bcs%free_sph_out%ngrp_sf_dat,                           &
     &      Vsf_bcs%free_sph_out%id_grp_sf_dat,                         &
     &      iphys%i_velo, ak_d_velo, fem_wk, surf_wk, f_l)
!
      end subroutine int_surf_velo_pre_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_velo_monitor                                  &
     &         (i_field, iak_diff_mf, iak_diff_lor, ak_d_velo,          &
     &          node, ele, surf, sf_grp, Vsf_bcs, Bsf_bcs,              &
     &          iphys, nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,         &
     &          diff_coefs, fem_wk, surf_wk, f_l, f_nl)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      integer(kind= kint), intent(in) :: i_field
      integer(kind= kint), intent(in) :: iak_diff_mf, iak_diff_lor
      real(kind = kreal), intent(in) :: ak_d_velo(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      if (i_field .eq. iphys%i_SGS_div_m_flux) then
        if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
          call int_sf_skv_sgs_div_t_flux(node, ele, surf, sf_grp,       &
     &        nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,                  &
     &        Vsf_bcs%sgs, intg_point_t_evo, ifilter_final,             &
     &        iphys%i_SGS_m_flux, iphys%i_velo, iphys%i_velo,           &
     &        diff_coefs%num_field, iak_diff_mf, diff_coefs%ak,         &
     &        coef_velo, fem_wk, surf_wk, f_nl)
        end if
      end if
!
      if (i_field .eq. iphys%i_SGS_Lorentz) then
        if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
          call int_sf_skv_sgs_div_t_flux(node, ele, surf, sf_grp,       &
     &        nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,                  &
     &        Bsf_bcs%sgs, intg_point_t_evo, ifilter_final,             &
     &        iphys%i_SGS_maxwell, iphys%i_magne, iphys%i_magne,        &
     &        diff_coefs%num_field, iak_diff_lor, diff_coefs%ak,        &
     &        (-coef_lor), fem_wk, surf_wk, f_nl)
        end if
      end if
!
!
      if (i_field .eq. iphys%i_v_diffuse) then
        call int_sf_grad_velocity(node, ele, surf, sf_grp,              &
     &      jac_sf_grp, rhs_tbl, Vsf_bcs%grad,                          &
     &      intg_point_t_evo, ak_d_velo, fem_wk, f_l)
        call int_free_slip_surf_sph_in(node, ele, surf, sf_grp,         &
     &      nod_fld, jac_sf_grp, rhs_tbl, intg_point_t_evo,             &
     &      Vsf_bcs%free_sph_in%ngrp_sf_dat,                            &
     &      Vsf_bcs%free_sph_in%id_grp_sf_dat,                          &
     &      iphys%i_velo, ak_d_velo, fem_wk, surf_wk, f_l)
        call int_free_slip_surf_sph_out(node, ele, surf, sf_grp,        &
     &      nod_fld, jac_sf_grp, rhs_tbl, intg_point_t_evo,             &
     &      Vsf_bcs%free_sph_out%ngrp_sf_dat,                           &
     &      Vsf_bcs%free_sph_out%id_grp_sf_dat,                         &
     &      iphys%i_velo, ak_d_velo, fem_wk, surf_wk, f_l)
      end if
!
      end subroutine int_surf_velo_monitor
!
! ----------------------------------------------------------------------
!
      end module int_surf_velo_pre
