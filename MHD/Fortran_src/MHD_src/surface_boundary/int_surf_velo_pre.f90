!
!      module int_surf_velo_pre
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine int_surf_velo_pre_ele                                &
!!     &        (ak_d_velo, num_int, SGS_param, cmt_param,              &
!!     &         node, ele, surf, sf_grp, fl_prop, Vsf_bcs, Bsf_bcs,    &
!!     &         iphys_base, iphys_SGS, nod_fld, g_FEM, jac_sf_grp,     &
!!     &         rhs_tbl, FEM_elens, iak_diff_SGS, diff_coefs,          &
!!     &         fem_wk, surf_wk, f_l, f_nl)
!!      subroutine int_surf_velo_monitor(i_field, ak_d_velo, num_int,   &
!!     &          SGS_param, cmt_param, node, ele, surf, sf_grp,        &
!!     &          fl_prop, Vsf_bcs, Bsf_bcs, iphys_base, iphys_dif,     &
!!     &          iphys_SGS, iphys_div_SGS, nod_fld, g_FEM, jac_sf_grp, &
!!     &          rhs_tbl, FEM_elens, iak_diff_SGS, diff_coefs,         &
!!     &          fem_wk, surf_wk, f_l, f_nl)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(diffusion_address), intent(in) :: iphys_dif
!!        type(SGS_term_address), intent(in) :: iphys_SGS
!!        type(SGS_term_address), intent(in) :: iphys_div_SGS
!!        type(phys_data), intent(in) :: nod_fld
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(SGS_term_address), intent(in) :: iak_diff_SGS
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      module int_surf_velo_pre
!
      use m_precision
!
      use t_SGS_control_parameter
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_base_field_labels
      use t_diffusion_term_labels
      use t_SGS_term_labels
      use t_fem_gauss_int_coefs
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_surface_bc_vector
      use t_surface_bc_velocity
      use t_material_property
      use t_SGS_model_coefs
      use t_physical_property
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
     &        (ak_d_velo, num_int, SGS_param, cmt_param,                &
     &         node, ele, surf, sf_grp, fl_prop, Vsf_bcs, Bsf_bcs,      &
     &         iphys_base, iphys_SGS, nod_fld, g_FEM, jac_sf_grp,       &
     &         rhs_tbl, FEM_elens, iak_diff_SGS, diff_coefs,            &
     &         fem_wk, surf_wk, f_l, f_nl)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(fluid_property), intent(in) :: fl_prop
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(base_field_address), intent(in) :: iphys_base
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(SGS_term_address), intent(in) :: iak_diff_SGS
!
      integer(kind= kint), intent(in) :: num_int
      real(kind = kreal), intent(in) :: ak_d_velo(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      if(SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none) then
        if(SGS_param%SGS_momentum%iflag_commute_flux                    &
     &      .eq. id_SGS_commute_ON) then
          call int_sf_skv_sgs_div_t_flux                                &
     &       (node, ele, surf, sf_grp, nod_fld, g_FEM, jac_sf_grp,      &
     &        rhs_tbl, FEM_elens, Vsf_bcs%sgs, num_int,                 &
     &        SGS_param%ifilter_final, iphys_SGS%i_SGS_m_flux,          &
     &        iphys_base%i_velo, iphys_base%i_velo,                     &
     &        diff_coefs%ak(1,iak_diff_SGS%i_SGS_m_flux),               &
     &        fl_prop%coef_velo, fem_wk, surf_wk, f_nl)
        end if
      end if
!
      if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
        if (cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON) then
          call int_sf_skv_sgs_div_t_flux                                &
     &       (node, ele, surf, sf_grp, nod_fld, g_FEM, jac_sf_grp,      &
     &        rhs_tbl, FEM_elens, Bsf_bcs%sgs, num_int,                 &
     &        SGS_param%ifilter_final, iphys_SGS%i_SGS_maxwell,         &
     &        iphys_base%i_magne, iphys_base%i_magne,                   &
     &        diff_coefs%ak(1,iak_diff_SGS%i_SGS_Lorentz),              &
     &        (-fl_prop%coef_lor), fem_wk, surf_wk, f_nl)
        end if
      end if
!
!
        call int_sf_grad_velocity(node, ele, surf, sf_grp,              &
     &      g_FEM, jac_sf_grp, rhs_tbl, Vsf_bcs%grad,                   &
     &      num_int, ak_d_velo, fem_wk, f_l)
        call int_free_slip_surf_sph_in(node, ele, surf, sf_grp,         &
     &      nod_fld, g_FEM, jac_sf_grp, rhs_tbl, num_int,               &
     &      Vsf_bcs%free_sph_in%ngrp_sf_dat,                            &
     &      Vsf_bcs%free_sph_in%id_grp_sf_dat,                          &
     &      iphys_base%i_velo, ak_d_velo, fem_wk, surf_wk, f_l)
        call int_free_slip_surf_sph_out(node, ele, surf, sf_grp,        &
     &      nod_fld, g_FEM, jac_sf_grp, rhs_tbl, num_int,               &
     &      Vsf_bcs%free_sph_out%ngrp_sf_dat,                           &
     &      Vsf_bcs%free_sph_out%id_grp_sf_dat,                         &
     &      iphys_base%i_velo, ak_d_velo, fem_wk, surf_wk, f_l)
!
      end subroutine int_surf_velo_pre_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_velo_monitor(i_field, ak_d_velo, num_int,     &
     &          SGS_param, cmt_param, node, ele, surf, sf_grp,          &
     &          fl_prop, Vsf_bcs, Bsf_bcs, iphys_base, iphys_dif,       &
     &          iphys_SGS, iphys_div_SGS, nod_fld, g_FEM, jac_sf_grp,   &
     &          rhs_tbl, FEM_elens, iak_diff_SGS, diff_coefs,           &
     &          fem_wk, surf_wk, f_l, f_nl)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(fluid_property), intent(in) :: fl_prop
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(base_field_address), intent(in) :: iphys_base
      type(diffusion_address), intent(in) :: iphys_dif
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(SGS_term_address), intent(in) :: iphys_div_SGS
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(SGS_term_address), intent(in) :: iak_diff_SGS
!
      integer(kind= kint), intent(in) :: num_int
      integer(kind= kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: ak_d_velo(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      if(i_field .eq. iphys_div_SGS%i_SGS_m_flux) then
        if(SGS_param%SGS_momentum%iflag_commute_flux                    &
     &        .eq. id_SGS_commute_ON) then
          call int_sf_skv_sgs_div_t_flux                                &
     &       (node, ele, surf, sf_grp, nod_fld, g_FEM, jac_sf_grp,      &
     &        rhs_tbl, FEM_elens, Vsf_bcs%sgs, num_int,                 &
     &        SGS_param%ifilter_final, iphys_SGS%i_SGS_m_flux,          &
     &        iphys_base%i_velo, iphys_base%i_velo,                     &
     &        diff_coefs%ak(1,iak_diff_SGS%i_SGS_m_flux),               &
     &        fl_prop%coef_velo, fem_wk, surf_wk, f_nl)
        end if
      end if
!
      if (i_field .eq. iphys_SGS%i_SGS_Lorentz) then
        if (cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON) then
          call int_sf_skv_sgs_div_t_flux                                &
     &       (node, ele, surf, sf_grp, nod_fld, g_FEM, jac_sf_grp,      &
     &        rhs_tbl, FEM_elens, Bsf_bcs%sgs, num_int,                 &
     &        SGS_param%ifilter_final, iphys_SGS%i_SGS_maxwell,         &
     &        iphys_base%i_magne, iphys_base%i_magne,                   &
     &        diff_coefs%ak(1,iak_diff_SGS%i_SGS_Lorentz),              &
     &        (-fl_prop%coef_lor), fem_wk, surf_wk, f_nl)
        end if
      end if
!
!
      if (i_field .eq. iphys_dif%i_v_diffuse) then
        call int_sf_grad_velocity(node, ele, surf, sf_grp,              &
     &      g_FEM, jac_sf_grp, rhs_tbl, Vsf_bcs%grad,                   &
     &      num_int, ak_d_velo, fem_wk, f_l)
        call int_free_slip_surf_sph_in(node, ele, surf, sf_grp,         &
     &      nod_fld, g_FEM, jac_sf_grp, rhs_tbl, num_int,               &
     &      Vsf_bcs%free_sph_in%ngrp_sf_dat,                            &
     &      Vsf_bcs%free_sph_in%id_grp_sf_dat,                          &
     &      iphys_base%i_velo, ak_d_velo, fem_wk, surf_wk, f_l)
        call int_free_slip_surf_sph_out(node, ele, surf, sf_grp,        &
     &      nod_fld, g_FEM, jac_sf_grp, rhs_tbl, num_int,               &
     &      Vsf_bcs%free_sph_out%ngrp_sf_dat,                           &
     &      Vsf_bcs%free_sph_out%id_grp_sf_dat,                         &
     &      iphys_base%i_velo, ak_d_velo, fem_wk, surf_wk, f_l)
      end if
!
      end subroutine int_surf_velo_monitor
!
! ----------------------------------------------------------------------
!
      end module int_surf_velo_pre
