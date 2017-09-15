!
!      module int_surf_magne_pre
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine int_surf_magne_pre_ele(SGS_param, cmt_param, num_int,&
!!     &          iak_diff_uxb, ak_d_magne, node, ele, surf, sf_grp,    &
!!     &          Asf_bcs, Bsf_bcs, iphys, nod_fld, g_FEM, jac_sf_grp,  &
!!     &          rhs_tbl, FEM_elens, diff_coefs, fem_wk, surf_wk,      &
!!     &          f_l, f_nl)
!!      subroutine int_surf_magne_monitor(SGS_param, cmt_param, num_int,&
!!     &          i_field, iak_diff_uxb, ak_d_magne,                    &
!!     &          node, ele, surf, sf_grp, Asf_bcs, Bsf_bcs,            &
!!     &          iphys, nod_fld, g_FEM, jac_sf_grp, rhs_tbl, FEM_elens,&
!!     &          diff_coefs, fem_wk, surf_wk, f_l, f_nl)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      module int_surf_magne_pre
!
      use m_precision
      use m_phys_constants
!
      use t_SGS_control_parameter
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_fem_gauss_int_coefs
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_surface_bc_data
      use t_material_property
      use t_SGS_model_coefs
!
      use int_surf_div_induct_tsr_sgs
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
      subroutine int_surf_magne_pre_ele(SGS_param, cmt_param, num_int,  &
     &          iak_diff_uxb, ak_d_magne, node, ele, surf, sf_grp,      &
     &          Asf_bcs, Bsf_bcs, iphys, nod_fld, g_FEM, jac_sf_grp,    &
     &          rhs_tbl, FEM_elens, diff_coefs, fem_wk, surf_wk,        &
     &          f_l, f_nl)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_address), intent(in) :: iphys
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      integer(kind= kint), intent(in) :: num_int
      integer(kind= kint), intent(in) :: iak_diff_uxb
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      call int_sf_grad_velocity(node, ele, surf, sf_grp,                &
     &    g_FEM, jac_sf_grp, rhs_tbl, Bsf_bcs%grad,                     &
     &    num_int, ak_d_magne, fem_wk, f_l)
!
       if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none                     &
     &    .and. cmt_param%iflag_c_uxb .eq. id_SGS_commute_ON) then
         call int_surf_div_induct_t_sgs                                 &
     &      (node, ele, surf, sf_grp, nod_fld, g_FEM, jac_sf_grp,       &
     &       rhs_tbl, FEM_elens, Bsf_bcs%sgs, num_int,                  &
     &       SGS_param%ifilter_final, diff_coefs%num_field,             &
     &       iak_diff_uxb, diff_coefs%ak, iphys%i_SGS_induct_t,         &
     &       iphys%i_velo, iphys%i_magne, fem_wk, surf_wk, f_nl)
      end if
!
!      call int_free_slip_surf_sph_out(node, ele, surf, sf_grp,         &
!     &    nod_fld, g_FEM, jac_sf_grp, rhs_tbl, n_int,                  &
!     &    Asf_bcs%free_sph_out%ngrp_sf_dat,                            &
!     &    Asf_bcs%free_sph_out%id_grp_sf_dat,                          &
!     &    iphys%i_vecp, ak_d_magne, fem_wk, surf_wk, f_l)
!      call int_free_slip_surf_sph_in(node, ele, surf, sf_grp,          &
!     &    nod_fld, g_FEM, jac_sf_grp, rhs_tbl, n_int,                  &
!     &    Asf_bcs%free_sph_in%ngrp_sf_dat,                             &
!     &    Asf_bcs%free_sph_in%id_grp_sf_dat,                           &
!     &    iphys%i_vecp, ak_d_magne, fem_wk, surf_wk, f_l)
!
      end subroutine int_surf_magne_pre_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_magne_monitor(SGS_param, cmt_param, num_int,  &
     &          i_field, iak_diff_uxb, ak_d_magne,                      &
     &          node, ele, surf, sf_grp, Asf_bcs, Bsf_bcs,              &
     &          iphys, nod_fld, g_FEM, jac_sf_grp, rhs_tbl, FEM_elens,  &
     &          diff_coefs, fem_wk, surf_wk, f_l, f_nl)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      integer(kind= kint), intent(in) :: num_int
      integer(kind= kint), intent(in) :: i_field, iak_diff_uxb
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      if (i_field .eq. iphys%i_b_diffuse) then
        call int_sf_grad_velocity                                       &
     &     (node, ele, surf, sf_grp, g_FEM, jac_sf_grp,                 &
     &      rhs_tbl, Bsf_bcs%grad, num_int, ak_d_magne, fem_wk, f_l)
      end if
!
      if (i_field .eq. iphys%i_SGS_induction) then
        if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none                    &
     &    .and. cmt_param%iflag_c_uxb .eq. id_SGS_commute_ON) then
          call int_surf_div_induct_t_sgs                                &
     &       (node, ele, surf, sf_grp, nod_fld, g_FEM, jac_sf_grp,      &
     &        rhs_tbl, FEM_elens, Bsf_bcs%sgs, num_int,                 &
     &        SGS_param%ifilter_final, diff_coefs%num_field,            &
     &        iak_diff_uxb, diff_coefs%ak, iphys%i_SGS_induct_t,        &
     &        iphys%i_velo, iphys%i_magne, fem_wk, surf_wk, f_nl)
        end if
      end if
!
!      call int_free_slip_surf_sph_out(node, ele, surf, sf_grp,         &
!     &    nod_fld, g_FEM, jac_sf_grp, rhs_tbl, n_int,                  &
!     &    Asf_bcs%free_sph_out%ngrp_sf_dat,                            &
!     &    Asf_bcs%free_sph_out%id_grp_sf_dat,                          &
!     &    iphys%i_vecp, ak_d_magne, fem_wk, surf_wk, f_l)
!      call int_free_slip_surf_sph_in(node, ele, surf, sf_grp,          &
!     &    nod_fld, g_FEM, jac_sf_grp, rhs_tbl, n_int,                  &
!     &    Asf_bcs%free_sph_in%ngrp_sf_dat,                             &
!     &    Asf_bcs%free_sph_in%id_grp_sf_dat,                           &
!     &    iphys%i_vecp, ak_d_magne, fem_wk, surf_wk, f_l)
!
      end subroutine int_surf_magne_monitor
!
! ----------------------------------------------------------------------
!
      end module int_surf_magne_pre
