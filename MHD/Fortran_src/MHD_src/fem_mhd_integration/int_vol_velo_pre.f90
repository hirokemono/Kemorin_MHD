!>@file   int_vol_velo_pre.f90
!!@brief  module int_vol_velo_pre
!!
!!@author H. Matsui and H.Okuda 
!!@date Programmed in July 2000 (ver 1.1)
!!        modified by H. Matsui in Oct., 2005
!!        modified by H. Matsui in Aug., 2007
!!
!>@brief  Finite elememt integration for momentum equation
!!
!!@verbatim
!!      subroutine int_vol_velo_pre_ele(iflag_4_rotate, num_int,        &
!!     &         SGS_param, cmt_param, node, ele, fluid,                &
!!     &         fl_prop, cd_prop, iphys_base, iphys_fil, iphys_SGS,    &
!!     &         nod_fld, ak_MHD, ncomp_ele, d_ele, iphys_ele_base,     &
!!     &         iak_diff_SGS, g_FEM, jac_3d, rhs_tbl, FEM_elens,       &
!!     &         diff_coefs, mhd_fem_wk, fem_wk, f_nl)
!!      subroutine int_vol_velo_pre_ele_upwind(iflag_4_rotate, num_int, &
!!     &          dt, SGS_param, cmt_param, node, ele, fluid,           &
!!     &          fl_prop, cd_prop, iphys_base, iphys_fil, iphys_SGS,   &
!!     &          nod_fld, ak_MHD, ncomp_ele, ie_upw,                   &
!!     &          d_ele, iphys_ele_base, iak_diff_SGS, g_FEM, jac_3d,   &
!!     &          rhs_tbl, FEM_elens, diff_coefs, mhd_fem_wk,           &
!!     &          fem_wk, f_nl)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_field_address), intent(in) :: iphys_fil
!!        type(SGS_term_address), intent(in) :: iphys_SGS
!!        type(phys_data), intent(in) :: nod_fld
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(SGS_term_address), intent(in) :: iak_diff_SGS
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!@endverbatim
!
!
      module int_vol_velo_pre
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
!
      use t_SGS_control_parameter
      use t_physical_property
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_base_field_labels
      use t_SGS_term_labels
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_phys_address
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_SGS_model_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_velo_pre_ele(iflag_4_rotate, num_int,          &
     &         SGS_param, cmt_param, node, ele, fluid,                  &
     &         fl_prop, cd_prop, iphys_base, iphys_fil, iphys_SGS,      &
     &         nod_fld, ak_MHD, ncomp_ele, d_ele, iphys_ele_base,       &
     &         iak_diff_SGS, g_FEM, jac_3d, rhs_tbl, FEM_elens,         &
     &         diff_coefs, mhd_fem_wk, fem_wk, f_nl)
!
      use cal_add_smp
      use nodal_fld_cst_to_element
      use gravity_vec_each_ele
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_nodal_field
      use fem_skv_inertia
      use fem_skv_div_flux
      use fem_skv_div_sgs_flux_type
      use fem_skv_lorentz_full
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(SGS_term_address), intent(in) :: iphys_SGS
!
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(SGS_term_address), intent(in) :: iak_diff_SGS
!
      integer(kind = kint), intent(in) :: iflag_4_rotate, num_int
      integer(kind = kint), intent(in) :: ncomp_ele
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      type(base_field_address), intent(in) :: iphys_ele_base
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind=kint) :: k2
!
!  ---------  set number of integral points
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele%nnod_4_ele
!
!  -----   set advection  --------
!
        if (fl_prop%coef_nega_v .ne. 0.0d0) then
          call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,       &
     &       iphys_base%i_velo, fl_prop%coef_nega_v, mhd_fem_wk%velo_1)
!
!  -----  Inertia including Reynolds stress by rotation form --------
!
          if (iflag_4_rotate .eq. id_turn_ON) then
!
            if(SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none   &
     &        .and. SGS_param%SGS_momentum%iflag_commute_flux           &
     &            .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &            iphys_base%i_velo, iphys_SGS%i_SGS_m_flux,            &
     &            fl_prop%coef_nega_v, mhd_fem_wk%sgs_t1,               &
     &            fem_wk%tensor_1)
!
              call fem_skv_rot_inertia                                  &
     &           (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,           &
     &            np_smp, fluid%istack_ele_fld_smp,                     &
     &            g_FEM%max_int_point, g_FEM%maxtot_int_3d,             &
     &            g_FEM%int_start3, g_FEM%owe3d, num_int, k2,           &
     &            jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%an,   &
     &            mhd_fem_wk%velo_1, d_ele(1,iphys_ele_base%i_vort),    &
     &            fem_wk%sk6)
              call fem_skv_div_sgs_tensor                               &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            SGS_param%ifilter_final,                              &
     &            diff_coefs%Cdiff_SGS_mf%coef(1,1),                    &
     &            ele, g_FEM, jac_3d, FEM_elens, mhd_fem_wk%sgs_t1,     &
     &            fem_wk%tensor_1, fem_wk%sk6)
            else if(SGS_param%SGS_momentum%iflag_SGS_flux               &
     &          .ne. id_SGS_none) then
              call tensor_cst_phys_2_each_ele(node, ele, nod_fld,       &
     &            k2, iphys_SGS%i_SGS_m_flux, fl_prop%coef_nega_v,      &
     &            mhd_fem_wk%sgs_t1)
              call fem_skv_inertia_rot_sgs_pg                           &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            ele, g_FEM, jac_3d, mhd_fem_wk%velo_1,                &
     &            mhd_fem_wk%sgs_t1, d_ele(1,iphys_ele_base%i_vort),    &
     &            fem_wk%sk6)
            else
              call fem_skv_rot_inertia                                  &
     &           (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,           &
     &            np_smp, fluid%istack_ele_fld_smp,                     &
     &            g_FEM%max_int_point, g_FEM%maxtot_int_3d,             &
     &            g_FEM%int_start3, g_FEM%owe3d, num_int, k2,           &
     &            jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%an,   &
     &            mhd_fem_wk%velo_1, d_ele(1,iphys_ele_base%i_vort),    &
     &            fem_wk%sk6)
            end if
!
!  -----  Inertia including Reynolds stress --------
!
          else
            if(SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none   &
     &        .and. SGS_param%SGS_momentum%iflag_commute_flux           &
     &             .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &            iphys_base%i_velo, iphys_SGS%i_SGS_m_flux,            &
     &            fl_prop%coef_nega_v, mhd_fem_wk%sgs_t1,               &
     &            fem_wk%tensor_1)
              call fem_skv_vec_inertia_modsgs_pg                        &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            SGS_param%ifilter_final,                              &
     &            diff_coefs%Cdiff_SGS_mf%coef(1,1),                    &
     &            ele, g_FEM, jac_3d, FEM_elens, mhd_fem_wk%velo_1,     &
     &            mhd_fem_wk%sgs_t1, fem_wk%tensor_1,                   &
     &            d_ele(1,iphys_ele_base%i_velo), fem_wk%sk6)
            else if(SGS_param%SGS_momentum%iflag_SGS_flux               &
     &           .ne. id_SGS_none) then
              call tensor_cst_phys_2_each_ele(node, ele, nod_fld,       &
     &            k2, iphys_SGS%i_SGS_m_flux, fl_prop%coef_nega_v,      &
     &            mhd_fem_wk%sgs_t1)
              call fem_skv_vec_inertia_sgs_pg                           &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            ele, g_FEM, jac_3d, mhd_fem_wk%velo_1,                &
     &            mhd_fem_wk%sgs_t1, d_ele(1,iphys_ele_base%i_velo),    &
     &            fem_wk%sk6)
            else
              call fem_skv_vector_inertia                               &
     &           (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,           &
     &            np_smp, fluid%istack_ele_fld_smp,                     &
     &            g_FEM%max_int_point, g_FEM%maxtot_int_3d,             &
     &            g_FEM%int_start3, g_FEM%owe3d, num_int, k2,           &
     &            jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,  &
     &            mhd_fem_wk%velo_1, d_ele(1,iphys_ele_base%i_velo),    &
     &            fem_wk%sk6)
            end if
          end if
        end if
!
!  -----   set Lorentz force  --------
!
        if(fl_prop%iflag_4_lorentz) then
          if(iflag_4_rotate .eq. id_turn_ON) then
            call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,     &
     &          iphys_base%i_vecp, fl_prop%coef_lor, mhd_fem_wk%vecp_1)
!$omp parallel
            call add_const_to_vector_smp(ele%numele,                    &
     &          d_ele(1,iphys_ele_base%i_magne), cd_prop%ex_magne,      &
     &          fem_wk%vector_1)
!$omp end parallel
!
            call fem_skv_lorentz_rot                                    &
     &         (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,             &
     &          np_smp, fluid%istack_ele_fld_smp, g_FEM%max_int_point,  &
     &          g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,     &
     &          jac_3d%ntot_int, num_int, k2, jac_3d%xjac,              &
     &          jac_3d%dnx, jac_3d%dnx, mhd_fem_wk%vecp_1,              &
     &          fem_wk%vector_1, fem_wk%sk6)
          else if (iflag_4_rotate .eq. id_turn_OFF) then
            call vector_cst_phys_2_each_ele                             &
     &         (node, ele, nod_fld, k2, iphys_base%i_magne,             &
     &          fl_prop%coef_lor, mhd_fem_wk%magne_1)
!$omp parallel
            call add_const_to_vector_smp(ele%numele,                    &
     &          d_ele(1,iphys_ele_base%i_magne), cd_prop%ex_magne,      &
     &          fem_wk%vector_1)
!$omp end parallel
!
            call fem_skv_vector_inertia                                 &
     &         (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,             &
     &          np_smp, fluid%istack_ele_fld_smp, g_FEM%max_int_point,  &
     &          g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,     &
     &          num_int, k2, jac_3d%ntot_int, jac_3d%xjac,              &
     &          jac_3d%an, jac_3d%dnx, mhd_fem_wk%magne_1,              &
     &          fem_wk%vector_1, fem_wk%sk6)
          end if
!
!    set SGS Lorentz force
!
          if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
            if(cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &            iphys_base%i_magne, iphys_SGS%i_SGS_maxwell,          &
     &            fl_prop%coef_lor, mhd_fem_wk%sgs_t1, fem_wk%tensor_1)
              call fem_skv_div_sgs_tensor                               &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            SGS_param%ifilter_final,                              &
     &            diff_coefs%Cdiff_SGS_lor%coef(1,1),                   &
     &            ele, g_FEM, jac_3d, FEM_elens, mhd_fem_wk%sgs_t1,     &
     &            fem_wk%tensor_1, fem_wk%sk6)
            else
              call tensor_cst_phys_2_each_ele                           &
     &           (node, ele, nod_fld, k2, iphys_SGS%i_SGS_maxwell,      &
     &            fl_prop%coef_lor, fem_wk%tensor_1)
              call fem_skv_all_div_flux                                 &
     &           (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,           &
     &           np_smp, fluid%istack_ele_fld_smp, g_FEM%max_int_point, &
     &           g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,    &
     &           num_int, k2, jac_3d%ntot_int, jac_3d%xjac,             &
     &           jac_3d%an, jac_3d%dnx, fem_wk%tensor_1, fem_wk%sk6)
            end if
          end if
!
        end if
!
!  --------  set coriolis force
!
        if(fl_prop%iflag_4_coriolis                                     &
     &     .and. fl_prop%iflag_FEM_coriolis .eq. id_FORCE_ele_int) then
          call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,       &
     &        iphys_base%i_velo, fl_prop%coef_cor, mhd_fem_wk%velo_1)
          call fem_skv_coriolis                                         &
     &       (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,               &
     &        np_smp, fluid%istack_ele_fld_smp, g_FEM%max_int_point,    &
     &        g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,       &
     &        num_int, k2, jac_3d%ntot_int, jac_3d%xjac,                &
     &        jac_3d%an, jac_3d%an, mhd_fem_wk%velo_1, fl_prop%sys_rot, &
     &        fem_wk%sk6)
        end if
!
! ---------  set buoyancy
!
        if(fl_prop%iflag_FEM_gravity .eq. id_FORCE_ele_int) then
          if(fl_prop%iflag_4_gravity                                    &
     &     .and. fl_prop%iflag_4_composit_buo) then
            call set_double_gvec_each_ele(node, ele, nod_fld, k2,       &
     &          iphys_base%i_temp, iphys_base%i_light,                  &
     &          fl_prop%i_grav, fl_prop%grav,                           &
     &          ak_MHD%ak_buo, ak_MHD%ak_comp_buo, fem_wk%vector_1)
          else if(fl_prop%iflag_4_filter_gravity                        &
     &     .and. fl_prop%iflag_4_filter_comp_buo) then
            call set_double_gvec_each_ele(node, ele, nod_fld, k2,       &
     &          iphys_fil%i_temp, iphys_fil%i_light,                    &
     &          fl_prop%i_grav, fl_prop%grav,                           &
     &          ak_MHD%ak_buo, ak_MHD%ak_comp_buo, fem_wk%vector_1)
          else if(fl_prop%iflag_4_gravity                               &
     &     .and. fl_prop%iflag_4_filter_comp_buo) then
            call set_double_gvec_each_ele(node, ele, nod_fld, k2,       &
     &          iphys_base%i_temp, iphys_fil%i_light,                   &
     &          fl_prop%i_grav, fl_prop%grav,                           &
     &          ak_MHD%ak_buo, ak_MHD%ak_comp_buo, fem_wk%vector_1)
          else if(fl_prop%iflag_4_filter_gravity                        &
     &     .and. fl_prop%iflag_4_composit_buo) then
            call set_double_gvec_each_ele(node, ele, nod_fld, k2,       &
     &          iphys_fil%i_temp, iphys_base%i_light,                   &
     &          fl_prop%i_grav, fl_prop%grav,                           &
     &          ak_MHD%ak_buo, ak_MHD%ak_comp_buo, fem_wk%vector_1)
!
          else if (fl_prop%iflag_4_gravity) then
            call set_gravity_vec_each_ele(node, ele, nod_fld, k2,       &
     &          iphys_base%i_temp, fl_prop%i_grav, fl_prop%grav,        &
     &          ak_MHD%ak_buo, fem_wk%vector_1)
          else if(fl_prop%iflag_4_composit_buo) then
            call set_gravity_vec_each_ele(node, ele, nod_fld, k2,       &
     &          iphys_base%i_light, fl_prop%i_grav, fl_prop%grav,       &
     &          ak_MHD%ak_comp_buo, fem_wk%vector_1)
          else if(fl_prop%iflag_4_filter_gravity) then
            call set_gravity_vec_each_ele(node, ele, nod_fld, k2,       &
     &          iphys_fil%i_temp, fl_prop%i_grav, fl_prop%grav,         &
     &          ak_MHD%ak_buo, fem_wk%vector_1)
          else if(fl_prop%iflag_4_filter_comp_buo) then
            call set_gravity_vec_each_ele(node, ele, nod_fld, k2,       &
     &          iphys_fil%i_light, fl_prop%i_grav, fl_prop%grav,        &
     &          ak_MHD%ak_comp_buo, fem_wk%vector_1)
          end if
!
          call fem_skv_vector_field                                     &
     &       (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,               &
     &        fluid%istack_ele_fld_smp, g_FEM%max_int_point,            &
     &        g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,       &
     &        jac_3d%ntot_int, num_int, k2, jac_3d%xjac,                &
     &        jac_3d%an, jac_3d%an, fem_wk%vector_1, fem_wk%sk6)
        end if
!
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_velo_pre_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_velo_pre_ele_upwind(iflag_4_rotate, num_int,   &
     &          dt, SGS_param, cmt_param, node, ele, fluid,             &
     &          fl_prop, cd_prop, iphys_base, iphys_fil, iphys_SGS,     &
     &          nod_fld, ak_MHD, ncomp_ele, ie_upw,                     &
     &          d_ele, iphys_ele_base, iak_diff_SGS, g_FEM, jac_3d,     &
     &          rhs_tbl, FEM_elens, diff_coefs, mhd_fem_wk,             &
     &          fem_wk, f_nl)
!
      use cal_add_smp
      use nodal_fld_cst_to_element
      use gravity_vec_each_ele
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_nodal_fld_upwind
      use fem_skv_nonlinear_upwind
      use fem_skv_div_sgs_flux_upw
      use fem_skv_lorentz_full
      use fem_skv_div_flux_upw
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(SGS_term_address), intent(in) :: iphys_SGS
!
      type(phys_data), intent(in) :: nod_fld
      type(base_field_address), intent(in) :: iphys_ele_base
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(SGS_term_address), intent(in) :: iak_diff_SGS
!
      integer(kind = kint), intent(in) :: iflag_4_rotate, num_int
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind = kint) :: k2
!
!  ---------  set number of integral points
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele%nnod_4_ele
!
!  -----   set advection  --------
!
        if (fl_prop%coef_nega_v .ne. 0.0d0) then
          call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,       &
     &       iphys_base%i_velo, fl_prop%coef_nega_v, mhd_fem_wk%velo_1)
!
!  -----  Inertia including Reynolds stress by rotation form --------
!
          if (iflag_4_rotate .eq. id_turn_ON) then
            if(SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none   &
     &        .and. SGS_param%SGS_momentum%iflag_commute_flux           &
     &             .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &            iphys_base%i_velo, iphys_SGS%i_SGS_m_flux,            &
     &            fl_prop%coef_nega_v, mhd_fem_wk%sgs_t1,               &
     &            fem_wk%tensor_1)
!
              call fem_skv_rot_inertia_upwind                           &
     &           (fluid%istack_ele_fld_smp, num_int, k2, dt,            &
     &            mhd_fem_wk%velo_1, d_ele(1,iphys_ele_base%i_vort),    &
     &            d_ele(1,ie_upw), ele, g_FEM, jac_3d, fem_wk%sk6)
              call fem_skv_div_sgs_tensor_upwind                        &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            SGS_param%ifilter_final, dt,                          &
     &            diff_coefs%Cdiff_SGS_mf%coef(1,1),                    &
     &            ele, g_FEM, jac_3d, FEM_elens, d_ele(1,ie_upw),       &
     &            mhd_fem_wk%sgs_t1, fem_wk%tensor_1, fem_wk%sk6)
            else if(SGS_param%SGS_momentum%iflag_SGS_flux               &
     &          .ne. id_SGS_none) then
              call tensor_cst_phys_2_each_ele(node, ele, nod_fld,       &
     &            k2, iphys_SGS%i_SGS_m_flux, fl_prop%coef_nega_v,      &
     &            mhd_fem_wk%sgs_t1)
              call fem_skv_inertia_rot_sgs_upwind                       &
     &           (fluid%istack_ele_fld_smp, num_int, k2, dt,            &
     &            ele, g_FEM, jac_3d, mhd_fem_wk%velo_1,                &
     &            mhd_fem_wk%sgs_t1, d_ele(1,ie_upw), d_ele(1,ie_upw),  &
     &            fem_wk%sk6)
            else
              call fem_skv_rot_inertia_upwind                           &
     &           (fluid%istack_ele_fld_smp, num_int, k2, dt,            &
     &            mhd_fem_wk%velo_1, d_ele(1,iphys_ele_base%i_vort),    &
     &            d_ele(1,ie_upw), ele, g_FEM, jac_3d, fem_wk%sk6)
            end if
!
!  -----  Inertia including Reynolds stress --------
!
          else
            if(SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none   &
     &        .and. SGS_param%SGS_momentum%iflag_commute_flux           &
     &             .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &            iphys_base%i_velo, iphys_SGS%i_SGS_m_flux,            &
     &            fl_prop%coef_nega_v, mhd_fem_wk%sgs_t1,               &
     &            fem_wk%tensor_1)
              call fem_skv_vec_inertia_msgs_upw                         &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            SGS_param%ifilter_final, dt,                          &
     &            diff_coefs%Cdiff_SGS_mf%coef(1,1),                    &
     &            ele, g_FEM, jac_3d, FEM_elens, mhd_fem_wk%velo_1,     &
     &            mhd_fem_wk%sgs_t1, fem_wk%tensor_1,                   &
     &            d_ele(1,iphys_ele_base%i_velo), d_ele(1,ie_upw),      &
     &            fem_wk%sk6)
            else if(SGS_param%SGS_momentum%iflag_SGS_flux               &
     &          .ne. id_SGS_none) then
              call tensor_cst_phys_2_each_ele(node, ele, nod_fld,       &
     &            k2, iphys_SGS%i_SGS_m_flux, fl_prop%coef_nega_v,      &
     &            mhd_fem_wk%sgs_t1)
              call fem_skv_vcl_inertia_sgs_upwind                       &
     &           (fluid%istack_ele_fld_smp, num_int, k2, dt,            &
     &            ele, g_FEM, jac_3d, mhd_fem_wk%velo_1,                &
     &            mhd_fem_wk%sgs_t1, d_ele(1,iphys_ele_base%i_velo),    &
     &            d_ele(1,ie_upw), fem_wk%sk6)
            else
              call fem_skv_vector_inertia_upwind                        &
     &           (fluid%istack_ele_fld_smp, num_int, k2, dt,            &
     &            mhd_fem_wk%velo_1, d_ele(1,iphys_ele_base%i_velo),    &
     &            d_ele(1,ie_upw), ele, g_FEM, jac_3d, fem_wk%sk6)
            end if
          end if
!
!    set Reynolds stress
!
          if(SGS_param%SGS_momentum%iflag_SGS_flux                      &
     &      .ne. id_SGS_none) then
            if(SGS_param%SGS_momentum%iflag_commute_flux                &
     &             .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &            iphys_base%i_velo, iphys_SGS%i_SGS_m_flux,            &
     &            fl_prop%coef_nega_v, mhd_fem_wk%sgs_t1,               &
     &            fem_wk%tensor_1)
              call fem_skv_div_sgs_tensor_upwind                        &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            SGS_param%ifilter_final, dt,                          &
     &            diff_coefs%Cdiff_SGS_mf%coef(1,1),                    &
     &            ele, g_FEM, jac_3d, FEM_elens, d_ele(1,ie_upw),       &
     &            mhd_fem_wk%sgs_t1, fem_wk%tensor_1, fem_wk%sk6)
            else
              call tensor_cst_phys_2_each_ele(node, ele, nod_fld,       &
     &            k2, iphys_SGS%i_SGS_m_flux, fl_prop%coef_nega_v,      &
     &            fem_wk%tensor_1)
              call fem_skv_all_div_flux_upw                             &
     &           (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,   &
     &            fluid%istack_ele_fld_smp, g_FEM%max_int_point,        &
     &            g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,   &
     &            num_int, k2, dt, jac_3d%ntot_int, jac_3d%xjac,        &
     &            jac_3d%an, jac_3d%dnx, jac_3d%dnx, d_ele(1,ie_upw),   &
     &            fem_wk%tensor_1, fem_wk%sk6)
            end if
          end if
        end if
!
!  -----   set Lorentz force  --------
!
        if(fl_prop%iflag_4_lorentz) then
          if(iflag_4_rotate .eq. id_turn_ON) then
            call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,     &
     &          iphys_base%i_vecp, fl_prop%coef_lor, mhd_fem_wk%vecp_1)
!$omp parallel
            call add_const_to_vector_smp(ele%numele,                    &
     &          d_ele(1,iphys_ele_base%i_magne), cd_prop%ex_magne,      &
     &          fem_wk%vector_1)
!$omp end parallel
!
            call fem_skv_lorentz_rot                                    &
     &         (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,             &
     &          np_smp, fluid%istack_ele_fld_smp, g_FEM%max_int_point,  &
     &          g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,     &
     &          jac_3d%ntot_int, num_int, k2, jac_3d%xjac,              &
     &          jac_3d%dnx, jac_3d%dnx, mhd_fem_wk%vecp_1,              &
     &          fem_wk%vector_1, fem_wk%sk6)
          else
            call vector_cst_phys_2_each_ele                             &
     &         (node, ele, nod_fld, k2, iphys_base%i_magne,             &
     &          fl_prop%coef_lor, mhd_fem_wk%magne_1)
!$omp parallel
            call add_const_to_vector_smp(ele%numele,                    &
     &          d_ele(1,iphys_ele_base%i_magne), cd_prop%ex_magne,      &
     &          fem_wk%vector_1)
!$omp end parallel
!
            call fem_skv_vector_inertia_upwind                          &
     &         (fluid%istack_ele_fld_smp, num_int, k2, dt,              &
     &          mhd_fem_wk%magne_1, fem_wk%vector_1,                    &
     &          d_ele(1,ie_upw), ele, g_FEM, jac_3d, fem_wk%sk6)
          end if
!
!    set SGS Lorentz force
!
          if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
            if (cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &            iphys_base%i_magne, iphys_SGS%i_SGS_maxwell,          &
     &            fl_prop%coef_lor, mhd_fem_wk%sgs_t1, fem_wk%tensor_1)
              call fem_skv_div_sgs_tensor_upwind                        &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            SGS_param%ifilter_final, dt,                          &
     &            diff_coefs%Cdiff_SGS_lor%coef(1,1),                   &
     &            ele, g_FEM, jac_3d, FEM_elens, d_ele(1,ie_upw),       &
     &            mhd_fem_wk%sgs_t1, fem_wk%tensor_1, fem_wk%sk6)
            else
              call tensor_cst_phys_2_each_ele                           &
     &           (node, ele, nod_fld, k2, iphys_SGS%i_SGS_maxwell,      &
     &            fl_prop%coef_lor, fem_wk%tensor_1)
              call fem_skv_all_div_flux_upw                             &
     &           (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,   &
     &            fluid%istack_ele_fld_smp, g_FEM%max_int_point,        &
     &            g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,   &
     &            num_int, k2, dt, jac_3d%ntot_int, jac_3d%xjac,        &
     &            jac_3d%an, jac_3d%dnx, jac_3d%dnx, d_ele(1,ie_upw),   &
     &            fem_wk%tensor_1, fem_wk%sk6)
            end if
          end if
        end if
!
!  --------  set coriolis force
!
        if(fl_prop%iflag_4_coriolis                                     &
     &     .and. fl_prop%iflag_FEM_coriolis .eq. id_FORCE_ele_int) then
          call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,       &
     &        iphys_base%i_velo, fl_prop%coef_cor, mhd_fem_wk%velo_1)
          call fem_skv_coriolis_upwind(fluid%istack_ele_fld_smp,        &
     &        num_int, k2, dt, mhd_fem_wk%velo_1, fl_prop%sys_rot,      &
     &        d_ele(1,ie_upw), ele, g_FEM, jac_3d, fem_wk%sk6)
        end if
!
! ---------  set buoyancy
!
        if(fl_prop%iflag_FEM_gravity .eq. id_FORCE_ele_int) then
          if(fl_prop%iflag_4_gravity                                    &
     &     .and. fl_prop%iflag_4_composit_buo) then
            call set_double_gvec_each_ele(node, ele, nod_fld, k2,       &
     &          iphys_base%i_temp, iphys_base%i_light,                  &
     &          fl_prop%i_grav, fl_prop%grav,                           &
     &          ak_MHD%ak_buo, ak_MHD%ak_comp_buo, fem_wk%vector_1)
          else if(fl_prop%iflag_4_filter_gravity                        &
     &     .and. fl_prop%iflag_4_filter_comp_buo) then
            call set_double_gvec_each_ele(node, ele, nod_fld, k2,       &
     &           iphys_fil%i_temp, iphys_fil%i_light,                   &
     &          fl_prop%i_grav, fl_prop%grav,                           &
     &          ak_MHD%ak_buo, ak_MHD%ak_comp_buo, fem_wk%vector_1)
          else if(fl_prop%iflag_4_gravity                               &
     &     .and. fl_prop%iflag_4_filter_comp_buo) then
            call set_double_gvec_each_ele(node, ele, nod_fld, k2,       &
     &           iphys_base%i_temp, iphys_fil%i_light,                  &
     &          fl_prop%i_grav, fl_prop%grav,                           &
     &          ak_MHD%ak_buo, ak_MHD%ak_comp_buo, fem_wk%vector_1)
          else if(fl_prop%iflag_4_filter_gravity                        &
     &     .and. fl_prop%iflag_4_composit_buo) then
            call set_double_gvec_each_ele(node, ele, nod_fld, k2,       &
     &           iphys_fil%i_temp, iphys_base%i_light,                  &
     &          fl_prop%i_grav, fl_prop%grav,                           &
     &          ak_MHD%ak_buo, ak_MHD%ak_comp_buo, fem_wk%vector_1)
!
          else if(fl_prop%iflag_4_gravity) then
            call set_gravity_vec_each_ele(node, ele, nod_fld, k2,       &
     &          iphys_base%i_temp, fl_prop%i_grav, fl_prop%grav,        &
     &          ak_MHD%ak_buo, fem_wk%vector_1)
          else if(fl_prop%iflag_4_composit_buo) then
            call set_gravity_vec_each_ele(node, ele, nod_fld, k2,       &
     &          iphys_base%i_light, fl_prop%i_grav, fl_prop%grav,       &
     &          ak_MHD%ak_comp_buo, fem_wk%vector_1)
          else if(fl_prop%iflag_4_filter_gravity) then
            call set_gravity_vec_each_ele(node, ele, nod_fld, k2,       &
     &          iphys_fil%i_temp, fl_prop%i_grav, fl_prop%grav,         &
     &          ak_MHD%ak_buo, fem_wk%vector_1)
          else if(fl_prop%iflag_4_filter_comp_buo) then
            call set_gravity_vec_each_ele(node, ele, nod_fld, k2,       &
     &          iphys_fil%i_light, fl_prop%i_grav, fl_prop%grav,        &
     &          ak_MHD%ak_comp_buo, fem_wk%vector_1)
          end if
!
          call fem_skv_vector_field_upwind(fluid%istack_ele_fld_smp,    &
     &        num_int, k2, dt, d_ele(1,ie_upw), ele, g_FEM, jac_3d,     &
     &        fem_wk%vector_1, fem_wk%sk6)
        end if
!
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_velo_pre_ele_upwind
!
!-----------------------------------------------------------------------
!
      end module int_vol_velo_pre
