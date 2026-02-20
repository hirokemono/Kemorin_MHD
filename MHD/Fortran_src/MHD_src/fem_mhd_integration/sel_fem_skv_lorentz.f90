!>@file   sel_fem_skv_lorentz.f90
!!@brief  module sel_fem_skv_lorentz
!!
!!@author H. Matsui and H.Okuda 
!!@date Programmed in July 2000 (ver 1.1)
!!        modified by H. Matsui in Oct., 2005
!!        modified by H. Matsui in Aug., 2007
!!
!>@brief  Finite elememt integration for momentum equation
!!
!!@verbatim
!!      subroutine sel_fem_skv_lorentz_pg(k2, iflag_4_rotate,           &
!!     &          num_int, SGS_param, cmt_param, node, ele, fluid,      &
!!     &          fl_prop, cd_prop, iphys_base, iphys_SGS, nod_fld,     &
!!     &          ncomp_ele, d_ele, iphys_ele_base, g_FEM,              &
!!     &          jac_3d, FEM_elens, diff_coefs, mhd_fem_wk, fem_wk)
!!      subroutine sel_fem_skv_lorentz_upwind(k2, iflag_4_rotate,       &
!!     &          num_int, dt, SGS_param, cmt_param, node, ele, fluid,  &
!!     &          fl_prop, cd_prop, iphys_base, iphys_SGS, nod_fld,     &
!!     &          ncomp_ele, ie_upw, d_ele, iphys_ele_base, g_FEM,      &
!!     &          jac_3d, FEM_elens, diff_coefs, mhd_fem_wk, fem_wk)
!!        integer(kind = kint), intent(in) :: k2
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(SGS_term_address), intent(in) :: iphys_SGS
!!        type(phys_data), intent(in) :: nod_fld
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_commutation_coefs), intent(in) :: diff_coefs
!!        integer(kind = kint), intent(in) :: iflag_4_rotate, num_int
!!        integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
!!        real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!!        real(kind = kreal), intent(in) :: dt
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!@endverbatim
!
!
      module sel_fem_skv_lorentz
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
      use t_FEM_SGS_model_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_fem_skv_lorentz_pg(k2, iflag_4_rotate,             &
     &          num_int, SGS_param, cmt_param, node, ele, fluid,        &
     &          fl_prop, cd_prop, iphys_base, iphys_SGS, nod_fld,       &
     &          ncomp_ele, d_ele, iphys_ele_base, g_FEM,                &
     &          jac_3d, FEM_elens, diff_coefs, mhd_fem_wk, fem_wk)
!
      use cal_add_smp
      use nodal_fld_cst_to_element
      use sgs_terms_to_each_ele
      use fem_skv_nodal_field
      use fem_skv_inertia
      use fem_skv_div_flux
      use fem_skv_div_sgs_flux_type
      use fem_skv_lorentz_full
!
      integer(kind = kint), intent(in) :: k2
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(base_field_address), intent(in) :: iphys_base
      type(SGS_term_address), intent(in) :: iphys_SGS
!
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_commutation_coefs), intent(in) :: diff_coefs
!
      integer(kind = kint), intent(in) :: iflag_4_rotate, num_int
      integer(kind = kint), intent(in) :: ncomp_ele
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      type(base_field_address), intent(in) :: iphys_ele_base
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!  -----   set Lorentz force  --------
          if(iflag_4_rotate .eq. id_turn_ON) then
            call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,     &
     &          iphys_base%i_vecp, fl_prop%coef_lor, mhd_fem_wk%vecp_1)
            call add_const_to_vector_smp(ele%numele,                    &
     &          d_ele(1,iphys_ele_base%i_magne), cd_prop%ex_magne,      &
     &          fem_wk%vector_1)
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
            call add_const_to_vector_smp(ele%numele,                    &
     &          d_ele(1,iphys_ele_base%i_magne), cd_prop%ex_magne,      &
     &          fem_wk%vector_1)
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
          if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
            if(cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &            iphys_base%i_magne, iphys_SGS%i_SGS_maxwell,          &
     &            fl_prop%coef_lor, mhd_fem_wk%sgs_t1, fem_wk%tensor_1)
              call fem_skv_div_sgs_tensor(fluid%istack_ele_fld_smp,     &
     &            num_int, k2, SGS_param%ifilter_final, ele, g_FEM,     &
     &            jac_3d, FEM_elens, diff_coefs%Cdiff_SGS_lor,          &
     &            mhd_fem_wk%sgs_t1, fem_wk%tensor_1, fem_wk%sk6)
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
      end subroutine sel_fem_skv_lorentz_pg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_fem_skv_lorentz_upwind(k2, iflag_4_rotate,         &
     &          num_int, dt, SGS_param, cmt_param, node, ele, fluid,    &
     &          fl_prop, cd_prop, iphys_base, iphys_SGS, nod_fld,       &
     &          ncomp_ele, ie_upw, d_ele, iphys_ele_base, g_FEM,        &
     &          jac_3d, FEM_elens, diff_coefs, mhd_fem_wk, fem_wk)
!
      use cal_add_smp
      use nodal_fld_cst_to_element
      use sgs_terms_to_each_ele
      use fem_skv_nodal_fld_upwind
      use fem_skv_nonlinear_upwind
      use fem_skv_div_sgs_flux_upw
      use fem_skv_lorentz_full
      use fem_skv_div_flux_upw
!
      integer(kind = kint), intent(in) :: k2
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(base_field_address), intent(in) :: iphys_base
      type(SGS_term_address), intent(in) :: iphys_SGS
!
      type(phys_data), intent(in) :: nod_fld
      type(base_field_address), intent(in) :: iphys_ele_base
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_commutation_coefs), intent(in) :: diff_coefs
!
      integer(kind = kint), intent(in) :: iflag_4_rotate, num_int
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!  -----   set Lorentz force  --------
          if(iflag_4_rotate .eq. id_turn_ON) then
            call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,     &
     &          iphys_base%i_vecp, fl_prop%coef_lor, mhd_fem_wk%vecp_1)
            call add_const_to_vector_smp(ele%numele,                    &
     &          d_ele(1,iphys_ele_base%i_magne), cd_prop%ex_magne,      &
     &          fem_wk%vector_1)
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
            call add_const_to_vector_smp(ele%numele,                    &
     &          d_ele(1,iphys_ele_base%i_magne), cd_prop%ex_magne,      &
     &          fem_wk%vector_1)
!
            call fem_skv_vector_inertia_upwind                          &
     &         (fluid%istack_ele_fld_smp, num_int, k2, dt,              &
     &          mhd_fem_wk%magne_1, fem_wk%vector_1,                    &
     &          d_ele(1,ie_upw), ele, g_FEM, jac_3d, fem_wk%sk6)
          end if
!
!    set SGS Lorentz force
          if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
            if (cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &            iphys_base%i_magne, iphys_SGS%i_SGS_maxwell,          &
     &            fl_prop%coef_lor, mhd_fem_wk%sgs_t1, fem_wk%tensor_1)
              call fem_skv_div_sgs_tensor_upwind                        &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            SGS_param%ifilter_final, dt, ele, g_FEM, jac_3d,      &
     &            FEM_elens, diff_coefs%Cdiff_SGS_lor, d_ele(1,ie_upw), &
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
!
      end subroutine sel_fem_skv_lorentz_upwind
!
!-----------------------------------------------------------------------
!
      end module sel_fem_skv_lorentz
