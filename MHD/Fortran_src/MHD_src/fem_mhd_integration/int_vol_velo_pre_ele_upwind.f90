!>@file   int_vol_velo_pre_ele_upwind.f90
!!@brief  module int_vol_velo_pre_ele_upwind
!!
!!@author H. Matsui and H.Okuda 
!!@date Programmed in July 2000 (ver 1.1)
!!        modified by H. Matsui in Oct., 2005
!!        modified by H. Matsui in Aug., 2007
!!
!>@brief  Finite elememt integration for momentum equation
!!
!!@verbatim
!!      subroutine s_int_vol_velo_pre_ele_upw(iflag_4_rotate, num_int,  &
!!     &          dt, SGS_param, cmt_param, node, ele, fluid,           &
!!     &          fl_prop, cd_prop, iphys_base, iphys_fil, iphys_SGS,   &
!!     &          nod_fld, ak_MHD, ele_fld, ie_upw, iphys_ele_base,     &
!!     &          g_FEM, jac_3d, rhs_tbl, FEM_elens, diff_coefs,        &
!!     &          mhd_fem_wk, fem_wk, f_nl)
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
!!        type(SGS_commutation_coefs), intent(in) :: diff_coefs
!!        type(phys_data), intent(in) :: ele_fld
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!@endverbatim
!
      module int_vol_velo_pre_ele_upwind
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
      use t_SGS_model_addresses
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_FEM_SGS_model_coefs
      use t_SGS_commutation_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_int_vol_velo_pre_ele_upw(iflag_4_rotate, num_int,    &
     &          dt, SGS_param, cmt_param, node, ele, fluid,             &
     &          fl_prop, cd_prop, iphys_base, iphys_LES,                &
     &          nod_fld, ak_MHD, ele_fld, ie_upw, iphys_ele_base,       &
     &          g_FEM, jac_3d, rhs_tbl, FEM_elens, diff_coefs,          &
     &          mhd_fem_wk, fem_wk, f_nl)
!
      use cal_add_smp
      use nodal_fld_cst_to_element
      use int_vol_buoyancy
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_nodal_fld_upwind
      use fem_skv_nonlinear_upwind
      use fem_skv_div_sgs_flux_upw
      use fem_skv_lorentz_full
      use fem_skv_div_flux_upw
      use sel_fem_skv_inertia
      use sel_fem_skv_rot_inertia
      use sel_fem_skv_lorentz
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(base_field_address), intent(in) :: iphys_base
      type(SGS_model_addresses), intent(in) :: iphys_LES
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
      type(SGS_commutation_coefs), intent(in) :: diff_coefs
      type(phys_data), intent(in) :: ele_fld
!
      integer(kind = kint), intent(in) :: iflag_4_rotate, num_int
      integer(kind = kint), intent(in) :: ie_upw
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
        if(fl_prop%coef_nega_v .ne. 0.0d0) then
!  -----  Inertia including Reynolds stress by rotation form --------
          if(iflag_4_rotate .eq. id_turn_ON) then
            call sel_fem_skv_rot_inertia_upwind                         &
     &         (k2, num_int, dt, SGS_param, node, ele, fluid, fl_prop,  &
     &          iphys_base, iphys_LES%SGS_term, nod_fld,                &
     &          ele_fld%ntot_phys, ie_upw, ele_fld%d_fld,               &
     &          iphys_ele_base, g_FEM, jac_3d, FEM_elens, diff_coefs,   &
     &          mhd_fem_wk, fem_wk)
!
!  -----  Inertia including Reynolds stress --------
          else
            call sel_fem_skv_inertia_upwind(k2, num_int, dt, SGS_param, &
     &          node, ele, fluid, fl_prop, iphys_base,                  &
     &          iphys_LES%SGS_term, nod_fld, ele_fld%ntot_phys,         &
     &          ie_upw, ele_fld%d_fld, iphys_ele_base, g_FEM, jac_3d,   &
     &          FEM_elens, diff_coefs, mhd_fem_wk, fem_wk)
          end if
        end if
!
!  -----   set Lorentz force  --------
!
        if(fl_prop%flag_lorentz) then
          call sel_fem_skv_lorentz_upwind(k2, iflag_4_rotate,           &
     &        num_int, dt, SGS_param, cmt_param, node, ele, fluid,      &
     &        fl_prop, cd_prop, iphys_base, iphys_LES%SGS_term,         &
     &        nod_fld, ele_fld%ntot_phys, ie_upw, ele_fld%d_fld,        &
     &        iphys_ele_base, g_FEM, jac_3d, FEM_elens, diff_coefs,     &
     &        mhd_fem_wk, fem_wk)
        end if
!
!  --------  set coriolis force
!
        if(fl_prop%flag_coriolis                                        &
     &     .and. fl_prop%iflag_FEM_coriolis .eq. id_FORCE_ele_int) then
          call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,       &
     &        iphys_base%i_velo, fl_prop%coef_cor, mhd_fem_wk%velo_1)
          call fem_skv_coriolis_upwind(fluid%istack_ele_fld_smp,        &
     &        num_int, k2, dt, mhd_fem_wk%velo_1, fl_prop%sys_rot,      &
     &        ele_fld%d_fld(1,ie_upw), ele, g_FEM, jac_3d, fem_wk%sk6)
        end if
!
! ---------  set buoyancy
        if(fl_prop%iflag_FEM_gravity .eq. id_FORCE_ele_int) then
          call sel_gravity_vec_each_ele                                 &
     &       (k2, node, ele, fl_prop, iphys_base, iphys_LES%filter_fld, &
     &        nod_fld, ak_MHD, fem_wk%vector_1)
          call fem_skv_vector_field_upwind(fluid%istack_ele_fld_smp,    &
     &        num_int, k2, dt, ele_fld%d_fld(1,ie_upw),                 &
     &        ele, g_FEM, jac_3d, fem_wk%vector_1, fem_wk%sk6)
        end if
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine s_int_vol_velo_pre_ele_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_velo_pre_ele_upwind
