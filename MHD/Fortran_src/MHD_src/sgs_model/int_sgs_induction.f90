!
!     module int_sgs_induction
!
!        programmed by H.Matsui on July, 2005
!        modified by H.Matsui on AUg., 2007
!
!!      subroutine int_vol_sgs_induction                                &
!!     &         (FEM_prm, nod_comm, node, ele, conduct, iphys, jac_3d, &
!!     &          rhs_tbl, mlump_cd, mhd_fem_wk, fem_wk, f_nl, nod_fld)
!!      subroutine cal_sgs_uxb_2_monitor(icomp_sgs_uxb, ie_dvx, dt,     &
!!     &          FEM_prm, SGS_param, filter_param, nod_comm, node, ele,&
!!     &          conduct, cd_prop, iphys, iphys_ele, ele_fld, jac_3d,  &
!!     &          rhs_tbl, FEM_elen, filtering, sgs_coefs, mlump_cd,    &
!!     &          wk_filter, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(SGS_filtering_params), intent(in) :: filter_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type (lumped_mass_matrices), intent(in) :: mlump_cd
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module int_sgs_induction
!
      use m_precision
      use m_machine_parameter
!
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use m_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
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
      subroutine int_vol_sgs_induction                                  &
     &         (FEM_prm, nod_comm, node, ele, conduct, iphys, jac_3d,   &
     &          rhs_tbl, mlump_cd, mhd_fem_wk, fem_wk, f_nl, nod_fld)
!
      use int_vol_vect_differences
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(phys_address), intent(in) :: iphys
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
      type(lumped_mass_matrices), intent(in) :: mlump_cd
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smp(node%max_nod_smp, f_nl)
!
      call int_vol_rotation                                             &
     &   (node, ele, g_FEM1, jac_3d, rhs_tbl, nod_fld,                  &
     &    conduct%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &    iphys%i_SGS_vp_induct, fem_wk, f_nl)
!
!      call cal_multi_pass_4_vector_ff                                  &
!     &   (ele%istack_ele_smp, FEM_prm, m1_lump, nod_comm, node, ele,   &
!     &    g_FEM1, jac_3d, rhs_tbl, mhd_fem_wk%ff_m_smp,                &
!     &    fem_wk, f_l, f_nl)
!      call cal_ff_2_vector(node%numnod, node%istack_nod_smp, f_l%ff,   &
!     &    mlump_cd%ml, nod_fld%ntot_phys, iphys%i_magne, nod_fld%d_fld)
       call cal_ff_smp_2_vector                                         &
     &    (node, rhs_tbl, f_nl%ff_smp, mlump_cd%ml,                     &
     &     nod_fld%ntot_phys, iphys%i_SGS_induction, nod_fld%d_fld)
!
       call vector_send_recv                                            &
     &    (iphys%i_SGS_induction, nod_comm, nod_fld)
!
      end subroutine int_vol_sgs_induction
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_2_monitor(icomp_sgs_uxb, ie_dvx, dt,       &
     &          FEM_prm, SGS_param, filter_param, nod_comm, node, ele,  &
     &          conduct, cd_prop, iphys, iphys_ele, ele_fld, jac_3d,    &
     &          rhs_tbl, FEM_elen, filtering, sgs_coefs, mlump_cd,      &
     &          wk_filter, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use cal_sgs_fluxes
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: icomp_sgs_uxb, ie_dvx
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type (lumped_mass_matrices), intent(in) :: mlump_cd
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
      call cal_sgs_uxb_2_evo(icomp_sgs_uxb, ie_dvx, dt,                 &
     &    FEM_prm, SGS_param, filter_param, nod_comm, node, ele,        &
     &    conduct, cd_prop, iphys, iphys_ele, ele_fld, jac_3d, rhs_tbl, &
     &    FEM_elen, filtering, sgs_coefs, wk_filter,                    &
     &    mhd_fem_wk, fem_wk, f_nl, nod_fld)
!
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
      call cal_ff_2_vector                                              &
     &   (node%numnod, node%istack_nod_smp, f_nl%ff, mlump_cd%ml,       &
     &    nod_fld%ntot_phys, iphys%i_SGS_vp_induct, nod_fld%d_fld)
      call vector_send_recv                                             &
     &   (iphys%i_SGS_vp_induct, nod_comm, nod_fld)
!
      end subroutine cal_sgs_uxb_2_monitor
!
!-----------------------------------------------------------------------
!
      end module int_sgs_induction
