!
!      module cal_sgs_uxb_grad
!
!      Written by H. Matsui
!
!!      subroutine cal_sgs_uxb_2_ff_grad(itype_Csym_uxb, icoord_Csim,   &
!!     &          i_filter, icomp_sgs_uxb, ie_dvx, dt,                  &
!!     &          FEM_prm, node, ele, conduct, cd_prop, iphys, nod_fld, &
!!     &          iphys_ele, ele_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,&
!!     &          sgs_coefs, mhd_fem_wk, fem_wk, f_nl)
!!      subroutine cal_sgs_vp_induct_grad_no_coef                       &
!!     &         (i_filter,  i_sgs, i_field, id_dx, dt,                 &
!!     &          FEM_prm, nod_comm, node, ele, conduct,                &
!!     &          cd_prop, iphys_ele, ele_fld, g_FEM, jac_3d, rhs_tbl,  &
!!     &          FEM_elens, mlump_cd, mhd_fem_wk, fem_wk, f_l, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(lumped_mass_matrices), intent(in) :: mlump_cd
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module cal_sgs_uxb_grad
!
      use m_precision
!
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_physical_property
      use t_geometry_data_MHD
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_SGS_model_coefs
      use t_MHD_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_2_ff_grad(itype_Csym_uxb, icoord_Csim,     &
     &          i_filter, icomp_sgs_uxb, ie_dvx, dt,                    &
     &          FEM_prm, node, ele, conduct, cd_prop, iphys, nod_fld,   &
     &          iphys_ele, ele_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,  &
     &          sgs_coefs, mhd_fem_wk, fem_wk, f_nl)
!
      use int_vol_sgs_uxb
      use cal_skv_to_ff_smp
      use product_model_coefs_to_sk
!
      integer (kind=kint), intent(in) :: itype_Csym_uxb, icoord_Csim
      integer (kind=kint), intent(in) :: i_filter, icomp_sgs_uxb
      integer (kind=kint), intent(in) :: ie_dvx
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      call sel_int_vol_sgs_uxb(i_filter, iphys%i_magne, ie_dvx, dt,     &
     &    FEM_prm, node, ele, conduct, nod_fld, iphys_ele, ele_fld,     &
     &    g_FEM, jac_3d, FEM_elens, fem_wk, mhd_fem_wk)
!
!     set elemental model coefficients
!
      call prod_model_coefs_4_vector(ele, itype_Csym_uxb, icoord_Csim,  &
     &    sgs_coefs%ntot_comp, icomp_sgs_uxb, sgs_coefs%ak, fem_wk%sk6)
!
      call add3_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,                &
     &    cd_prop%coef_induct, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine cal_sgs_uxb_2_ff_grad
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_vp_induct_grad_no_coef                         &
     &         (i_filter,  i_sgs, i_field, id_dx, dt,                   &
     &          FEM_prm, nod_comm, node, ele, conduct,                  &
     &          cd_prop, iphys_ele, ele_fld, g_FEM, jac_3d, rhs_tbl,    &
     &          FEM_elens, mlump_cd, mhd_fem_wk, fem_wk, f_l, nod_fld)
!
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp
      use cal_for_ffs
      use nod_phys_send_recv
      use int_vol_sgs_uxb
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(lumped_mass_matrices), intent(in) :: mlump_cd
!
      integer (kind=kint), intent(in) :: i_filter
      integer (kind=kint), intent(in) :: i_sgs, i_field
      integer (kind=kint), intent(in) :: id_dx
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
      call reset_ff_smp(node%max_nod_smp, f_l)
!
      call sel_int_vol_sgs_uxb(i_filter, i_field, id_dx, dt,            &
     &    FEM_prm, node, ele, conduct, nod_fld, iphys_ele, ele_fld,     &
     &    g_FEM, jac_3d, FEM_elens, fem_wk, mhd_fem_wk)
!
      call add3_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,                &
     &    cd_prop%coef_induct, fem_wk%sk6, f_l%ff_smp)
      call cal_ff_smp_2_vector(node, rhs_tbl, f_l%ff_smp, mlump_cd%ml,  &
     &    nod_fld%ntot_phys, i_sgs, nod_fld%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_sgs, nod_comm, nod_fld)
!
      end subroutine cal_sgs_vp_induct_grad_no_coef
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_uxb_grad
