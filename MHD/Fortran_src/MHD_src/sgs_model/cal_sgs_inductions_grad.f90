!
!      module cal_sgs_inductions_grad
!
!      Written by H. Matsui
!
!!      subroutine cal_sgs_induct_t_grad_w_coef(i_filter, icomp_sgs_uxb,&
!!     &          i_sgs, ifield_v, ifield_b, ie_dvx, ie_dbx,            &
!!     &          nod_comm, node, ele, conduct, iphys_ele, ele_fld,     &
!!     &          jac_3d, rhs_tbl, FEM_elen, fem_wk, mhd_fem_wk,        &
!!     &          f_l, nod_fld)
!!      subroutine cal_sgs_induct_t_grad_no_coef                        &
!!     &         (i_filter, i_sgs, ifield_v, ifield_b, ie_dvx, ie_dbx,  &
!!     &          nod_comm, node, ele, conduct, iphys_ele, ele_fld,     &
!!     &          jac_3d, rhs_tbl, FEM_elen, fem_wk, mhd_fem_wk,        &
!!     &          f_l, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_sgs_inductions_grad
!
      use m_precision
!
      use m_control_parameter
      use m_physical_property
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_grad_w_coef(i_filter, icomp_sgs_uxb,  &
     &          i_sgs, ifield_v, ifield_b, ie_dvx, ie_dbx,              &
     &          nod_comm, node, ele, conduct, iphys_ele, ele_fld,       &
     &          jac_3d, rhs_tbl, FEM_elen, fem_wk, mhd_fem_wk,          &
     &          f_l, nod_fld)
!
      use m_SGS_model_coefs
!
      use int_vol_sgs_induct_t
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp
      use nod_phys_send_recv
      use product_model_coefs_to_sk
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
!
      integer (kind=kint), intent(in) :: i_filter, icomp_sgs_uxb
      integer (kind=kint), intent(in) :: i_sgs, ifield_v, ifield_b
      integer (kind=kint), intent(in) :: ie_dvx, ie_dbx
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_sk6(n_asym_tensor, ele, fem_wk%sk6)
      call reset_ff_smp(node%max_nod_smp, f_l)
!
      call sel_int_vol_sgs_induct_t(i_filter, ie_dvx, ie_dbx,           &
     &    ifield_v, ifield_b, node, ele, conduct,                       &
     &    nod_fld, iphys_ele, ele_fld, jac_3d, FEM_elen,                &
     &    fem_wk, mhd_fem_wk)
!
!     set elemental model coefficients
!
      call prod_model_coefs_4_asym_t(ele, itype_SGS_uxb_coef,           &
     &    sgs_coefs%ntot_comp, icomp_sgs_uxb, sgs_coefs%ak, fem_wk%sk6)
!
      call add3_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,                &
     &    coef_induct, fem_wk%sk6, f_l%ff_smp)
      call cal_ff_smp_2_vector(node, rhs_tbl,                           &
     &    f_l%ff_smp, mhd_fem_wk%mlump_cd%ml, nod_fld%ntot_phys,        &
     &    i_sgs, nod_fld%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_sgs, node, nod_comm, nod_fld)
!
      end subroutine cal_sgs_induct_t_grad_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_grad_no_coef                          &
     &         (i_filter, i_sgs, ifield_v, ifield_b, ie_dvx, ie_dbx,    &
     &          nod_comm, node, ele, conduct, iphys_ele, ele_fld,       &
     &          jac_3d, rhs_tbl, FEM_elen, fem_wk, mhd_fem_wk,          &
     &          f_l, nod_fld)
!
      use int_vol_sgs_induct_t
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
!
      integer (kind=kint), intent(in) :: i_filter
      integer (kind=kint), intent(in) :: i_sgs, ifield_v, ifield_b
      integer (kind=kint), intent(in) :: ie_dvx, ie_dbx
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_sk6(n_asym_tensor, ele, fem_wk%sk6)
      call reset_ff_smp(node%max_nod_smp, f_l)
!
      call sel_int_vol_sgs_induct_t(i_filter, ie_dvx, ie_dbx,           &
     &    ifield_v, ifield_b, node, ele, conduct,                       &
     &    nod_fld, iphys_ele, ele_fld, jac_3d, FEM_elen,                &
     &    fem_wk, mhd_fem_wk)
!
      call add3_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,                &
     &    coef_induct, fem_wk%sk6, f_l%ff_smp)
      call cal_ff_smp_2_vector(node, rhs_tbl,                           &
     &    f_l%ff_smp, mhd_fem_wk%mlump_cd%ml, nod_fld%ntot_phys,        &
     &    i_sgs, nod_fld%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_sgs, node, nod_comm, nod_fld)
!
      end subroutine cal_sgs_induct_t_grad_no_coef
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_inductions_grad
