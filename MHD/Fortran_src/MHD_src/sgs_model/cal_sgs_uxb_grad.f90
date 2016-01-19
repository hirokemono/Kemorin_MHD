!
!      module cal_sgs_uxb_grad
!
!      Written by H. Matsui
!
!      subroutine cal_sgs_uxb_2_ff_grad(i_filter)
!
!!      subroutine cal_sgs_uxb_grad_4_dyn
!!      subroutine cal_sgs_filter_uxb_grad_4_dyn
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module cal_sgs_uxb_grad
!
      use m_precision
!
      use m_phys_constants
      use m_physical_property
!
      use t_geometry_data_MHD
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_finite_element_mat
      use t_filter_elength
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
      subroutine cal_sgs_uxb_2_ff_grad                                  &
     &         (i_filter, i_dvx, node, ele, conduct,                    &
     &          iphys, nod_fld, iphys_ele, ele_fld, jac_3d,             &
     &          rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_nl)
!
      use m_control_parameter
      use m_SGS_model_coefs
      use m_SGS_address
!
      use int_vol_sgs_uxb
      use cal_skv_to_ff_smp
      use product_model_coefs_to_sk
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      integer (kind=kint), intent(in) :: i_filter, i_dvx
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      call sel_int_vol_sgs_uxb(i_filter, iphys%i_magne, i_dvx,          &
     &    node, ele, conduct, nod_fld, iphys_ele, ele_fld,              &
     &    jac_3d, FEM_elens, fem_wk, mhd_fem_wk)
!
!     set elemental model coefficients
!
      call prod_model_coefs_4_vector(ele, itype_SGS_uxb_coef,           &
     &    ak_sgs(1,icomp_sgs_uxb), fem_wk%sk6)
!
      call add3_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,                &
     &    coef_induct, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine cal_sgs_uxb_2_ff_grad
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_vp_induct_grad_no_coef(i_filter,               &
     &          i_sgs, i_field, id_dx, nod_comm, node, ele,             &
     &          conduct, iphys_ele, ele_fld, jac_3d, rhs_tbl,           &
     &          FEM_elens, mhd_fem_wk, fem_wk, f_l, nod_fld)
!
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp
      use cal_for_ffs
      use nod_phys_send_recv
      use int_vol_sgs_uxb
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind=kint), intent(in) :: i_filter
      integer (kind=kint), intent(in) :: i_sgs, i_field
      integer (kind=kint), intent(in) :: id_dx
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
      call sel_int_vol_sgs_uxb(i_filter, i_field, id_dx,                &
     &    node, ele, conduct, nod_fld, iphys_ele, ele_fld,              &
     &    jac_3d, FEM_elens, fem_wk, mhd_fem_wk)
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
      end subroutine cal_sgs_vp_induct_grad_no_coef
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_uxb_grad
