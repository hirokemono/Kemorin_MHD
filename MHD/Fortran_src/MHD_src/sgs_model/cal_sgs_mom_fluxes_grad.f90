!cal_sgs_mom_fluxes_grad.f90
!      module cal_sgs_mom_fluxes_grad
!
!      Written by H. Matsui on Apr., 2012
!
!!      subroutine cal_sgs_m_flux_grad_w_coef(itype_csim, i_filter,     &
!!     &          icm_sgs, i_sgs, i_field, ie_dvx, nod_comm,            &
!!     &          node, ele, fluid, iphys_ele, ele_fld, jac_3d,         &
!!     &          FEM_elens, rhs_tbl, fem_wk, mhd_fem_wk, nod_fld)
!!      subroutine cal_sgs_m_flux_grad_no_coef                          &
!!     &         (i_filter, i_sgs, i_field, ie_dvx, nod_comm,           &
!!     &          node, ele, fluid, iphys_ele, ele_fld, jac_3d,         &
!!     &          FEM_elens, rhs_tbl, fem_wk, mhd_fem_wk, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(phys_data), intent(inout) :: nod_fld
!!         i_filter: filter ID for heat flux
!
      module cal_sgs_mom_fluxes_grad
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
      use m_SGS_model_coefs
      use m_SGS_address
!
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
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
      subroutine cal_sgs_m_flux_grad_w_coef(itype_csim, i_filter,       &
     &          icm_sgs, i_sgs, i_field, ie_dvx, nod_comm,              &
     &          node, ele, fluid, iphys_ele, ele_fld, jac_3d,           &
     &          FEM_elens, rhs_tbl, fem_wk, mhd_fem_wk, nod_fld)
!
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp
      use nod_phys_send_recv
      use int_vol_sgs_flux
      use product_model_coefs_to_sk
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer (kind=kint), intent(in) :: itype_csim
      integer (kind=kint), intent(in) :: i_filter, icm_sgs
      integer (kind=kint), intent(in) :: i_sgs, i_field
      integer (kind=kint), intent(in) :: ie_dvx
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_sk6(n_sym_tensor, ele, fem_wk%sk6)
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
!
      call sel_int_vol_sgs_flux                                         &
     &   (iflag_velo_supg, i_filter, n_sym_tensor, i_field, ie_dvx,     &
     &    node, ele, fluid, nod_fld, iphys_ele, ele_fld,                &
     &    jac_3d, FEM_elens, fem_wk, mhd_fem_wk)
!
!     set elemental model coefficients
!
      call prod_model_coefs_4_tensor(ele, itype_csim,                   &
     &    ak_sgs(1,icm_sgs), fem_wk%sk6)
!
      call add6_skv_to_ff_t_smp(node, ele, rhs_tbl,                     &
     &     fem_wk%sk6, mhd_fem_wk%ff_t_smp)
      call cal_ff_smp_2_tensor(node, rhs_tbl, mhd_fem_wk%ff_t_smp,      &
     &    mhd_fem_wk%mlump_fl%ml, nod_fld%ntot_phys,                    &
     &    i_sgs, nod_fld%d_fld)
!
! ----------   communications
!
      call sym_tensor_send_recv(i_sgs, node, nod_comm, nod_fld)
!
      end subroutine cal_sgs_m_flux_grad_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_m_flux_grad_no_coef                            &
     &         (i_filter, i_sgs, i_field, ie_dvx, nod_comm,             &
     &          node, ele, fluid, iphys_ele, ele_fld, jac_3d,           &
     &          FEM_elens, rhs_tbl, fem_wk, mhd_fem_wk, nod_fld)
!
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp
      use nod_phys_send_recv
      use int_vol_sgs_flux
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer (kind=kint), intent(in) :: i_filter
      integer (kind=kint), intent(in) :: i_sgs, i_field
      integer (kind=kint), intent(in) :: ie_dvx
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_sk6(n_sym_tensor, ele, fem_wk%sk6)
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
!
      call sel_int_vol_sgs_flux                                         &
     &   (iflag_velo_supg, i_filter, n_sym_tensor, i_field, ie_dvx,     &
     &    node, ele, fluid, nod_fld, iphys_ele, ele_fld,                &
     &    jac_3d, FEM_elens, fem_wk, mhd_fem_wk)
!
      call add6_skv_to_ff_t_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, mhd_fem_wk%ff_t_smp)
      call cal_ff_smp_2_tensor(node, rhs_tbl, mhd_fem_wk%ff_t_smp,      &
     &    mhd_fem_wk%mlump_fl%ml, nod_fld%ntot_phys,                    &
     &    i_sgs, nod_fld%d_fld)
!
! ----------   communications
!
      call sym_tensor_send_recv(i_sgs, node, nod_comm, nod_fld)
!
      end subroutine cal_sgs_m_flux_grad_no_coef
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_mom_fluxes_grad
