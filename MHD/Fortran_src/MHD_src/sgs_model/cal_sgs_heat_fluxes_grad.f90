!cal_sgs_heat_fluxes_grad.f90
!      module cal_sgs_heat_fluxes_grad
!
!      Written by H. Matsui
!
!!      subroutine cal_sgs_h_flux_grad_w_coef                           &
!!     &         (i_filter, icomp_sgs_hf, i_sgs, i_field, ie_dvx,       &
!!     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,       &
!!     &          jac_3d, rhs_tbl, FEM_elens, sgs_coefs,                &
!!     &          mhd_fem_wk, fem_wk, f_l, nod_fld)
!!      subroutine cal_sgs_h_flux_grad_no_coef                          &
!!     &         (i_filter, i_sgs, i_field, ie_dvx,                     &
!!     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,       &
!!     &          jac_3d, rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk,       &
!!     &          f_l, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(MHD_coefficients_type), intent(in) :: sgs_coefs
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!        type(phys_data), intent(inout) :: nod_fld
!         i_filter: filter ID for heat flux
!
      module cal_sgs_heat_fluxes_grad
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
!
      use t_comm_table
      use t_geometry_data
      use t_geometry_data_MHD
      use t_phys_address
      use t_phys_data
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
      use t_material_property
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_h_flux_grad_w_coef                             &
     &         (i_filter, icomp_sgs_hf, i_sgs, i_field, ie_dvx,         &
     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,         &
     &          jac_3d, rhs_tbl, FEM_elens, sgs_coefs,                  &
     &          mhd_fem_wk, fem_wk, f_l, nod_fld)
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
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_coefficients_type), intent(in) :: sgs_coefs
!
      integer (kind=kint), intent(in) :: i_filter, icomp_sgs_hf
      integer (kind=kint), intent(in) :: i_sgs, i_field
      integer (kind=kint), intent(in) :: ie_dvx
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
      call reset_ff_smp(node%max_nod_smp, f_l)
!
      call sel_int_vol_sgs_flux                                         &
     &   (iflag_temp_supg, i_filter, n_vector, i_field, ie_dvx,         &
     &    node, ele, fluid, nod_fld, iphys_ele, ele_fld,                &
     &    jac_3d, FEM_elens, fem_wk, mhd_fem_wk)
!
!     set elemental model coefficients
!
      call prod_model_coefs_4_vector(ele, itype_SGS_h_flux_coef,        &
     &    sgs_coefs%ntot_comp, icomp_sgs_hf, sgs_coefs%ak, fem_wk%sk6)
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_l%ff_smp)
      call cal_ff_smp_2_vector(node, rhs_tbl,                           &
     &    f_l%ff_smp, mhd_fem_wk%mlump_fl%ml,                           &
     &    nod_fld%ntot_phys, i_sgs, nod_fld%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_sgs, nod_comm, nod_fld)
!
      end subroutine cal_sgs_h_flux_grad_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_h_flux_grad_no_coef                            &
     &         (i_filter, i_sgs, i_field, ie_dvx,                       &
     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,         &
     &          jac_3d, rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk,         &
     &          f_l, nod_fld)
!
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp
      use nod_phys_send_recv
      use int_vol_sgs_flux
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind=kint), intent(in) :: i_filter
      integer (kind=kint), intent(in) :: i_sgs, i_field
      integer (kind=kint), intent(in) :: ie_dvx
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
      call reset_ff_smp(node%max_nod_smp, f_l)
!
      call sel_int_vol_sgs_flux                                         &
     &   (iflag_temp_supg, i_filter, n_vector, i_field, ie_dvx,         &
     &    node, ele, fluid, nod_fld, iphys_ele, ele_fld,                &
     &    jac_3d, FEM_elens, fem_wk, mhd_fem_wk)
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_l%ff_smp)
      call cal_ff_smp_2_vector(node, rhs_tbl,                           &
     &    f_l%ff_smp, mhd_fem_wk%mlump_fl%ml,                           &
     &    nod_fld%ntot_phys, i_sgs, nod_fld%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_sgs, nod_comm, nod_fld)
!
      end subroutine cal_sgs_h_flux_grad_no_coef
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_heat_fluxes_grad
