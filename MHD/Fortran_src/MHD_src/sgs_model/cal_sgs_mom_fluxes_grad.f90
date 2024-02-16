!cal_sgs_mom_fluxes_grad.f90
!      module cal_sgs_mom_fluxes_grad
!
!      Written by H. Matsui on Apr., 2012
!
!!      subroutine cal_sgs_m_flux_grad_w_coef                           &
!!     &         (i_filter, i_sgs, i_field, ie_dvx, dt,                 &
!!     &          FEM_prm, SGS_param, nod_comm, node, ele, fluid,       &
!!     &          iphys_ele_base, ele_fld, jacs, FEM_elens, Csim,       &
!!     &          rhs_tbl, mlump_fl, fem_wk, mhd_fem_wk, nod_fld,       &
!!     &          v_sol, SR_sig, SR_r)
!!      subroutine cal_sgs_m_flux_grad_no_coef                          &
!!     &         (i_filter, i_sgs, i_field, ie_dvx, dt,                 &
!!     &          FEM_prm, nod_comm, node, ele, fluid,                  &
!!     &          iphys_ele_base, ele_fld, jacs, FEM_elens,             &
!!     &          rhs_tbl, mlump_fl, fem_wk, mhd_fem_wk, nod_fld,       &
!!     &          v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(jacobians_type), intent(in) :: jacs
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_model_coefficient), intent(in) :: Csim
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type (lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!          i_filter: filter ID for heat flux
!
      module cal_sgs_mom_fluxes_grad
!
      use m_precision
!
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_base_field_labels
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_SGS_model_coefs
      use t_MHD_finite_element_mat
      use t_vector_for_solver
      use t_solver_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_m_flux_grad_w_coef                             &
     &         (i_filter, i_sgs, i_field, ie_dvx, dt,                   &
     &          FEM_prm, SGS_param, nod_comm, node, ele, fluid,         &
     &          iphys_ele_base, ele_fld, jacs, FEM_elens, Csim,         &
     &          rhs_tbl, mlump_fl, fem_wk, mhd_fem_wk, nod_fld,         &
     &          v_sol, SR_sig, SR_r)
!
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp
      use nod_phys_send_recv
      use int_vol_sgs_flux
      use product_model_coefs_to_sk
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(jacobians_type), intent(in) :: jacs
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_model_coefficient), intent(in) :: Csim
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type (lumped_mass_matrices), intent(in) :: mlump_fl
!
      integer (kind=kint), intent(in) :: i_filter
      integer (kind=kint), intent(in) :: i_sgs, i_field
      integer (kind=kint), intent(in) :: ie_dvx
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call reset_sk6(n_sym_tensor, ele, fem_wk%sk6)
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
!
      call sel_int_vol_sgs_flux                                         &
     &   (FEM_prm%iflag_velo_supg, FEM_prm%npoint_t_evo_int, dt,        &
     &    i_filter, n_sym_tensor, i_field, ie_dvx,                      &
     &    node, ele, fluid, nod_fld, iphys_ele_base, ele_fld,           &
     &    jacs%g_FEM, jacs%jac_3d, FEM_elens, fem_wk, mhd_fem_wk)
!
!     set elemental model coefficients
!
      call prod_model_coefs_4_tensor                                    &
     &   (ele, SGS_param%SGS_momentum%itype_Csym_flux,                  &
     &    SGS_param%icoord_Csim, Csim%coef(1,1), fem_wk%sk6)
!
      call add6_skv_to_ff_t_smp(node, ele, rhs_tbl,                     &
     &     fem_wk%sk6, mhd_fem_wk%ff_t_smp)
      call cal_ff_smp_2_tensor(node, rhs_tbl, mhd_fem_wk%ff_t_smp,      &
     &    mlump_fl%ml, nod_fld%ntot_phys, i_sgs, nod_fld%d_fld)
!
! ----------   communications
!
      call sym_tensor_send_recv(i_sgs, nod_comm, nod_fld,               &
     &                          v_sol, SR_sig, SR_r)
!
      end subroutine cal_sgs_m_flux_grad_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_m_flux_grad_no_coef                            &
     &         (i_filter, i_sgs, i_field, ie_dvx, dt,                   &
     &          FEM_prm, nod_comm, node, ele, fluid,                    &
     &          iphys_ele_base, ele_fld, jacs, FEM_elens,               &
     &          rhs_tbl, mlump_fl, fem_wk, mhd_fem_wk, nod_fld,         &
     &          v_sol, SR_sig, SR_r)
!
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp
      use nod_phys_send_recv
      use int_vol_sgs_flux
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(jacobians_type), intent(in) :: jacs
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type (lumped_mass_matrices), intent(in) :: mlump_fl
!
      integer (kind=kint), intent(in) :: i_filter
      integer (kind=kint), intent(in) :: i_sgs, i_field
      integer (kind=kint), intent(in) :: ie_dvx
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call reset_sk6(n_sym_tensor, ele, fem_wk%sk6)
      call reset_ff_t_smp(node%max_nod_smp, mhd_fem_wk)
!
      call sel_int_vol_sgs_flux                                         &
     &   (FEM_prm%iflag_velo_supg, FEM_prm%npoint_t_evo_int, dt,        &
     &    i_filter, n_sym_tensor, i_field, ie_dvx,                      &
     &    node, ele, fluid, nod_fld, iphys_ele_base, ele_fld,           &
     &    jacs%g_FEM, jacs%jac_3d, FEM_elens, fem_wk, mhd_fem_wk)
!
      call add6_skv_to_ff_t_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, mhd_fem_wk%ff_t_smp)
      call cal_ff_smp_2_tensor(node, rhs_tbl, mhd_fem_wk%ff_t_smp,      &
     &    mlump_fl%ml, nod_fld%ntot_phys, i_sgs, nod_fld%d_fld)
!
! ----------   communications
!
      call sym_tensor_send_recv(i_sgs, nod_comm, nod_fld,               &
     &                          v_sol, SR_sig, SR_r)
!
      end subroutine cal_sgs_m_flux_grad_no_coef
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_mom_fluxes_grad
