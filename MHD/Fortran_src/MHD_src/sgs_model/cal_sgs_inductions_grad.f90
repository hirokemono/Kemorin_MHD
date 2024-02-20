!
!      module cal_sgs_inductions_grad
!
!      Written by H. Matsui
!
!!      subroutine cal_sgs_induct_t_grad_w_coef(i_filter, i_sgs, dt,    &
!!     &          FEM_prm, SGS_param, nod_comm, node, ele, conduct,     &
!!     &          cd_prop, iphys_base, iphys_ele_base, ele_fld,         &
!!     &          jacs, rhs_tbl, FEM_elen, Csim_SGS_uxb, mlump_cd,      &
!!     &          iphys_elediff_v, iphys_elediff_b, mhd_fem_wk,         &
!!     &          fem_wk, f_l, nod_fld, v_sol, SR_sig, SR_r)
!!      subroutine cal_sgs_induct_t_grad_no_coef(i_filter, i_sgs, dt,   &
!!     &          FEM_prm, nod_comm, node, ele, conduct,                &
!!     &          cd_prop, iphys_base, iphys_ele_base,                  &
!!     &          ele_fld, jacs, rhs_tbl, FEM_elen, mlump_cd,           &
!!     &          iphys_elediff_v, iphys_elediff_b, mhd_fem_wk,         &
!!     &          fem_wk,  f_l, nod_fld, v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(jacobians_type), intent(in) :: jacs
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(SGS_model_coefficient), intent(in) :: Csim_SGS_uxb
!!        type(lumped_mass_matrices), intent(in) :: mlump_cd
!!        type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module cal_sgs_inductions_grad
!
      use m_precision
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_base_field_labels
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_material_property
      use t_SGS_model_coefs
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
      subroutine cal_sgs_induct_t_grad_w_coef(i_filter, i_sgs, dt,      &
     &          FEM_prm, SGS_param, nod_comm, node, ele, conduct,       &
     &          cd_prop, iphys_base, iphys_ele_base, ele_fld,           &
     &          jacs, rhs_tbl, FEM_elen, Csim_SGS_uxb, mlump_cd,        &
     &          iphys_elediff_v, iphys_elediff_b, mhd_fem_wk,           &
     &          fem_wk, f_l, nod_fld, v_sol, SR_sig, SR_r)
!
      use int_vol_sgs_induct_t
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp
      use nod_phys_send_recv
      use product_model_coefs_to_sk
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(SGS_model_coefficient), intent(in) :: Csim_SGS_uxb
      type(lumped_mass_matrices), intent(in) :: mlump_cd
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!
      integer (kind=kint), intent(in) :: i_filter
      integer (kind=kint), intent(in) :: i_sgs
      integer (kind=kint), intent(in) :: iphys_elediff_v
      integer (kind=kint), intent(in) :: iphys_elediff_b
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_sk6(n_asym_tensor, ele, fem_wk%sk6)
      call reset_ff_smp(n_vector, node, f_l)
!
      call sel_int_vol_sgs_induct_t(i_filter, iphys_base, dt, FEM_prm,  &
     &    node, ele, conduct, nod_fld, iphys_ele_base, ele_fld,         &
     &    jacs%g_FEM, jacs%jac_3d, FEM_elen,                            &
     &    iphys_elediff_v, iphys_elediff_b, mhd_fem_wk, fem_wk)
!
!     set elemental model coefficients
!
      call prod_model_coefs_4_asym_t                                    &
     &   (ele, SGS_param%itype_Csym_uxb, SGS_param%icoord_Csim,         &
     &    Csim_SGS_uxb%coef(1,1), fem_wk%sk6)
!
      call add3_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,                &
     &    cd_prop%coef_induct, fem_wk%sk6, f_l%ff_smp)
      call cal_ff_smp_2_vector(node, rhs_tbl, f_l%ff_smp, mlump_cd%ml,  &
     &    nod_fld%ntot_phys, i_sgs, nod_fld%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_sgs, nod_comm, nod_fld,                   &
     &                      v_sol, SR_sig, SR_r)
!
      end subroutine cal_sgs_induct_t_grad_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_grad_no_coef(i_filter, i_sgs, dt,     &
     &          FEM_prm, nod_comm, node, ele, conduct,                  &
     &          cd_prop, iphys_base, iphys_ele_base,                    &
     &          ele_fld, jacs, rhs_tbl, FEM_elen, mlump_cd,             &
     &          iphys_elediff_v, iphys_elediff_b, mhd_fem_wk,           &
     &          fem_wk,  f_l, nod_fld, v_sol, SR_sig, SR_r)
!
      use int_vol_sgs_induct_t
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp
      use nod_phys_send_recv
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
      type(lumped_mass_matrices), intent(in) :: mlump_cd
!
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint), intent(in) :: i_sgs
      integer(kind = kint), intent(in) :: iphys_elediff_v
      integer(kind = kint), intent(in) :: iphys_elediff_b
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_sk6(n_asym_tensor, ele, fem_wk%sk6)
      call reset_ff_smp(n_vector, node, f_l)
!
      call sel_int_vol_sgs_induct_t(i_filter, iphys_base, dt, FEM_prm,  &
     &    node, ele, conduct, nod_fld, iphys_ele_base, ele_fld,         &
     &    jacs%g_FEM, jacs%jac_3d, FEM_elen,                            &
     &    iphys_elediff_v, iphys_elediff_b, mhd_fem_wk, fem_wk)
!
      call add3_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,                &
     &    cd_prop%coef_induct, fem_wk%sk6, f_l%ff_smp)
      call cal_ff_smp_2_vector(node, rhs_tbl, f_l%ff_smp, mlump_cd%ml,  &
     &    nod_fld%ntot_phys, i_sgs, nod_fld%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_sgs, nod_comm, nod_fld,                   &
     &                      v_sol, SR_sig, SR_r)
!
      end subroutine cal_sgs_induct_t_grad_no_coef
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_inductions_grad
