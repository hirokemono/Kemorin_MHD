!
!     module cal_diff_coef_temp
!
!     Written by H. Matsui
!
!!      subroutine s_cal_diff_coef_temp(iak_diff_t, icomp_diff_t,       &
!!     &         nod_comm, node, ele, surf, sf_grp,                     &
!!     &         iphys, iphys_ele, ele_fld, fluid, layer_tbl,           &
!!     &         jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,             &
!!     &         FEM_elens, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_diff_coef_temp
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_2d
      use t_jacobian_3d
      use t_table_FEM_const
      use t_layering_ele_list
      use t_MHD_finite_element_mat
      use t_filter_elength
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_diff_coef_temp(iak_diff_t, icomp_diff_t,         &
     &          nod_comm, node, ele, surf, sf_grp,                      &
     &          iphys, iphys_ele, ele_fld, fluid, layer_tbl,            &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,              &
     &          FEM_elens, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_machine_parameter
      use m_control_parameter
      use m_phys_constants
      use m_surf_data_temp
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_filtering_vectors
      use cal_gradient
      use commute_error_gradient
      use cal_model_diff_coefs
      use set_boundary_scalars
      use nod_phys_send_recv
      use clear_work_4_dynamic_model
!
      integer (kind=kint), intent(in) :: iak_diff_t, icomp_diff_t
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!    reset model coefficients
!
      call reset_diff_model_coefs(iak_diff_t, ele%istack_ele_smp)
      call s_clear_work_4_dynamic_model(node, iphys, nod_fld)
!
!   take gradient of filtered temperature (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_gradent_in_fluid',          &
     &        iphys%i_sgs_simi, iphys%i_filter_temp
      call choose_cal_gradient                                          &
     &   (iflag_temp_supg, iphys%i_filter_temp, iphys%i_sgs_simi,       &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl,                &
     &    nod_comm, node, ele, iphys_ele, ele_fld, jac_3d_q,            &
     &    rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
!
!   take gradient of temperature (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_gradent_in_fluid',          &
     &                     iphys%i_sgs_grad, iphys%i_sgs_temp
      call choose_cal_gradient                                          &
     &   (iflag_temp_supg, iphys%i_sgs_temp, iphys%i_sgs_grad,          &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl,                &
     &    nod_comm, node, ele, iphys_ele, ele_fld, jac_3d_q,            &
     &    rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector(nod_comm, node,                         &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, nod_fld)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_vectors(node, nod_fld,                        &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld, n_vector, iphys%i_sgs_simi)
!
!    modeled commutative error by second filter ( to iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'cal_commute_error_f_temp', iphys%i_sgs_grad_f
      call cal_grad_commute                                             &
     &   (fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl,                &
     &    node, ele, surf, sf_grp, jac_3d_q, jac_sf_grp_q,              &
     &    rhs_tbl, FEM_elens, Tsf1_bcs%sgs, ifilter_4delta,             &
     &    iphys%i_sgs_grad_f, iphys%i_filter_temp,                      &
     &    fem_wk, f_l, f_nl, nod_fld)
!
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad_f, node, nod_comm, nod_fld)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld, n_vector, iphys%i_sgs_grad_f)
!
!    modeled commutative error by grid filter ( to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_commute_error_temp', iphys%i_sgs_grad
      call cal_grad_commute                                             &
     &   (fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl,                &
     &    node, ele, surf, sf_grp, jac_3d_q, jac_sf_grp_q,              &
     &    rhs_tbl, FEM_elens, Tsf1_bcs%sgs, ifilter_2delta,             &
     &    iphys%i_sgs_grad, iphys%i_sgs_temp,                           &
     &    fem_wk, f_l, f_nl, nod_fld)
!
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad, node, nod_comm, nod_fld)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector(nod_comm, node,                          &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, nod_fld)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld, n_vector, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_vector, iak_diff_t, icomp_diff_t
      call cal_diff_coef_fluid(layer_tbl,                               &
     &    node, ele, fluid, iphys, nod_fld, jac_3d_q, jac_3d_l,         &
     &    n_vector, iak_diff_t, icomp_diff_t, intg_point_t_evo)
!
      iflag_diff_coefs(iak_diff_t) = 1
!
      end subroutine s_cal_diff_coef_temp
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_temp
