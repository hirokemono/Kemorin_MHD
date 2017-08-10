!
!     module cal_diff_coef_temp
!
!     Written by H. Matsui
!
!!      subroutine s_cal_diff_coef_scalar(iflag_supg, num_int, dt,      &
!!     &          ifield, ifield_f, iak_diff_t, icomp_diff_t,           &
!!     &          SGS_par, nod_comm, node, ele, surf, sf_grp,           &
!!     &          Tsf_bcs, iphys, iphys_ele, ele_fld, fluid, layer_tbl, &
!!     &          jacobians, rhs_tbl, FEM_elens, filtering, mlump_fl,   &
!!     &          wk_filter, wk_cor, wk_lsq, wk_diff, fem_wk, surf_wk,  &
!!     &          f_l, f_nl, nod_fld, diff_coefs)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_type), intent(in) :: jacobians
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(dynamic_correlation_data), intent(inout) :: wk_cor
!!        type(dynamic_least_suare_data), intent(inout) :: wk_lsq
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      module cal_diff_coef_temp
!
      use m_precision
!
      use t_SGS_control_parameter
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_table_FEM_const
      use t_layering_ele_list
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_filtering_data
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_work_layer_correlate
      use t_surface_bc_data
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
      subroutine s_cal_diff_coef_scalar(iflag_supg, num_int, dt,        &
     &          ifield, ifield_f, iak_diff_t, icomp_diff_t,             &
     &          SGS_par, nod_comm, node, ele, surf, sf_grp,             &
     &          Tsf_bcs, iphys, iphys_ele, ele_fld, fluid, layer_tbl,   &
     &          jacobians, rhs_tbl, FEM_elens, filtering, mlump_fl,     &
     &          wk_filter, wk_cor, wk_lsq, wk_diff, fem_wk, surf_wk,    &
     &          f_l, f_nl, nod_fld, diff_coefs)
!
      use m_machine_parameter
      use m_phys_constants
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_gradient
      use commute_error_gradient
      use cal_model_diff_coefs
      use set_boundary_scalars
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: iflag_supg, num_int
      integer(kind = kint), intent(in) :: iak_diff_t, icomp_diff_t
      integer(kind = kint), intent(in) :: ifield, ifield_f
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_type), intent(in) :: jacobians
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(dynamic_correlation_data), intent(inout) :: wk_cor
      type(dynamic_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!    reset model coefficients
!
      call reset_diff_model_coefs(ele%numele, ele%istack_ele_smp,       &
     &    diff_coefs%num_field, iak_diff_t, diff_coefs%ak)
      call clear_work_4_dynamic_model(iphys, nod_fld)
!
!   take gradient of filtered temperature (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_gradent_in_fluid',          &
     &        iphys%i_sgs_simi, ifield_f
      call choose_cal_gradient                                          &
     &   (iflag_supg, num_int, dt, ifield_f, iphys%i_sgs_simi,          &
     &    fluid%istack_ele_fld_smp, mlump_fl,                           &
     &    nod_comm, node, ele, iphys_ele, ele_fld, jacobians%jac_3d,    &
     &    rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
!
!   take gradient of temperature (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_gradent_in_fluid',          &
     &                     iphys%i_sgs_grad, ifield
      call choose_cal_gradient                                          &
     &   (iflag_supg, num_int, dt, ifield, iphys%i_sgs_grad,            &
     &    fluid%istack_ele_fld_smp, mlump_fl,                           &
     &    nod_comm, node, ele, iphys_ele, ele_fld, jacobians%jac_3d,    &
     &    rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector_whole                                    &
     &   (SGS_par%filter_p, nod_comm, node, filtering,                  &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, wk_filter, nod_fld)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_vectors(nod_fld,                              &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys%i_sgs_simi)
!
!    modeled commutative error by second filter ( to iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'cal_commute_error_f_temp', iphys%i_sgs_grad_f
      call cal_grad_commute(num_int, fluid%istack_ele_fld_smp,          &
     &    mlump_fl, node, ele, surf, sf_grp,                            &
     &    jacobians%jac_3d, jacobians%jac_sf_grp, rhs_tbl,              &
     &    FEM_elens, Tsf_bcs%sgs, ifilter_4delta, iphys%i_sgs_grad_f,   &
     &    ifield_f, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad_f, nod_comm, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys%i_sgs_grad_f)
!
!    modeled commutative error by grid filter ( to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_commute_error_temp', iphys%i_sgs_grad
      call cal_grad_commute(num_int, fluid%istack_ele_fld_smp,          &
     &    mlump_fl, node, ele, surf, sf_grp,                            &
     &    jacobians%jac_3d, jacobians%jac_sf_grp, rhs_tbl,              &
     &    FEM_elens, Tsf_bcs%sgs, ifilter_2delta, iphys%i_sgs_grad,     &
     &    ifield, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad, nod_comm, nod_fld)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector_whole                                    &
     &   (SGS_par%filter_p, nod_comm, node, filtering,                  &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, wk_filter, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_vector, iak_diff_t, icomp_diff_t
      call cal_diff_coef_fluid                                          &
     &   (SGS_par, layer_tbl, node, ele, fluid, iphys, nod_fld,         &
     &    jacobians%jac_3d, jacobians%jac_3d_l,                         &
     &    n_vector, iak_diff_t, icomp_diff_t, num_int,                  &
     &    wk_cor, wk_lsq, wk_diff, diff_coefs)
!
      diff_coefs%iflag_field(iak_diff_t) = 1
!
      end subroutine s_cal_diff_coef_scalar
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_temp
