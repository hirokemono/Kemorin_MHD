!>@file   cal_diff_coef_temp.f90
!!@brief  module cal_diff_coef_temp
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2005
!
!>@brief  Evaluate model coefficients for scalar differentiation
!!
!!@verbatim
!!      subroutine s_cal_diff_coef_scalar(iflag_SGS_initial,            &
!!     &        iflag_supg, num_int, dt, ifield, ifield_f,              &
!!     &        SGS_param, cmt_param, filter_param,                     &
!!     &        nod_comm, node, ele, surf, sf_grp, Tsf_bcs,             &
!!     &        iphys_SGS_wk, iphys_ele_base, ele_fld, fluid, layer_tbl,&
!!     &        jacs, rhs_tbl, FEM_elens, filtering, mlump_fl,          &
!!     &        wk_filter, wk_cor, wk_lsq, wk_diff, fem_wk, surf_wk,    &
!!     &        f_l, f_nl, nod_fld, Cdiff_scalar, v_sol, SR_sig, SR_r)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        integer(kind = kint), intent(in) :: iflag_SGS_initial
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(SGS_filtering_params), intent(in) :: filter_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_type), intent(in) :: jacs
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
!!        type(SGS_model_coefficient), intent(inout) :: Cdiff_scalar
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
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
      use t_base_field_labels
      use t_SGS_model_coef_labels
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
      use t_surface_bc_scalar
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
      subroutine s_cal_diff_coef_scalar(iflag_SGS_initial,              &
     &        iflag_supg, num_int, dt, ifield, ifield_f,                &
     &         SGS_param, cmt_param, filter_param,                      &
     &         nod_comm, node, ele, surf, sf_grp, Tsf_bcs,              &
     &         iphys_SGS_wk, iphys_ele_base, ele_fld, fluid, layer_tbl, &
     &         jacs, rhs_tbl, FEM_elens, filtering, mlump_fl,           &
     &         wk_filter, wk_cor, wk_lsq, wk_diff, fem_wk, surf_wk,     &
     &         f_l, f_nl, nod_fld, Cdiff_scalar, v_sol, SR_sig, SR_r)
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
      integer(kind = kint), intent(in) :: ifield, ifield_f
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: iflag_SGS_initial
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(SGS_filtering_params), intent(in) :: filter_param
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_type), intent(in) :: jacs
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
      type(SGS_model_coefficient), intent(inout) :: Cdiff_scalar
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!    reset model coefficients
!
      call reset_diff_model_coefs(ele%numele, ele%istack_ele_smp,       &
     &                            Cdiff_scalar%coef(1,1))
      call clear_work_4_dynamic_model(iphys_SGS_wk, nod_fld)
!
!   take gradient of filtered temperature (to iphys_SGS_wk%i_simi)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_gradent_in_fluid',          &
     &        iphys_SGS_wk%i_simi, ifield_f
      call choose_cal_gradient                                          &
     &   (iflag_supg, num_int, dt, ifield_f, iphys_SGS_wk%i_simi,       &
     &    fluid%istack_ele_fld_smp, mlump_fl,                           &
     &    nod_comm, node, ele, iphys_ele_base, ele_fld, jacs%g_FEM,     &
     &    jacs%jac_3d,  rhs_tbl, fem_wk, f_l, f_nl, nod_fld,            &
     &    v_sol, SR_sig, SR_r)
!
!   take gradient of temperature (to iphys_SGS_wk%i_nlg)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_gradent_in_fluid',          &
     &                     iphys_SGS_wk%i_nlg, ifield
      call choose_cal_gradient                                          &
     &   (iflag_supg, num_int, dt, ifield, iphys_SGS_wk%i_nlg,          &
     &    fluid%istack_ele_fld_smp, mlump_fl,                           &
     &    nod_comm, node, ele, iphys_ele_base, ele_fld, jacs%g_FEM,     &
     &    jacs%jac_3d, rhs_tbl, fem_wk, f_l, f_nl, nod_fld,             &
     &    v_sol, SR_sig, SR_r)
!
!    filtering (to iphys_SGS_wk%i_nlg)
!
      call cal_filtered_vector_whole                                    &
     &   (filter_param, nod_comm, node, filtering,                      &
     &    iphys_SGS_wk%i_nlg, iphys_SGS_wk%i_nlg, wk_filter,            &
     &    nod_fld, v_sol, SR_sig, SR_r)
!
!    take difference (to iphys_SGS_wk%i_simi)
!
      call subtract_2_nod_vectors(nod_fld,                              &
     &    iphys_SGS_wk%i_nlg, iphys_SGS_wk%i_simi, iphys_SGS_wk%i_simi)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys_SGS_wk%i_simi)
!
!    modeled commutative error by second filter
!                               (to iphys_SGS_wk%i_wd_nlg)
!
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'cal_commute_error_f_temp', iphys_SGS_wk%i_wd_nlg
      call cal_grad_commute(num_int, fluid%istack_ele_fld_smp,          &
     &    mlump_fl, node, ele, surf, sf_grp,                            &
     &    jacs%g_FEM, jacs%jac_3d, jacs%jac_sf_grp, rhs_tbl,            &
     &    FEM_elens, Tsf_bcs%sgs, ifilter_4delta,                       &
     &    iphys_SGS_wk%i_wd_nlg, ifield_f, fem_wk, surf_wk,             &
     &    f_l, f_nl, nod_fld)
!
      call vector_send_recv(iphys_SGS_wk%i_wd_nlg, nod_comm,            &
     &                      nod_fld, v_sol, SR_sig, SR_r)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys_SGS_wk%i_wd_nlg)
!
!    modeled commutative error by grid filter ( to iphys_SGS_wk%i_nlg)
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_commute_error_temp', iphys_SGS_wk%i_nlg
      call cal_grad_commute(num_int, fluid%istack_ele_fld_smp,          &
     &    mlump_fl, node, ele, surf, sf_grp,                            &
     &    jacs%g_FEM, jacs%jac_3d, jacs%jac_sf_grp, rhs_tbl,            &
     &    FEM_elens, Tsf_bcs%sgs, ifilter_2delta, iphys_SGS_wk%i_nlg,   &
     &    ifield, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      call vector_send_recv(iphys_SGS_wk%i_nlg, nod_comm,               &
     &                      nod_fld, v_sol, SR_sig, SR_r)
!
!    filtering (to iphys_SGS_wk%i_nlg)
!
      call cal_filtered_vector_whole                                    &
     &   (filter_param, nod_comm, node, filtering,                      &
     &    iphys_SGS_wk%i_nlg, iphys_SGS_wk%i_nlg, wk_filter,            &
     &    nod_fld, v_sol, SR_sig, SR_r)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys_SGS_wk%i_nlg)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)  'cal_diff_coef_fluid',         &
     &        n_vector, Cdiff_scalar%iak_Csim, Cdiff_scalar%icomp_Csim
      call cal_diff_coef_fluid(iflag_SGS_initial, SGS_param, cmt_param, &
     &    layer_tbl, node, ele, fluid, iphys_SGS_wk, nod_fld, jacs,     &
     &    n_vector, num_int, wk_cor, wk_lsq, wk_diff, Cdiff_scalar)
!
      Cdiff_scalar%flag_set = .TRUE.
!
      end subroutine s_cal_diff_coef_scalar
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_temp
