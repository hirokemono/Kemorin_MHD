!
!     module cal_diff_coef_magne
!
!     Written by H. Matsui
!
!!      subroutine s_cal_diff_coef_magne                                &
!!     &         (iak_diff_b, icomp_diff_b, dt, FEM_prm, SGS_par,       &
!!     &          nod_comm, node, ele, surf, sf_grp, Bsf_bcs, Fsf_bcs,  &
!!     &          iphys, iphys_ele, ele_fld, fluid, layer_tbl,          &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,            &
!!     &          FEM_elens, filtering, m_lump, wk_filter,              &
!!     &          wk_cor, wk_lsq, wk_diff, fem_wk, surf_wk,             &
!!     &          f_l, f_nl, nod_fld, diff_coefs)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Fsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(dynamis_correlation_data), intent(inout) :: wk_cor
!!        type(dynamis_least_suare_data), intent(inout) :: wk_lsq
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      module cal_diff_coef_magne
!
      use m_precision
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
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
      subroutine s_cal_diff_coef_magne                                  &
     &         (iak_diff_b, icomp_diff_b, dt, FEM_prm, SGS_par,         &
     &          nod_comm, node, ele, surf, sf_grp, Bsf_bcs, Fsf_bcs,    &
     &          iphys, iphys_ele, ele_fld, fluid, layer_tbl,            &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,              &
     &          FEM_elens, filtering, m_lump, wk_filter,                &
     &          wk_cor, wk_lsq, wk_diff, fem_wk, surf_wk,               &
     &          f_l, f_nl, nod_fld, diff_coefs)
!
      use m_machine_parameter
      use m_phys_constants
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_rotation
      use cal_divergence
      use cal_gradient
      use commute_error_gradient
      use cal_model_diff_coefs
      use nod_phys_send_recv
!
      integer (kind=kint), intent(in) :: iak_diff_b, icomp_diff_b
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid
      type(surface_group_data), intent(in) :: sf_grp
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      integer (kind=kint) :: i_sgs_grad_p, i_sgs_grad_fp, i_sgs_simi_p
!
!
      i_sgs_grad_p =  iphys%i_sgs_grad +   3
      i_sgs_grad_fp = iphys%i_sgs_grad_f + 3
      i_sgs_simi_p =  iphys%i_sgs_simi +   3
!
!    reset model coefficients
!
      call reset_diff_model_coefs(ele%numele, ele%istack_ele_smp,       &
     &    diff_coefs%num_field, iak_diff_b, diff_coefs%ak)
      call clear_work_4_dynamic_model(iphys, nod_fld)
!
!    get filtered scalar potential(to iphys%i_sgs_grad_f)
!
      call copy_vector_component(nod_fld,                               &
     &    iphys%i_filter_magne, iphys%i_sgs_grad_f)
      call cal_filtered_scalar_whole(SGS_par%filter_p, nod_comm, node,  &
     &    filtering, i_sgs_grad_fp, iphys%i_mag_p, wk_filter, nod_fld)
!
!   take rotation and gradient of filtered B (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_rotation_whole', iphys%i_sgs_simi, iphys%i_sgs_grad_f
      call choose_cal_rotation                                          &
     &   (FEM_prm%iflag_magne_supg, FEM_prm%npoint_t_evo_int, dt,       &
     &    iphys%i_sgs_grad_f, iphys%i_sgs_simi, ele%istack_ele_smp,     &
     &    m_lump, nod_comm, node, ele, iphys_ele, ele_fld, jac_3d_q,    &
     &    rhs_tbl, fem_wk, f_nl, nod_fld)
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'cal_gradent_whole', i_sgs_simi_p, i_sgs_grad_fp
      call choose_cal_gradient                                          &
     &   (FEM_prm%iflag_magne_supg, FEM_prm%npoint_t_evo_int, dt,       &
     &    i_sgs_grad_fp, i_sgs_simi_p, ele%istack_ele_smp, m_lump,      &
     &    nod_comm, node, ele, iphys_ele, ele_fld, jac_3d_q,            &
     &    rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
!      call choose_cal_divergence                                       &
!     &   (FEM_prm%iflag_magne_supg, FEM_prm%npoint_t_evo_int, dt,      &
!     &    iphys%i_sgs_grad_f, iphys%i_sgs_simi+6, node%istack_nod_smp, &
!     &    m_lump,)
!
!   take rotation and gradient of B (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_rotation_whole',            &
     &                     iphys%i_sgs_grad, iphys%i_magne
      call choose_cal_rotation                                          &
     &   (FEM_prm%iflag_magne_supg, FEM_prm%npoint_t_evo_int, dt,       &
     &    iphys%i_magne, iphys%i_sgs_grad, ele%istack_ele_smp,          &
     &    m_lump, nod_comm, node, ele, iphys_ele, ele_fld, jac_3d_q,    &
     &    rhs_tbl, fem_wk, f_nl, nod_fld)
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'cal_gradent_in_fluid', i_sgs_grad_p, iphys%i_mag_p
      call choose_cal_gradient                                          &
     &   (FEM_prm%iflag_magne_supg, FEM_prm%npoint_t_evo_int, dt,       &
     &    iphys%i_mag_p, i_sgs_grad_p, ele%istack_ele_smp, m_lump,      &
     &    nod_comm, node, ele, iphys_ele, ele_fld, jac_3d_q,            &
     &    rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
!      call choose_cal_divergence                                       &
!     &   (FEM_prm%iflag_magne_supg, FEM_prm%npoint_t_evo_int, dt,      &
!     &    iphys%i_magne, iphys%i_sgs_grad+6, node%istack_nod_smp,      &
!     &    m_lump, )
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_sym_tensor_whole                                &
     &   (SGS_par%filter_p, nod_comm, node, filtering,                  &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, wk_filter, nod_fld)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_tensors(nod_fld,                              &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_sym_tensor, iphys%i_sgs_simi)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad_f)
!
      call cal_rotation_commute                                         &
     &   (FEM_prm%npoint_t_evo_int,ele%istack_ele_smp,                  &
     &    m_lump, node, ele, surf, sf_grp, jac_3d_q, jac_sf_grp_q,      &
     &    rhs_tbl, FEM_elens, Bsf_bcs%sgs, ifilter_4delta,              &
     &    iphys%i_sgs_grad_f, iphys%i_sgs_grad_f,                       &
     &    fem_wk, surf_wk, f_l, f_nl, nod_fld)
      call cal_grad_commute                                             &
     &   (FEM_prm%npoint_t_evo_int, ele%istack_ele_smp,                 &
     &    m_lump, node, ele, surf, sf_grp, jac_3d_q, jac_sf_grp_q,      &
     &    rhs_tbl, FEM_elens, Fsf_bcs%sgs, ifilter_4delta,              &
     &    i_sgs_grad_fp, i_sgs_grad_fp, fem_wk, surf_wk,                &
     &    f_l, f_nl, nod_fld)
!
      call sym_tensor_send_recv                                         &
     &   (iphys%i_sgs_grad_f, nod_comm, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_sym_tensor, iphys%i_sgs_grad_f)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad)
!
      call cal_rotation_commute                                         &
     &   (FEM_prm%npoint_t_evo_int, ele%istack_ele_smp,                 &
     &    m_lump, node, ele, surf, sf_grp, jac_3d_q, jac_sf_grp_q,      &
     &    rhs_tbl, FEM_elens, Bsf_bcs%sgs, ifilter_2delta,              &
     &    iphys%i_sgs_grad, iphys%i_magne,                              &
     &    fem_wk, surf_wk, f_l, f_nl, nod_fld)
      call cal_grad_commute                                             &
     &   (FEM_prm%npoint_t_evo_int, ele%istack_ele_smp,                 &
     &    m_lump, node, ele, surf, sf_grp, jac_3d_q, jac_sf_grp_q,      &
     &    rhs_tbl, FEM_elens, Fsf_bcs%sgs, ifilter_2delta,              &
     &    i_sgs_grad_p, iphys%i_mag_p, fem_wk, surf_wk,                 &
     &    f_l, f_nl, nod_fld)
!
      call sym_tensor_send_recv                                         &
     &   (iphys%i_sgs_grad, nod_comm, nod_fld)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_sym_tensor_whole                                &
     &   (SGS_par%filter_p, nod_comm, node, filtering,                  &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, wk_filter, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_sym_tensor, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_sym_tensor, iak_diff_b, icomp_diff_b
      call cal_diff_coef_fluid(SGS_par, layer_tbl,                      &
     &    node, ele, fluid, iphys, nod_fld, jac_3d_q, jac_3d_l,         &
     &    n_sym_tensor, iak_diff_b, icomp_diff_b,                       &
     &    FEM_prm%npoint_t_evo_int, wk_cor, wk_lsq, wk_diff,            &
     &    diff_coefs)
!
      diff_coefs%iflag_field(iak_diff_b) = 1
!
      end subroutine s_cal_diff_coef_magne
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_magne
