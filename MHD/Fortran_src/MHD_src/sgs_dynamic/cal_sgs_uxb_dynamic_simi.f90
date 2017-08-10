!cal_sgs_uxb_dynamic_simi.f90
!      module cal_sgs_uxb_dynamic_simi
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!!      subroutine s_cal_sgs_uxb_dynamic_simi                           &
!!     &        (iak_sgs_uxb, icomp_sgs_uxb, FEM_prm, SGS_par, mesh,    &
!!     &         iphys, layer_tbl, jacobians, filtering, wide_filtering,&
!!     &         FEM_SGS_wk, nod_fld, sgs_coefs)
!!      subroutine cal_sgs_induct_t_dynamic_simi                        &
!!     &        (iak_sgs_uxb, icomp_sgs_uxb, FEM_prm, SGS_par,          &
!!     &         mesh, iphys, layer_tbl, jacobians, rhs_tbl,            &
!!     &         filtering, wide_filtering, m_lump, FEM_SGS_wk,         &
!!     &         rhs_mat, nod_fld, sgs_coefs, sgs_coefs_nod)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_address), intent(in) :: iphys
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_type), intent(in) :: jacobians
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_data_type), intent(in) :: wide_filtering
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
!
      module cal_sgs_uxb_dynamic_simi
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_table_FEM_const
      use t_MHD_finite_element_mat
      use t_filtering_data
      use t_work_4_dynamic_model
      use t_work_layer_correlate
      use t_material_property
      use t_SGS_model_coefs
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_sgs_uxb_dynamic_simi                             &
     &        (iak_sgs_uxb, icomp_sgs_uxb, FEM_prm, SGS_par, mesh,      &
     &         iphys, layer_tbl, jacobians, filtering, wide_filtering,  &
     &         FEM_SGS_wk, nod_fld, sgs_coefs)
!
      use reset_dynamic_model_coefs
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_model_diff_coefs
      use cvt_dynamic_scheme_coord
!
      integer(kind = kint), intent(in) :: iak_sgs_uxb, icomp_sgs_uxb
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(phys_address), intent(in) :: iphys
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_type), intent(in) :: jacobians
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(phys_data), intent(inout) :: nod_fld
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (mesh%ele, layer_tbl, icomp_sgs_uxb, sgs_coefs)
      call clear_work_4_dynamic_model(iphys, nod_fld)
!
!   similarity model with wider filter
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_uxb_simi_wide i_wide_fil_velo'
      call cal_sgs_uxb_simi(iphys%i_sgs_grad_f,                         &
     &    iphys%i_filter_velo, iphys%i_filter_magne,                    &
     &    iphys%i_wide_fil_velo, iphys%i_wide_fil_magne,                &
     &    SGS_par%filter_p, mesh%nod_comm, mesh%node, wide_filtering,   &
     &    FEM_SGS_wk%wk_filter, nod_fld)
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys%i_sgs_simi)
!
!    SGS term by similarity model (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_uxb_simi'
      call cal_sgs_uxb_simi(iphys%i_sgs_simi, iphys%i_velo,             &
     &    iphys%i_magne, iphys%i_filter_velo, iphys%i_filter_magne,     &
     &    SGS_par%filter_p, mesh%nod_comm, mesh%node, filtering,        &
     &    FEM_SGS_wk%wk_filter, nod_fld)
!
!      filtering
!
      call cal_filtered_vector_whole                                    &
     &   (SGS_par%filter_p, mesh%nod_comm, mesh%node, filtering,        &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi, FEM_SGS_wk%wk_filter,     &
     &    nod_fld)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord                              &
     &   (SGS_par%model_p, mesh%node, iphys, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     & 'cal_model_coefs', n_vector, iak_sgs_uxb, icomp_sgs_uxb
      call cal_model_coefs(SGS_par, layer_tbl, mesh%node, mesh%ele,     &
     &    iphys, nod_fld, jacobians%jac_3d, jacobians%jac_3d_l,         &
     &    SGS_par%model_p%itype_Csym_uxb, n_vector, iak_sgs_uxb,        &
     &    icomp_sgs_uxb, FEM_prm%npoint_t_evo_int,                      &
     &    FEM_SGS_wk%wk_cor, FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_sgs,      &
     &    sgs_coefs)
!
      end subroutine s_cal_sgs_uxb_dynamic_simi
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_dynamic_simi                          &
     &        (iak_sgs_uxb, icomp_sgs_uxb, FEM_prm, SGS_par,            &
     &         mesh, iphys, layer_tbl, jacobians, rhs_tbl,              &
     &         filtering, wide_filtering, m_lump, FEM_SGS_wk,           &
     &         rhs_mat, nod_fld, sgs_coefs, sgs_coefs_nod)
!
      use reset_dynamic_model_coefs
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_model_diff_coefs
      use copy_nodal_fields
      use int_element_field_2_node
      use cal_similarity_terms
      use cvt_dynamic_scheme_coord
!
      integer(kind = kint), intent(in) :: iak_sgs_uxb, icomp_sgs_uxb
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(phys_address), intent(in) :: iphys
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_type), intent(in) :: jacobians
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
      type(phys_data), intent(inout) :: nod_fld
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (mesh%ele, layer_tbl, icomp_sgs_uxb, sgs_coefs)
      call reset_vector_sgs_nod_m_coefs                                 &
     &   (mesh%node%numnod, mesh%node%istack_nod_smp,                   &
     &    sgs_coefs_nod%ntot_comp, icomp_sgs_uxb, sgs_coefs_nod%ak)
      call clear_work_4_dynamic_model(iphys, nod_fld)
!
!   similarity model with wider filter
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_induct_t_simi_wide i_wide_fil_velo'
      call cal_sgs_induct_t_simi(iphys%i_sgs_grad_f,                    &
     &    iphys%i_filter_velo, iphys%i_filter_magne,                    &
     &    iphys%i_wide_fil_velo, iphys%i_wide_fil_magne, icomp_sgs_uxb, &
     &    SGS_par%filter_p, mesh%nod_comm, mesh%node, wide_filtering,   &
     &    sgs_coefs_nod, FEM_SGS_wk%wk_filter, nod_fld)
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys%i_sgs_grad_f)
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_induct_t_simi'
      call cal_sgs_induct_t_simi(iphys%i_SGS_induct_t,                  &
     &    iphys%i_velo, iphys%i_magne, iphys%i_filter_velo,             &
     &    iphys%i_filter_magne, icomp_sgs_uxb, SGS_par%filter_p,        &
     &    mesh%nod_comm, mesh%node, filtering, sgs_coefs_nod,           &
     &    FEM_SGS_wk%wk_filter, nod_fld)
!
!    copy to work array
!
       call copy_vector_component(nod_fld,                              &
     &     iphys%i_SGS_induct_t, iphys%i_sgs_simi)
!
!      filtering
!
      call cal_filtered_vector_whole                                    &
     &   (SGS_par%filter_p, mesh%nod_comm, mesh%node, filtering,        &
     &    iphys%i_sgs_grad, iphys%i_SGS_induct_t, FEM_SGS_wk%wk_filter, &
     &    nod_fld)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord                              &
     &   (SGS_par%model_p, mesh%node, iphys, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     & 'cal_model_coefs', n_asym_tensor, iak_sgs_uxb, icomp_sgs_uxb
      call cal_model_coefs(SGS_par, layer_tbl, mesh%node, mesh%ele,     &
     &    iphys, nod_fld, jacobians%jac_3d, jacobians%jac_3d_l,         &
     &    SGS_par%model_p%itype_Csym_uxb, n_asym_tensor,                &
     &    iak_sgs_uxb, icomp_sgs_uxb, FEM_prm%npoint_t_evo_int,         &
     &    FEM_SGS_wk%wk_cor, FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_sgs,      &
     &    sgs_coefs)
!
      call cal_ele_vector_2_node                                        &
     &   (mesh%node, mesh%ele, jacobians%jac_3d, rhs_tbl, m_lump,       &
     &    sgs_coefs%ntot_comp, icomp_sgs_uxb, sgs_coefs%ak,             &
     &    sgs_coefs_nod%ntot_comp, icomp_sgs_uxb, sgs_coefs_nod%ak,     &
     &    rhs_mat%fem_wk, rhs_mat%f_l)
!
      end subroutine cal_sgs_induct_t_dynamic_simi
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_uxb_dynamic_simi
