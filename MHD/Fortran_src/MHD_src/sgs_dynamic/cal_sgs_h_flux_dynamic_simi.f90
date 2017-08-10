!cal_sgs_h_flux_dynamic_simi.f90
!      module cal_sgs_h_flux_dynamic_simi
!
!     Written by H. Matsui on May, 2009
!
!!      subroutine s_cal_sgs_s_flux_dynamic_simi                        &
!!     &        (num_int, itype_Csym_flux, ifield, ifield_f, ifield_w,  &
!!     &         ivelo, ivelo_f, i_sgs, iak_sgs_hlux, icomp_sgs_flux,   &
!!     &         SGS_par, mesh, iphys, layer_tbl, jacobians, rhs_tbl,   &
!!     &         filtering, wide_filtering, m_lump,                     &
!!     &         wk_filter, wk_cor, wk_lsq, wk_sgs, fem_wk,             &
!!     &         f_l, nod_fld, sgs_coefs, sgs_coefs_nod)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_address), intent(in) :: iphys
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_type), intent(in) :: jacobians
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_data_type), intent(in) :: wide_filtering
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(dynamic_correlation_data), intent(inout) :: wk_cor
!!        type(dynamis_least_suare_data), intent(inout) :: wk_lsq
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
!
      module cal_sgs_h_flux_dynamic_simi
!
      use m_precision
!
      use m_phys_constants
!
      use m_machine_parameter
!
      use t_SGS_control_parameter
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_table_FEM_const
      use t_layering_ele_list
      use t_filtering_data
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_work_layer_correlate
      use t_material_property
      use t_SGS_model_coefs
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_sgs_s_flux_dynamic_simi                          &
     &        (num_int, itype_Csym_flux, ifield, ifield_f, ifield_w,    &
     &         ivelo, ivelo_f, i_sgs, iak_sgs_hlux, icomp_sgs_flux,     &
     &         SGS_par, mesh, iphys, layer_tbl, jacobians, rhs_tbl,     &
     &         filtering, wide_filtering, m_lump,                       &
     &         wk_filter, wk_cor, wk_lsq, wk_sgs, fem_wk,               &
     &         f_l, nod_fld, sgs_coefs, sgs_coefs_nod)
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_model_diff_coefs
      use int_element_field_2_node
      use cal_similarity_terms
      use cvt_dynamic_scheme_coord
!
      integer(kind = kint), intent(in) :: num_int, itype_Csym_flux
      integer(kind = kint), intent(in) :: iak_sgs_hlux, icomp_sgs_flux
!
      integer (kind=kint), intent(in) :: ifield, ifield_f, ifield_w
      integer (kind=kint), intent(in) :: i_sgs, ivelo, ivelo_f
!
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
      type(filtering_work_type), intent(inout) :: wk_filter
      type(dynamic_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (mesh%ele, layer_tbl, icomp_sgs_flux, sgs_coefs)
      call reset_vector_sgs_nod_m_coefs                                 &
     &   (mesh%node%numnod, mesh%node%istack_nod_smp,                   &
     &    sgs_coefs_nod%ntot_comp, icomp_sgs_flux, sgs_coefs_nod%ak)
      call clear_work_4_dynamic_model(iphys, nod_fld)
!
!   similarity model with wider filter
!
      if (iflag_debug.eq.1)                                             &
     &     write(*,*) 'cal_sgs_sf_simi i_wide_fil_temp'
      call cal_sgs_sf_simi(iphys%i_sgs_grad_f,                          &
     &    ifield_f, ifield_w, ivelo, ivelo_f, icomp_sgs_flux,           &
     &    SGS_par%filter_p, mesh%nod_comm, mesh%node, wide_filtering,   &
     &    sgs_coefs_nod, wk_filter, nod_fld)
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys%i_sgs_grad_f)
!
!    SGS term by similarity model
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sgs_sf_simi'
      call cal_sgs_sf_simi                                              &
     &   (i_sgs, ifield, ifield_f, ivelo, ivelo_f, icomp_sgs_flux,      &
     &    SGS_par%filter_p, mesh%nod_comm, mesh%node,                   &
     &    filtering, sgs_coefs_nod, wk_filter, nod_fld)
!
!    copy to work array
!
      call copy_vector_component(nod_fld, i_sgs, iphys%i_sgs_simi)
!
!      filtering
!
      call cal_filtered_vector_whole                                    &
     &   (SGS_par%filter_p, mesh%nod_comm, mesh%node, filtering,        &
     &    iphys%i_sgs_grad, i_sgs, wk_filter, nod_fld)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord                              &
     &   (SGS_par%model_p, mesh%node, iphys, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.eq.1)  write(*,*)' cal_model_coefs',              &
     &   n_vector, iak_sgs_hlux, icomp_sgs_flux
      call cal_model_coefs(SGS_par, layer_tbl, mesh%node, mesh%ele,     &
     &    iphys, nod_fld, jacobians%jac_3d, jacobians%jac_3d_l,         &
     &    itype_Csym_flux, n_vector, iak_sgs_hlux,                      &
     &    icomp_sgs_flux, num_int, wk_cor, wk_lsq, wk_sgs, sgs_coefs)
!
      call cal_ele_vector_2_node                                        &
     &   (mesh%node, mesh%ele, jacobians%jac_3d, rhs_tbl,               &
     &    m_lump, sgs_coefs%ntot_comp, icomp_sgs_flux, sgs_coefs%ak,    &
     &    sgs_coefs_nod%ntot_comp, icomp_sgs_flux, sgs_coefs_nod%ak,    &
     &    fem_wk, f_l)
!
      end subroutine s_cal_sgs_s_flux_dynamic_simi
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_h_flux_dynamic_simi
