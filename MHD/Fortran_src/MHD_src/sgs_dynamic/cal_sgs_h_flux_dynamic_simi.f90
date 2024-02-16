!cal_sgs_h_flux_dynamic_simi.f90
!      module cal_sgs_h_flux_dynamic_simi
!
!     Written by H. Matsui on May, 2009
!
!!      subroutine s_cal_sgs_s_flux_dynamic_simi                        &
!!     &         (num_int, itype_Csym_flux, ifield, ifield_f, ifield_w, &
!!     &          ivelo, ivelo_f, i_sgs, SGS_par, mesh,                 &
!!     &          iphys_SGS_wk, fem_int, FEM_filters, FEM_SGS_wk,       &
!!     &          rhs_mat, nod_fld, Csim_SGS_flux, sgs_coefs_nod,       &
!!     &          v_sol, SR_sig, SR_r)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_model_coefficient), intent(inout) :: Csim_SGS_flux
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module cal_sgs_h_flux_dynamic_simi
!
      use m_precision
!
      use m_phys_constants
      use m_machine_parameter
!
      use t_SGS_control_parameter
      use t_mesh_data
      use t_phys_data
      use t_SGS_model_coef_labels
      use t_jacobians
      use t_table_FEM_const
      use t_material_property
      use t_SGS_model_coefs
      use t_FEM_MHD_filter_data
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
      use t_vector_for_solver
      use t_solver_SR
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
     &         (num_int, itype_Csym_flux, ifield, ifield_f, ifield_w,   &
     &          ivelo, ivelo_f, i_sgs, SGS_par, mesh,                   &
     &          iphys_SGS_wk, fem_int, FEM_filters, FEM_SGS_wk,         &
     &          rhs_mat, nod_fld, Csim_SGS_flux, sgs_coefs_nod,         &
     &          v_sol, SR_sig, SR_r)
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
!
      integer (kind=kint), intent(in) :: ifield, ifield_f, ifield_w
      integer (kind=kint), intent(in) :: i_sgs, ivelo, ivelo_f
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
      type(SGS_model_coefficient), intent(inout) :: Csim_SGS_flux
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (mesh%ele, FEM_filters%layer_tbl, Csim_sgs_flux)
      call reset_vector_sgs_nod_m_coefs                                 &
     &   (mesh%node%numnod, mesh%node%istack_nod_smp,                   &
     &    sgs_coefs_nod%ak(1,Csim_SGS_flux%icomp_Csim))
      call clear_work_4_dynamic_model(iphys_SGS_wk, nod_fld)
!
!   similarity model with wider filter
!
      if (iflag_debug.eq.1)                                             &
     &     write(*,*) 'cal_sgs_sf_simi wide_filter_fld%i_temp'
      call cal_sgs_sf_simi(iphys_SGS_wk%i_wd_nlg,                       &
     &    ifield_f, ifield_w, ivelo, ivelo_f, Csim_SGS_flux%icomp_Csim, &
     &    SGS_par%filter_p, mesh%nod_comm, mesh%node,                   &
     &    FEM_filters%wide_filtering, sgs_coefs_nod,                    &
     &    FEM_SGS_wk%wk_filter, nod_fld, v_sol, SR_sig, SR_r)
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys_SGS_wk%i_wd_nlg)
!
!    SGS term by similarity model
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sgs_sf_simi'
      call cal_sgs_sf_simi(i_sgs, ifield, ifield_f, ivelo, ivelo_f,     &
     &    Csim_SGS_flux%icomp_Csim, SGS_par%filter_p,                   &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    sgs_coefs_nod, FEM_SGS_wk%wk_filter, nod_fld,                 &
     &    v_sol, SR_sig, SR_r)
!
!    copy to work array
!
      call copy_vector_component(nod_fld, i_sgs, iphys_SGS_wk%i_simi)
!
!      filtering
!
      call cal_filtered_vector_whole(SGS_par%filter_p,                  &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    iphys_SGS_wk%i_nlg, i_sgs, FEM_SGS_wk%wk_filter,              &
     &    nod_fld, v_sol, SR_sig, SR_r)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord                              &
     &   (SGS_par%model_p, mesh%node, iphys_SGS_wk, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.eq.1)  write(*,*)' cal_model_coefs',              &
     &   n_vector, Csim_SGS_flux%iak_Csim, Csim_SGS_flux%icomp_Csim
      call cal_model_coefs(SGS_par, FEM_filters%layer_tbl,              &
     &    mesh%node, mesh%ele, iphys_SGS_wk, nod_fld, fem_int%jcs,      &
     &    itype_Csym_flux, num_int, FEM_SGS_wk%wk_cor,                  &
     &    FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_sgs, Csim_SGS_flux)
!
      call cal_ele_vector_2_node(mesh%node, mesh%ele,                   &
     &    fem_int%jcs, fem_int%rhs_tbl, fem_int%m_lump,                 &
     &    Csim_SGS_flux%num_comp, ione, Csim_SGS_flux%coef(1,1),        &
     &    sgs_coefs_nod%ntot_comp, Csim_SGS_flux%icomp_Csim,            &
     &    sgs_coefs_nod%ak, rhs_mat%fem_wk, rhs_mat%f_l)
!
      end subroutine s_cal_sgs_s_flux_dynamic_simi
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_h_flux_dynamic_simi
