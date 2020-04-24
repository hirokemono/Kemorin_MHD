!
!      module cal_sgs_mom_flux_dynamic
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!!      subroutine cal_sgs_m_flux_dynamic(iak_sgs_term,                 &
!!     &          icomp_sgs_term, iphys_elediff_base, iphys_elediff_fil,&
!!     &          dt, FEM_prm, SGS_par, mesh, iphys, iphys_ele, ele_fld,&
!!     &          fluid, fem_int, FEM_filters, sgs_coefs_nod, mk_MHD,   &
!!     &          FEM_SGS_wk, mhd_fem_wk, rhs_mat, nod_fld, sgs_coefs)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: fld_ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(SGS_term_address), intent(in) :: iak_sgs_term
!!        type(SGS_term_address), intent(in) :: icomp_sgs_term
!!        type(base_field_address), intent(in) :: iphys_elediff_base
!!        type(base_field_address), intent(in) :: iphys_elediff_fil
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
      module cal_sgs_mom_flux_dynamic
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_SGS_term_labels
      use t_jacobians
      use t_table_FEM_const
      use t_FEM_MHD_filter_data
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
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
      subroutine cal_sgs_m_flux_dynamic(iak_sgs_term,                   &
     &          icomp_sgs_term, iphys_elediff_base, iphys_elediff_fil,  &
     &          dt, FEM_prm, SGS_par, mesh, iphys, iphys_ele, ele_fld,  &
     &          fluid, fem_int, FEM_filters, sgs_coefs_nod, mk_MHD,     &
     &          FEM_SGS_wk, mhd_fem_wk, rhs_mat, nod_fld, sgs_coefs)
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_filtering_scalars
      use cal_sgs_mom_fluxes_grad
      use cal_model_diff_coefs
      use cvt_dynamic_scheme_coord
      use reduce_model_coefs
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_term_address), intent(in) :: iak_sgs_term
      type(SGS_term_address), intent(in) :: icomp_sgs_term
      type(base_field_address), intent(in) :: iphys_elediff_base
      type(base_field_address), intent(in) :: iphys_elediff_fil
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
!    reset model coefficients
!
      call reset_tensor_sgs_model_coefs                                 &
     &   (mesh%ele, FEM_filters%layer_tbl,                              &
     &    icomp_sgs_term%i_SGS_m_flux, sgs_coefs)
      call clear_work_4_dynamic_model(iphys%SGS_wk, nod_fld)
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_mf_simi iphys%SGS_term%i_SGS_m_flux'
      call cal_sgs_mf_simi(iphys%SGS_term%i_SGS_m_flux,                 &
     &    iphys%base%i_velo, iphys%filter_fld%i_velo,                   &
     &    icomp_sgs_term%i_SGS_m_flux, SGS_par%filter_p,                &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    sgs_coefs_nod, FEM_SGS_wk%wk_filter, nod_fld)
!
!    copy to work array
!
       call copy_tensor_component(nod_fld,                              &
     &     iphys%SGS_term%i_SGS_m_flux, iphys%SGS_wk%i_simi)
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_sym_tensor, iphys%SGS_wk%i_simi)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_filter_mf_grad_4_dyn'
      call cal_sgs_m_flux_grad_no_coef                                  &
     &   (ifilter_4delta, iphys%SGS_wk%i_wd_nlg,                        &
     &    iphys%filter_fld%i_velo, iphys_elediff_fil%i_velo, dt,        &
     &    FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,           &
     &    iphys_ele%base, ele_fld, fem_int%jcs, FEM_filters%FEM_elens,  &
     &    fem_int%rhs_tbl, mk_MHD%mlump_fl, rhs_mat%fem_wk,             &
     &    mhd_fem_wk, nod_fld)
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_sym_tensor, iphys%SGS_wk%i_wd_nlg)
!
!   gradient model by original field
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_m_flux_grad_4_dyn'
      call cal_sgs_m_flux_grad_no_coef                                  &
     &   (ifilter_2delta, iphys%SGS_term%i_SGS_m_flux,                  &
     &    iphys%base%i_velo, iphys_elediff_base%i_velo, dt,             &
     &    FEM_prm,  mesh%nod_comm, mesh%node, mesh%ele, fluid,          &
     &    iphys_ele%base, ele_fld, fem_int%jcs, FEM_filters%FEM_elens,  &
     &    fem_int%rhs_tbl, mk_MHD%mlump_fl, rhs_mat%fem_wk,             &
     &    mhd_fem_wk, nod_fld)
!
!      filtering
!
      call cal_filtered_sym_tensor_whole(SGS_par%filter_p,              &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    iphys%SGS_wk%i_nlg, iphys%SGS_term%i_SGS_m_flux,              &
     &    FEM_SGS_wk%wk_filter, nod_fld)
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_sym_tensor, iphys%SGS_wk%i_nlg)
!
!   Change coordinate
!
      call cvt_tensor_dynamic_scheme_coord                              &
     &   (SGS_par%model_p, mesh%node, iphys%SGS_wk, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &                     'cal_model_coefs', n_sym_tensor,             &
     &                     iak_sgs_term%i_SGS_m_flux,                   &
     &                     icomp_sgs_term%i_SGS_m_flux
      call cal_model_coefs(SGS_par, FEM_filters%layer_tbl,              &
     &    mesh%node, mesh%ele, iphys%SGS_wk, nod_fld, fem_int%jcs,      &
     &    SGS_par%model_p%itype_Csym_m_flux, n_sym_tensor,              &
     &    iak_sgs_term%i_SGS_m_flux, icomp_sgs_term%i_SGS_m_flux,       &
     &    FEM_prm%npoint_t_evo_int, FEM_SGS_wk%wk_cor,                  &
     &    FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_sgs, sgs_coefs)
!
      call reduce_model_coefs_layer                                     &
     &   (SGS_par%model_p%SGS_mf_factor, iak_sgs_term%i_SGS_m_flux,     &
     &    FEM_SGS_wk%wk_sgs)
      call reduce_ele_tensor_model_coefs                                &
     &   (mesh%ele, SGS_par%model_p%SGS_mf_factor, sgs_coefs%ntot_comp, &
     &    icomp_sgs_term%i_SGS_m_flux, sgs_coefs%ak)
!
      end subroutine cal_sgs_m_flux_dynamic
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_mom_flux_dynamic
