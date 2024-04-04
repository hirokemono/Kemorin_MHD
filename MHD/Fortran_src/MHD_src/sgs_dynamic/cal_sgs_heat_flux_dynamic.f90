!cal_sgs_heat_flux_dynamic.f90
!      module cal_sgs_heat_flux_dynamic
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!!      subroutine cal_sgs_sf_dynamic(iflag_supg, num_int, dt,          &
!!     &         itype_Csym_flux, SGS_flux_factor,                      &
!!     &         ifield, ifield_f, ivelo, ivelo_f, i_sgs, SGS_par, mesh,&
!!     &         iphys_SGS_wk, iphys_ele_base, ele_fld, fluid, fem_int, &
!!     &         FEM_filters,  mk_MHD, FEM_SGS_wk, mhd_fem_wk,          &
!!     &         rhs_mat, nod_fld, Csim_SGS_flux, v_sol, SR_sig, SR_r)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_model_coefficient), intent(inout) :: Csim_SGS_flux
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module cal_sgs_heat_flux_dynamic
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_SGS_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
      use t_phys_data
      use t_base_field_labels
      use t_SGS_model_coef_labels
      use t_table_FEM_const
      use t_jacobians
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_FEM_MHD_filter_data
      use t_material_property
      use t_FEM_SGS_model_coefs
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
      subroutine cal_sgs_sf_dynamic(iflag_supg, num_int, dt,            &
     &         itype_Csym_flux, SGS_flux_factor,                        &
     &         ifield, ifield_f, ivelo, ivelo_f, i_sgs, SGS_par, mesh,  &
     &         iphys_SGS_wk, iphys_ele_base, ele_fld, fluid, fem_int,   &
     &         FEM_filters,  mk_MHD, FEM_SGS_wk, mhd_fem_wk,            &
     &         rhs_mat, nod_fld, Csim_SGS_flux, v_sol, SR_sig, SR_r)
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_sgs_heat_fluxes_grad
      use cal_model_diff_coefs
      use cvt_dynamic_scheme_coord
      use reduce_model_coefs
      use overwrite_prod_const_smp
!
      integer(kind = kint), intent(in) :: iflag_supg, num_int
      integer(kind = kint), intent(in) :: itype_Csym_flux
      real(kind = kreal), intent(in) :: SGS_flux_factor
      real(kind = kreal), intent(in) :: dt
!
      integer (kind=kint), intent(in) :: i_sgs, ifield, ifield_f
      integer (kind=kint), intent(in) :: ivelo, ivelo_f
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_model_coefficient), intent(inout) :: Csim_SGS_flux
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (mesh%ele, FEM_filters%layer_tbl, Csim_SGS_flux)
      call clear_work_4_dynamic_model(iphys_SGS_wk, nod_fld)
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_sf_simi'
      call cal_sgs_sf_simi                                              &
     &   (i_sgs, ifield, ifield_f, ivelo, ivelo_f, SGS_par%filter_p,    &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    Csim_SGS_flux, FEM_SGS_wk%wk_filter, nod_fld,                 &
     &    v_sol, SR_sig, SR_r)
!
!    copy to work array
!
      call copy_vector_component(nod_fld, i_sgs, iphys_SGS_wk%i_simi)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_hf_grad_4_dyn'
      call cal_sgs_s_flux_grad_no_coef                                  &
     &   (iflag_supg, num_int, dt, ifilter_4delta,                      &
     &    iphys_SGS_wk%i_wd_nlg, ifield_f, mhd_fem_wk%ifil_elediff_v,   &
     &    mesh%nod_comm, mesh%node, mesh%ele, fluid, iphys_ele_base,    &
     &    ele_fld, fem_int%jcs, fem_int%rhs_tbl, FEM_filters%FEM_elens, &
     &    mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_l,     &
     &    nod_fld, v_sol, SR_sig, SR_r)
!
!   gradient model by original field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_h_flux_grad_4_dyn'
      call cal_sgs_s_flux_grad_no_coef(iflag_supg, num_int, dt,         &
     &    ifilter_2delta, i_sgs, ifield, mhd_fem_wk%iphys_elediff_v,    &
     &    mesh%nod_comm, mesh%node, mesh%ele, fluid, iphys_ele_base,    &
     &    ele_fld, fem_int%jcs, fem_int%rhs_tbl, FEM_filters%FEM_elens, &
     &    mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_l,     &
     &    nod_fld, v_sol, SR_sig, SR_r)
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
      if (iflag_debug.gt.0)  write(*,*) 'cal_model_coefs', n_vector,    &
     &              Csim_SGS_flux%iak_Csim, Csim_SGS_flux%icomp_Csim
      call cal_model_coefs(SGS_par, FEM_filters%layer_tbl,              &
     &    mesh%node, mesh%ele, iphys_SGS_wk, nod_fld, fem_int%jcs,      &
     &    itype_Csym_flux, num_int, FEM_SGS_wk%wk_cor,                  &
     &    FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_sgs, Csim_SGS_flux)
!
      call reduce_model_coefs_layer                                     &
     &   (SGS_flux_factor, Csim_SGS_flux%iak_Csim, FEM_SGS_wk%wk_sgs)
!
!$omp parallel
      call ovwrt_coef_prod_vect_smp                                     &
     &   (np_smp, mesh%ele%numele, mesh%ele%istack_ele_smp,             &
     &    SGS_flux_factor, Csim_SGS_flux%coef(1,1))
!$omp end parallel
!
      end subroutine cal_sgs_sf_dynamic
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_heat_flux_dynamic
