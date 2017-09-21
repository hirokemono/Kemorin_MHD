!cal_sgs_heat_flux_dynamic.f90
!      module cal_sgs_heat_flux_dynamic
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!!      subroutine cal_sgs_sf_dynamic(iflag_supg, num_int, dt,          &
!!     &          itype_Csym_flux, SGS_flux_factor,                     &
!!     &          ifield, ifield_f, ivelo, ivelo_f, i_sgs,              &
!!     &          iak_sgs_hlux, icomp_sgs_flux, ie_dvx, ie_dfvx,        &
!!     &          SGS_par, mesh, iphys, iphys_ele, ele_fld, fluid,      &
!!     &          fem_int, FEM_filters, sgs_coefs_nod, mk_MHD,          &
!!     &          FEM_SGS_wk, mhd_fem_wk, rhs_mat, nod_fld, sgs_coefs)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
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
      use t_phys_address
      use t_table_FEM_const
      use t_jacobians
      use t_MHD_finite_element_mat
      use t_MHD_mass_matricxes
      use t_FEM_MHD_filter_data
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
      subroutine cal_sgs_sf_dynamic(iflag_supg, num_int, dt,            &
     &          itype_Csym_flux, SGS_flux_factor,                       &
     &          ifield, ifield_f, ivelo, ivelo_f, i_sgs,                &
     &          iak_sgs_hlux, icomp_sgs_flux, ie_dvx, ie_dfvx,          &
     &          SGS_par, mesh, iphys, iphys_ele, ele_fld, fluid,        &
     &          fem_int, FEM_filters, sgs_coefs_nod, mk_MHD,            &
     &          FEM_SGS_wk, mhd_fem_wk, rhs_mat, nod_fld, sgs_coefs)
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_sgs_heat_fluxes_grad
      use cal_model_diff_coefs
      use cvt_dynamic_scheme_coord
      use reduce_model_coefs
!
      integer(kind = kint), intent(in) :: iflag_supg, num_int
      integer(kind = kint), intent(in) :: itype_Csym_flux
      real(kind = kreal), intent(in) :: SGS_flux_factor
      real(kind = kreal), intent(in) :: dt
!
      integer (kind=kint), intent(in) :: i_sgs, ifield, ifield_f
      integer (kind=kint), intent(in) :: ivelo, ivelo_f
!
      integer(kind = kint), intent(in) :: iak_sgs_hlux, icomp_sgs_flux
      integer(kind = kint), intent(in) :: ie_dvx, ie_dfvx
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
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
      call reset_vector_sgs_model_coefs                                 &
     &   (mesh%ele, FEM_filters%layer_tbl, icomp_sgs_flux, sgs_coefs)
      call clear_work_4_dynamic_model(iphys, nod_fld)
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_sf_simi'
      call cal_sgs_sf_simi                                              &
     &   (i_sgs, ifield, ifield_f, ivelo, ivelo_f, icomp_sgs_flux,      &
     &    SGS_par%filter_p, mesh%nod_comm, mesh%node,                   &
     &    FEM_filters%filtering, sgs_coefs_nod, FEM_SGS_wk%wk_filter,   &
     &    nod_fld)
!
!    copy to work array
!
      call copy_vector_component(nod_fld, i_sgs, iphys%i_sgs_simi)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_hf_grad_4_dyn'
      call cal_sgs_s_flux_grad_no_coef                                  &
     &   (iflag_supg, num_int, dt, ifilter_4delta,                      &
     &    iphys%i_sgs_grad_f, ifield_f, ie_dfvx,                        &
     &    mesh%nod_comm, mesh%node, mesh%ele, fluid, iphys_ele,         &
     &    ele_fld, fem_int%jcs, fem_int%rhs_tbl, FEM_filters%FEM_elens, &
     &    mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_l,     &
     &    nod_fld)
!
!   gradient model by original field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_h_flux_grad_4_dyn'
      call cal_sgs_s_flux_grad_no_coef(iflag_supg, num_int, dt,         &
     &    ifilter_2delta, i_sgs, ifield, ie_dvx,                        &
     &    mesh%nod_comm, mesh%node, mesh%ele, fluid, iphys_ele,         &
     &    ele_fld, fem_int%jcs, fem_int%rhs_tbl, FEM_filters%FEM_elens, &
     &    mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_l,     &
     &    nod_fld)
!
!      filtering
!
      call cal_filtered_vector_whole(SGS_par%filter_p,                  &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    iphys%i_sgs_grad, i_sgs, FEM_SGS_wk%wk_filter, nod_fld)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord                              &
     &   (SGS_par%model_p, mesh%node, iphys, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_model_coefs', n_vector, iak_sgs_hlux, icomp_sgs_flux
      call cal_model_coefs(SGS_par, FEM_filters%layer_tbl,              &
     &    mesh%node, mesh%ele, iphys, nod_fld, fem_int%jcs,             &
     &    itype_Csym_flux, n_vector, iak_sgs_hlux, icomp_sgs_flux,      &
     &    num_int, FEM_SGS_wk%wk_cor, FEM_SGS_wk%wk_lsq,                &
     &    FEM_SGS_wk%wk_sgs, sgs_coefs)
!
      call reduce_model_coefs_layer                                     &
     &   (SGS_flux_factor, iak_sgs_hlux, FEM_SGS_wk%wk_sgs)
      call reduce_ele_vect_model_coefs(mesh%ele, SGS_flux_factor,       &
     &    sgs_coefs%ntot_comp, icomp_sgs_flux, sgs_coefs%ak)
!
      end subroutine cal_sgs_sf_dynamic
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_heat_flux_dynamic
