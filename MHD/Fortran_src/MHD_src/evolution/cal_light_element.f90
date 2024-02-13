!>@file   cal_light_element.f90
!!@brief  module cal_light_element
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2005
!
!>@brief  Time integration routine select for scalar
!!
!!@verbatim
!!      subroutine light_element_evolution(time_d, FEM_prm, SGS_par,    &
!!     &         geofem, MHD_mesh, property, ref_param, nod_bcs, sf_bcs,&
!!     &         iref_base, iref_grad, ref_fld, iphys, iphys_LES,       &
!!     &         ak_diffuse, FEM_filters, Smatrix, MGCG_WK, SGS_MHD_wk, &
!!     &         nod_fld, Csims_FEM_MHD, m_SR)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(scalar_property), intent(in) :: property
!!        type(reference_scalar_param), intent(in) :: ref_param
!!        type(nodal_bcs_4_scalar_type), intent(in) :: nod_bcs
!!        type(scaler_surf_bc_type), intent(in) :: sf_bcs
!!        type(base_field_address), intent(in) :: iref_base
!!        type(gradient_field_address), intent(in) :: iref_grad
!!        type(phys_data), intent(in) :: ref_fld
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(MHD_MG_matrix), intent(in) :: Smatrix
!!        type(MGCG_data), intent(inout) :: MGCG_WK
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module cal_light_element
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_phys_constants
!
      use t_time_data
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
      use t_bc_data_MHD
      use t_surface_bc_data_MHD
      use t_base_field_labels
      use t_phys_data
      use t_phys_address
      use t_FEM_MHD_filter_data
      use t_SGS_model_addresses
      use t_material_property
      use t_MHD_matrices_pack
      use t_MGCG_data
      use t_work_FEM_SGS_MHD
      use t_FEM_SGS_model_coefs
      use t_mesh_SR
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine light_element_evolution(time_d, FEM_prm, SGS_par,      &
     &         geofem, MHD_mesh, property, ref_param, nod_bcs, sf_bcs,  &
     &         iref_base, iref_grad, ref_fld, iphys, iphys_LES,         &
     &         ak_diffuse, FEM_filters, Smatrix,                        &
     &         icomp_sgs_flux, iak_diff, icomp_diff_t, i_diff_SGS,      &
     &         iphys_elediff_vec_v, sgs_coefs, sgs_coefs_nod,           &
     &         MGCG_WK, SGS_MHD_wk, nod_fld, diff_coefs, m_SR)
!
      use update_with_scalars
      use cal_add_smp
      use cal_subtract_smp
      use cal_temperature
!
      integer(kind = kint), intent(in) :: icomp_sgs_flux
      integer(kind = kint), intent(in) :: iak_diff
      integer(kind = kint), intent(in) :: i_diff_SGS
      integer(kind = kint), intent(in) :: icomp_diff_t
      integer(kind = kint), intent(in) :: iphys_elediff_vec_v
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(scalar_property), intent(in) :: property
      type(reference_scalar_param), intent(in) :: ref_param
      type(nodal_bcs_4_scalar_type), intent(in) :: nod_bcs
      type(scaler_surf_bc_type), intent(in) :: sf_bcs
!
      type(base_field_address), intent(in) :: iref_base
      type(gradient_field_address), intent(in) :: iref_grad
      type(phys_data), intent(in) :: ref_fld
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(MHD_MG_matrix), intent(in) :: Smatrix
      real(kind = kreal), intent(in)                                    &
     &      :: ak_diffuse(geofem%mesh%ele%numele)
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_scalar, i_pert, iref_scalar
      integer(kind = kint) :: i_velo, i_pre_advect
      integer(kind = kint) :: i_gref
!
      integer(kind = kint) :: i_filter_s, i_filter_v, i_tensor
      integer(kind = kint) :: i_SGS_wk_field
      integer(kind = kint) :: iphys_wfl_scalar
      integer(kind = kint) :: iphys_fefx_buo_gen
!
      real(kind = kreal) :: eps_4_crank
      integer(kind = kint) :: iflag_supg
!
      integer(kind = kint) :: iflag_SGS_flux
      integer(kind = kint) :: itype_Csym_flux
      integer(kind = kint) :: ifilter_final
      integer(kind = kint) :: iflag_commute_flux
      integer(kind = kint) :: iflag_commute_field
!
!     ----- composition update
!
      i_scalar =     iphys%base%i_light
      i_pert =       iphys%base%i_per_light
      i_velo =       iphys%base%i_velo
      i_pre_advect = iphys%exp_work%i_pre_composit
!
      iref_scalar =  iref_base%i_light
      i_gref =       iref_grad%i_grad_composit
!
      i_filter_s =         iphys_LES%filter_fld%i_light
      i_filter_v =         iphys_LES%filter_fld%i_velo
      i_tensor =           iphys_LES%SGS_term%i_SGS_c_flux
      i_SGS_wk_field =     iphys_LES%SGS_wk%i_sgs_composit
      iphys_wfl_scalar =   iphys_LES%wide_filter_fld%i_light
      iphys_fefx_buo_gen = iphys_LES%eflux_by_filter%i_c_buo_gen
!
      eps_4_crank = FEM_prm%eps_4_comp_crank
      iflag_supg =  FEM_prm%iflag_comp_supg
!
      iflag_SGS_flux =  SGS_par%model_p%SGS_light%iflag_SGS_flux
      itype_Csym_flux = SGS_par%model_p%SGS_light%itype_Csym_flux
      ifilter_final =   SGS_par%model_p%ifilter_final
!
      iflag_commute_flux = SGS_par%model_p%SGS_light%iflag_commute_flux
      iflag_commute_field = SGS_par%model_p%SGS_light%iflag_commute_field
!
      call scalar_evolution(i_scalar, i_pert,                           &
     &    iref_scalar, i_velo, i_pre_advect, i_gref,                    &
     &    i_filter_s, i_filter_v, i_tensor, i_SGS_wk_field,             &
     &    iphys_wfl_scalar, iphys_fefx_buo_gen, icomp_sgs_flux,         &
     &    iak_diff, i_diff_SGS, icomp_diff_t, iphys_elediff_vec_v,      &
     &    eps_4_crank, iflag_supg, iflag_SGS_flux,                      &
     &    itype_Csym_flux, ifilter_final,                               &
     &    iflag_commute_flux, iflag_commute_field,                      &
     &    time_d, FEM_prm, SGS_par, geofem, MHD_mesh, property,         &
     &    ref_param, nod_bcs, sf_bcs, ref_fld, iphys_LES,               &
     &    ak_diffuse, FEM_filters, sgs_coefs, sgs_coefs_nod,            &
     &    Smatrix, MGCG_WK, SGS_MHD_wk, nod_fld, diff_coefs, m_SR)
!
      end subroutine light_element_evolution
!
!-----------------------------------------------------------------------
!
      end module cal_light_element
