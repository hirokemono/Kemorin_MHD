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
!!     &         ak_MHD, FEM_filters, Smatrix, MGCG_WK, SGS_MHD_wk,     &
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
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(MHD_MG_matrix), intent(in) :: Smatrix
!!        type(MGCG_data), intent(inout) :: MGCG_WK
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
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
     &         ak_MHD, FEM_filters, Smatrix, MGCG_WK, SGS_MHD_wk,       &
     &         nod_fld, Csims_FEM_MHD, m_SR)
!
      use update_with_scalars
      use cal_add_smp
      use cal_subtract_smp
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
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(MHD_MG_matrix), intent(in) :: Smatrix
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_scalar, i_pert, iref_scalar
      integer(kind = kint) :: i_velo, i_pre_advect
      integer(kind = kint) :: i_gref
!
      integer(kind = kint) :: i_filter_s, i_tensor
      integer(kind = kint) :: i_SGS_wk_field
      integer(kind = kint) :: iphys_wfl_scalar
      integer(kind = kint) :: iphys_fefx_buo_gen
!
      integer(kind = kint) :: icomp_sgs_flux
      integer(kind = kint) :: iak_diff, i_diff_SGS, icomp_diff_t
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
      i_tensor =           iphys_LES%SGS_term%i_SGS_c_flux
      i_SGS_wk_field =     iphys_LES%SGS_wk%i_sgs_composit
      iphys_wfl_scalar =   iphys_LES%wide_filter_fld%i_light
      iphys_fefx_buo_gen = iphys_LES%eflux_by_filter%i_c_buo_gen
!
      icomp_sgs_flux = Csims_FEM_MHD%icomp_sgs_term%i_SGS_c_flux
      iak_diff =       Csims_FEM_MHD%iak_diff_base%i_light
      i_diff_SGS =     Csims_FEM_MHD%iak_diff_SGS%i_SGS_c_flux
      icomp_diff_t =  Csims_FEM_MHD%icomp_diff_base%i_light
!
      if(property%iflag_scheme .gt. id_no_evolution) then
        if(ref_param%iflag_reference .ne. id_no_ref_temp) then
          if(iflag_debug.eq.1) write(*,*) 's_cal_light_element part'
          call s_cal_light_element(i_pert, i_scalar, i_velo,            &
     &        i_pre_advect, i_gref, i_filter_s, i_tensor,               &
     &        time_d%dt, FEM_prm, &
     &        SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,     &
     &        geofem%mesh, geofem%group, MHD_mesh%fluid,                &
     &        property, ref_param, nod_bcs, sf_bcs, ref_fld,            &
     &        SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,            &
     &        SGS_MHD_wk%fem_int, FEM_filters%FEM_elens,                &
     &        icomp_sgs_flux, iak_diff, i_diff_SGS,                     &
     &        Csims_FEM_MHD%iphys_elediff_vec, Csims_FEM_MHD%sgs_coefs, &
     &        Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,    &
     &        FEM_filters%filtering, SGS_MHD_wk%mk_MHD,                 &
     &        Smatrix, ak_MHD, MGCG_WK,                                 &
     &        SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,             &
     &        SGS_MHD_wk%rhs_mat, nod_fld, m_SR)
!
!$omp parallel
          call add_scalars_smp(nod_fld%n_point,                         &
     &                         ref_fld%d_fld(1,iref_scalar),            &
     &                         nod_fld%d_fld(1,i_pert),                 &
     &                         nod_fld%d_fld(1,i_scalar))
!$omp end parallel
        else
          if(iflag_debug.eq.1) write(*,*) 's_cal_light_element C'
          call s_cal_light_element(i_scalar, i_scalar, i_velo,          &
     &        i_pre_advect, i_gref, i_filter_s, i_tensor,               &
     &        time_d%dt, FEM_prm, &
     &        SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,     &
     &        geofem%mesh, geofem%group, MHD_mesh%fluid,                &
     &        property, ref_param, nod_bcs, sf_bcs, ref_fld,            &
     &        SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,            &
     &        SGS_MHD_wk%fem_int, FEM_filters%FEM_elens,                &
     &        icomp_sgs_flux, iak_diff, i_diff_SGS,                     &
     &        Csims_FEM_MHD%iphys_elediff_vec, Csims_FEM_MHD%sgs_coefs, &
     &        Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,    &
     &        FEM_filters%filtering, SGS_MHD_wk%mk_MHD,                 &
     &        Smatrix, ak_MHD, MGCG_WK,                                 &
     &        SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,             &
     &        SGS_MHD_wk%rhs_mat, nod_fld, m_SR)
!
          if(i_pert .gt. 0) then
!$omp parallel
            call subtract_scalars_smp(nod_fld%n_point,                  &
     &                                nod_fld%d_fld(1,i_scalar),        &
     &                                ref_fld%d_fld(1,iref_scalar),     &
     &                                nod_fld%d_fld(1,i_pert))
!$omp end parallel
          end if
        end if
!
        call update_with_dummy_scalar(time_d%i_time_step, time_d%dt,    &
     &     i_scalar, i_pert, i_filter_s, i_SGS_wk_field,                &
     &     iphys_wfl_scalar, iphys_fefx_buo_gen, FEM_prm,               &
     &     SGS_par%iflag_SGS_initial, SGS_par%i_step_sgs_coefs,         &
     &     SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,        &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, sf_bcs,           &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int, FEM_filters,         &
     &     iak_diff, icomp_diff_t,                                      &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%rhs_mat, nod_fld, Csims_FEM_MHD%diff_coefs,       &
     &     m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
      end if
!
      end subroutine light_element_evolution
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_light_element(i_field, i_scalar, i_velo,         &
     &         i_pre_advect, i_gref, i_filter_s, i_tensor, dt,          &
     &         FEM_prm, SGS_param, cmt_param, filter_param, mesh, group, &
     &         fluid, property, ref_param, nod_bcs, sf_bcs,             &
     &         ref_fld, iphys_ele_base, ele_fld, fem_int, FEM_elens,    &
     &         icomp_sgs_flux, iak_diff, i_diff_SGS,                    &
     &         iphys_elediff_vec, sgs_coefs, sgs_coefs_nod,             &
     &         diff_coefs, filtering, mk_MHD, Smatrix, ak_MHD,          &
     &         MGCG_WK, FEM_SGS_wk, mhd_fem_wk, rhs_mat,                &
     &         nod_fld, m_SR)
!
      use cal_temperature
!
      integer(kind = kint), intent(in) :: i_field, i_scalar
      integer(kind = kint), intent(in) :: i_velo, i_pre_advect
      integer(kind = kint), intent(in) :: i_gref
      integer(kind = kint), intent(in) :: i_filter_s, i_tensor
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: icomp_sgs_flux
      integer(kind = kint), intent(in) :: iak_diff, i_diff_SGS
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(reference_scalar_param), intent(in) :: ref_param
      type(nodal_bcs_4_scalar_type), intent(in) :: nod_bcs
      type(scaler_surf_bc_type), intent(in) :: sf_bcs
!
      type(phys_data), intent(in) :: ref_fld
!
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(filtering_data_type), intent(in) :: filtering
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(base_field_address), intent(in) :: iphys_elediff_vec
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
      type(MHD_MG_matrix), intent(in) :: Smatrix
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(mesh_SR), intent(inout) :: m_SR
!
!
      integer(kind = kint) :: iflag_supg
      integer(kind = kint) :: iflag_SGS_flux
      integer(kind = kint) :: itype_Csym_flux
      integer(kind = kint) :: ifilter_final
      integer(kind = kint) :: iflag_commute_flux
      integer(kind = kint) :: iflag_commute_field
!
      integer(kind = kint) :: i_filter_v
!
      real(kind = kreal) :: eps_4_crank
!
!
      itype_Csym_flux = SGS_param%itype_Csym_c_flux
!
      iflag_supg =  FEM_prm%iflag_comp_supg
      eps_4_crank = FEM_prm%eps_4_comp_crank
      iflag_SGS_flux = SGS_param%iflag_SGS_c_flux
      ifilter_final =  SGS_param%ifilter_final
      iflag_commute_flux = cmt_param%iflag_c_cf
      iflag_commute_field = cmt_param%iflag_c_light
      call cal_scalar_field_pre(i_field, dt,                            &
     &          iflag_supg, iflag_SGS_flux, itype_Csym_flux,            &
     &          ifilter_final, iflag_commute_flux, iflag_commute_field, &
     &          i_scalar, i_velo, i_gref, i_tensor,                     &
     &          i_filter_s, i_filter_v, i_pre_advect,                   &
     &          iak_diff, i_diff_SGS, icomp_sgs_flux, eps_4_crank,      &
     &          FEM_prm, SGS_param, filter_param, mesh, group,          &
     &          fluid, property, ref_param, nod_bcs, sf_bcs,            &
     &          ref_fld, iphys_ele_base, ele_fld, fem_int,              &
     &          FEM_elens, iphys_elediff_vec, sgs_coefs, sgs_coefs_nod, &
     &          diff_coefs, filtering, mk_MHD%mlump_fl, Smatrix,        &
     &          ak_MHD%ak_d_composit, MGCG_WK, FEM_SGS_wk%wk_filter,    &
     &          mhd_fem_wk, rhs_mat, nod_fld, m_SR)
!
      end subroutine s_cal_light_element
!
! ----------------------------------------------------------------------
!
      end module cal_light_element
