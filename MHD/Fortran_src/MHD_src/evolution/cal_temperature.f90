!
!      module cal_temperature
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!
!!      subroutine temperature_evolution(time_d, FEM_prm, SGS_par,      &
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
!
      module cal_temperature
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
      subroutine temperature_evolution(time_d, FEM_prm, SGS_par,        &
     &         geofem, MHD_mesh, property, ref_param, nod_bcs, sf_bcs,  &
     &         iref_base, iref_grad, ref_fld, iphys, iphys_LES,         &
     &         ak_diffuse, FEM_filters, Smatrix,                        &
     &         icomp_sgs_flux, icomp_diff_t, i_diff_SGS,                &
     &         iphys_elediff_vec_v, sgs_coefs, sgs_coefs_nod,           &
     &         MGCG_WK, SGS_MHD_wk, nod_fld, diff_coefs, m_SR)
!
      use update_with_scalars
      use cal_add_smp
      use cal_subtract_smp
!
      integer(kind = kint), intent(in) :: icomp_sgs_flux
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
!     ---- temperature update
!
      i_scalar =     iphys%base%i_temp
      i_pert =       iphys%base%i_per_temp
      i_velo =       iphys%base%i_velo
      i_pre_advect = iphys%exp_work%i_pre_heat
      iref_scalar =  iref_base%i_temp
      i_gref =       iref_grad%i_grad_temp
!
      i_filter_s =         iphys_LES%filter_fld%i_temp
      i_filter_v =         iphys_LES%filter_fld%i_velo
      i_tensor =           iphys_LES%SGS_term%i_SGS_h_flux
      i_SGS_wk_field =     iphys_LES%SGS_wk%i_sgs_temp
      iphys_wfl_scalar =   iphys_LES%wide_filter_fld%i_temp
      iphys_fefx_buo_gen = iphys_LES%eflux_by_filter%i_buo_gen
!
      eps_4_crank = FEM_prm%eps_4_temp_crank
      iflag_supg = FEM_prm%iflag_temp_supg
!
      iflag_SGS_flux =  SGS_par%model_p%SGS_heat%iflag_SGS_flux
      itype_Csym_flux = SGS_par%model_p%SGS_heat%itype_Csym_flux
      ifilter_final =   SGS_par%model_p%ifilter_final
!
      iflag_commute_flux = SGS_par%model_p%SGS_heat%iflag_commute_flux
      iflag_commute_field = SGS_par%model_p%SGS_heat%iflag_commute_field
!
      call scalar_evolution(i_scalar, i_pert,                           &
     &    iref_scalar, i_velo, i_pre_advect, i_gref,                    &
     &    i_filter_s, i_filter_v, i_tensor, i_SGS_wk_field,             &
     &    iphys_wfl_scalar, iphys_fefx_buo_gen, icomp_sgs_flux,         &
     &    i_diff_SGS, icomp_diff_t, iphys_elediff_vec_v,                &
     &    eps_4_crank, iflag_supg, iflag_SGS_flux,                      &
     &    itype_Csym_flux, ifilter_final,                               &
     &    iflag_commute_flux, iflag_commute_field,                      &
     &    time_d, FEM_prm, SGS_par, geofem, MHD_mesh, property,         &
     &    ref_param, nod_bcs, sf_bcs, ref_fld, iphys_LES,               &
     &    ak_diffuse, FEM_filters, sgs_coefs, sgs_coefs_nod,            &
     &    Smatrix, MGCG_WK, SGS_MHD_wk, nod_fld,                        &
     &    diff_coefs%Cdiff_temp, diff_coefs, m_SR)
!
      end subroutine temperature_evolution
!
!-----------------------------------------------------------------------
!
      subroutine scalar_evolution(i_scalar, i_pert,                     &
     &         iref_scalar, i_velo, i_pre_advect, i_gref,               &
     &         i_filter_s, i_filter_v, i_tensor, i_SGS_wk_field,        &
     &         iphys_wfl_scalar, iphys_fefx_buo_gen, icomp_sgs_flux,    &
     &         i_diff_SGS, icomp_diff_t, iphys_elediff_vec_v,           &
     &         eps_4_crank, iflag_supg, iflag_SGS_flux,                 &
     &         itype_Csym_flux, ifilter_final,                          &
     &         iflag_commute_flux, iflag_commute_field, time_d,         &
     &         FEM_prm, SGS_par, geofem, MHD_mesh, property,            &
     &         ref_param, nod_bcs, sf_bcs, ref_fld, iphys_LES,          &
     &         ak_diffuse, FEM_filters, sgs_coefs, sgs_coefs_nod,       &
     &         Smatrix, MGCG_WK, SGS_MHD_wk, nod_fld,                   &
     &         Cdiff_scalar, diff_coefs, m_SR)
!
      use update_with_scalars
      use cal_add_smp
      use cal_subtract_smp
!
      integer(kind = kint), intent(in) :: i_scalar, i_pert
      integer(kind = kint), intent(in) :: i_velo, i_pre_advect
      integer(kind = kint), intent(in) :: iref_scalar, i_gref
!
      integer(kind = kint), intent(in) :: i_filter_s, i_filter_v
      integer(kind = kint), intent(in) :: i_tensor
      integer(kind = kint), intent(in) :: i_SGS_wk_field
      integer(kind = kint), intent(in) :: iphys_wfl_scalar
      integer(kind = kint), intent(in) :: iphys_fefx_buo_gen
!
      integer(kind = kint), intent(in) :: icomp_sgs_flux
      integer(kind = kint), intent(in) :: i_diff_SGS
      integer(kind = kint), intent(in) :: icomp_diff_t
      integer(kind = kint), intent(in) :: iphys_elediff_vec_v
!
      real(kind = kreal), intent(in) :: eps_4_crank
      integer(kind = kint), intent(in) :: iflag_supg
!
      integer(kind = kint), intent(in) :: iflag_SGS_flux
      integer(kind = kint), intent(in) :: itype_Csym_flux
      integer(kind = kint), intent(in) :: ifilter_final
      integer(kind = kint), intent(in) :: iflag_commute_flux
      integer(kind = kint), intent(in) :: iflag_commute_field
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
      type(phys_data), intent(in) :: ref_fld
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
      type(SGS_model_coefficient), intent(inout) :: Cdiff_scalar
      type(mesh_SR), intent(inout) :: m_SR
!
!     ---- temperature update
!
      if(property%iflag_scheme .gt. id_no_evolution) then
        if(ref_param%iflag_reference .ne. id_no_ref_temp) then
          if(iflag_debug.eq.1) write(*,*) 'cal_scalar_field_pre pert'
          call cal_scalar_field_pre(i_pert, i_scalar, i_velo,           &
     &       i_pre_advect, i_gref, i_filter_s, i_filter_v, i_tensor,    &
     &       time_d%dt, eps_4_crank, iflag_supg,                        &
     &       iflag_SGS_flux, itype_Csym_flux, ifilter_final,            &
     &       iflag_commute_flux, iflag_commute_field,                   &
     &       FEM_prm, SGS_par%model_p, SGS_par%filter_p,                &
     &       geofem%mesh, geofem%group, MHD_mesh%fluid,                 &
     &       property, ref_param, nod_bcs, sf_bcs, ref_fld,             &
     &       SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,             &
     &       SGS_MHD_wk%fem_int, FEM_filters%FEM_elens,                 &
     &       icomp_sgs_flux, i_diff_SGS, iphys_elediff_vec_v,           &
     &       sgs_coefs, sgs_coefs_nod, Cdiff_scalar, diff_coefs,        &
     &       FEM_filters%filtering, SGS_MHD_wk%mk_MHD,                  &
     &       Smatrix, ak_diffuse, MGCG_WK,                              &
     &       SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,              &
     &       SGS_MHD_wk%rhs_mat, nod_fld, m_SR)
!
!$omp parallel
          call add_scalars_smp(nod_fld%n_point,                         &
     &                         ref_fld%d_fld(1,iref_scalar),            &
     &                         nod_fld%d_fld(1,i_pert),                 &
     &                         nod_fld%d_fld(1,i_scalar))
!$omp end parallel
        else
!          call check_surface_param_smp('cal_scalar_field_pre start',   &
!     &        my_rank, sf_grp, geofem%group%surf_nod_grp)
          if(iflag_debug.eq.1) write(*,*) 'cal_scalar_field_pre'
          call cal_scalar_field_pre(i_scalar, i_scalar, i_velo,         &
     &       i_pre_advect, i_gref, i_filter_s, i_filter_v, i_tensor,    &
     &       time_d%dt, eps_4_crank, iflag_supg,                        &
     &       iflag_SGS_flux, itype_Csym_flux, ifilter_final,            &
     &       iflag_commute_flux, iflag_commute_field,                   &
     &       FEM_prm, SGS_par%model_p, SGS_par%filter_p,                &
     &       geofem%mesh, geofem%group, MHD_mesh%fluid,                 &
     &       property, ref_param, nod_bcs, sf_bcs, ref_fld,             &
     &       SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,             &
     &       SGS_MHD_wk%fem_int, FEM_filters%FEM_elens,                 &
     &       icomp_sgs_flux, i_diff_SGS, iphys_elediff_vec_v,           &
     &       sgs_coefs, sgs_coefs_nod, Cdiff_scalar, diff_coefs,        &
     &       FEM_filters%filtering, SGS_MHD_wk%mk_MHD,                  &
     &       Smatrix, ak_diffuse, MGCG_WK,                              &
     &       SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,              &
     &       SGS_MHD_wk%rhs_mat, nod_fld, m_SR)
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
        call update_with_scalar(time_d%i_time_step, time_d%dt,          &
     &     i_scalar, i_pert, i_filter_s, i_SGS_wk_field,                &
     &     iphys_wfl_scalar, iphys_fefx_buo_gen,                        &
     &     iflag_supg, FEM_prm%npoint_t_evo_int,                        &
     &     iflag_SGS_flux, iflag_commute_field,                         &
     &     SGS_par%iflag_SGS_initial, SGS_par%i_step_sgs_coefs,         &
     &     SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,        &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, sf_bcs,           &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int, FEM_filters,         &
     &     icomp_diff_t, SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,      &
     &     SGS_MHD_wk%rhs_mat, nod_fld, Cdiff_scalar,                   &
     &     m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
      end if
!
      end subroutine scalar_evolution
!
!-----------------------------------------------------------------------
!
      subroutine cal_scalar_field_pre(i_field, i_scalar, i_velo,        &
     &         i_pre_advect, i_gref, i_filter_s, i_filter_v, i_tensor,  &
     &         dt, eps_4_crank, iflag_supg,                             &
     &         iflag_SGS_flux, itype_Csym_flux, ifilter_final,          &
     &         iflag_commute_flux, iflag_commute_field,                 &
     &         FEM_prm, SGS_param, filter_param, mesh, group,           &
     &         fluid, property, ref_param, nod_bcs, sf_bcs,             &
     &         ref_fld, iphys_ele_base, ele_fld, fem_int, FEM_elens,    &
     &         icomp_sgs_flux, i_diff_SGS, iphys_elediff_vec_v,         &
     &         sgs_coefs, sgs_coefs_nod, Cdiff_scalar, diff_coefs,      &
     &         filtering, mk_MHD, Smatrix, ak_diffuse, MGCG_WK,         &
     &         FEM_SGS_wk, mhd_fem_wk, rhs_mat, nod_fld, m_SR)
!
      use nod_phys_send_recv
      use cal_sgs_fluxes
      use set_boundary_scalars
      use sel_int_scalar_ele
      use sel_cal_scalar_pre
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: iflag_supg
      integer(kind = kint), intent(in) :: iflag_SGS_flux
      integer(kind = kint), intent(in) :: itype_Csym_flux
      integer(kind = kint), intent(in) :: ifilter_final
      integer(kind = kint), intent(in) :: iflag_commute_flux
      integer(kind = kint), intent(in) :: iflag_commute_field
!
      integer(kind = kint), intent(in) :: i_scalar
      integer(kind = kint), intent(in) :: i_velo
      integer(kind = kint), intent(in) :: i_gref
      integer(kind = kint), intent(in) :: i_tensor
      integer(kind = kint), intent(in) :: i_filter_s
      integer(kind = kint), intent(in) :: i_filter_v
      integer(kind = kint), intent(in) :: i_pre_advect
      integer(kind = kint), intent(in) :: i_diff_SGS
      integer(kind = kint), intent(in) :: icomp_sgs_flux
      integer(kind = kint), intent(in) :: iphys_elediff_vec_v
!
      real(kind = kreal), intent(in) :: eps_4_crank
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
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
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(SGS_model_coefficient), intent(in) :: Cdiff_scalar
      type(filtering_data_type), intent(in) :: filtering
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
      type(MHD_MG_matrix), intent(in) :: Smatrix
!
      real(kind = kreal), intent(in) :: ak_diffuse(mesh%ele%numele)
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(mesh_SR), intent(inout) :: m_SR
!
!
!      call check_jacobians_triquad(mesh%ele, fem_int%jcs%jac_3d)
!
      if (iflag_SGS_flux .ne. id_SGS_none) then
        call cal_sgs_heat_flux                                          &
     &     (iflag_supg, FEM_prm%npoint_t_evo_int, dt,                   &
     &      iflag_SGS_flux, itype_Csym_flux, i_field, i_filter_s,       &
     &      i_velo, i_filter_v, i_tensor, icomp_sgs_flux,               &
     &      iphys_elediff_vec_v, SGS_param, filter_param,               &
     &      mesh%nod_comm, mesh%node, mesh%ele, fluid,                  &
     &      iphys_ele_base, ele_fld, fem_int%jcs, fem_int%rhs_tbl,      &
     &      FEM_elens, filtering, sgs_coefs, sgs_coefs_nod,             &
     &      mk_MHD%mlump_fl, FEM_SGS_wk%wk_filter, mhd_fem_wk,          &
     &      rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld,         &
     &      m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
      end if
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, 3, i_tensor)
!
      call s_sel_int_scalar_ele(i_field, dt,                            &
     &    iflag_supg, FEM_prm%npoint_t_evo_int, iflag_SGS_flux,         &
     &    ifilter_final, iflag_commute_flux, iflag_commute_field,       &
     &    i_scalar, i_velo, i_gref, i_tensor, i_diff_SGS,               &
     &    mesh, group, fluid, property, ref_param, sf_bcs, ref_fld,     &
     &    iphys_ele_base, ele_fld, fem_int%jcs, fem_int%rhs_tbl,        &
     &    FEM_elens, Cdiff_scalar, diff_coefs, ak_diffuse, mhd_fem_wk,  &
     &    rhs_mat, nod_fld)
!
!
      call s_sel_cal_scalar_pre(iflag_supg, i_field, dt,                &
     &    iflag_commute_field, i_pre_advect, eps_4_crank,               &
     &    FEM_prm, SGS_param, mesh, fluid, property, nod_bcs,           &
     &    iphys_ele_base, ele_fld, fem_int%jcs, fem_int%rhs_tbl,        &
     &    FEM_elens, Cdiff_scalar%coef(1,1), mk_MHD%mlump_fl,           &
     &    Smatrix, ak_diffuse, MGCG_WK, mhd_fem_wk, rhs_mat%fem_wk,     &
     &    rhs_mat%f_l, rhs_mat%f_nl, nod_fld, m_SR)
!
      call set_boundary_scalar(nod_bcs%nod_bc_s, i_field, nod_fld)
      call scalar_send_recv(i_field, mesh%nod_comm, nod_fld,            &
     &                      m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
      end subroutine cal_scalar_field_pre
!
! ----------------------------------------------------------------------
!
      end module cal_temperature
