!
!      module cal_temperature
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!
!!      subroutine cal_temperature_field(i_field, dt, FEM_prm, SGS_par, &
!!     &         mesh, group, fluid, property, ref_param,               &
!!     &         nod_bcs, sf_bcs, iref_grad, ref_fld,                   &
!!     &         iphys, iphys_LES, iphys_ele_base, ele_fld, fem_int,    &
!!     &         FEM_elens, icomp_sgs_term, iak_diff_base, iak_diff_SGS,&
!!     &         iphys_elediff_vec, sgs_coefs, sgs_coefs_nod,           &
!!     &         diff_coefs, filtering, mk_MHD, Smatrix, ak_MHD,        &
!!     &         MGCG_WK, FEM_SGS_wk, mhd_fem_wk, rhs_mat,              &
!!     &         nod_fld, v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(scalar_property), intent(in) :: property
!!        type(reference_scalar_param), intent(in) :: ref_param
!!        type(nodal_bcs_4_scalar_type), intent(in) :: nod_bcs
!!        type(scaler_surf_bc_type), intent(in) :: sf_bcs
!!        type(gradient_field_address), intent(in) :: iref_grad
!!        type(phys_data), intent(in) :: ref_fld
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_term_address), intent(in) :: icomp_sgs_term
!!        type(base_field_address), intent(in) :: iak_diff_base
!!        type(SGS_term_address), intent(in) :: iak_diff_SGS
!!        type(base_field_address), intent(in) :: iphys_elediff_vec
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(MHD_MG_matrix), intent(in) :: Smatrix
!!        type(MGCG_data), intent(inout) :: MGCG_WK
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
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
     &          geofem, MHD_mesh, MHD_prop, nod_bcs, surf_bcs,          &
     &          iref_base, iref_grad, ref_fld, iphys, iphys_LES,        &
     &          ak_MHD, FEM_filters, s_package, MGCG_WK, SGS_MHD_wk,    &
     &          nod_fld, Csims_FEM_MHD, m_SR)
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
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(base_field_address), intent(in) :: iref_base
      type(gradient_field_address), intent(in) :: iref_grad
      type(phys_data), intent(in) :: ref_fld
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(MHD_matrices_pack), intent(in) :: s_package
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(mesh_SR), intent(inout) :: m_SR
!
!     ---- temperature update
!
      if(MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        if(MHD_prop%ref_param_T%iflag_reference .ne. id_no_ref_temp)    &
     &   then
          if(iflag_debug.eq.1) write(*,*) 'cal_temperature_field theta'
          call cal_temperature_field                                    &
     &       (iphys%base%i_per_temp, time_d%dt, FEM_prm,                &
     &        SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,     &
     &        geofem%mesh, geofem%group,                                &
     &        MHD_mesh%fluid, MHD_prop%ht_prop, MHD_prop%ref_param_T,   &
     &        nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs,                       &
     &        iref_grad, ref_fld, iphys, iphys_LES,                     &
     &        SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,            &
     &        SGS_MHD_wk%fem_int, FEM_filters%FEM_elens,                &
     &        Csims_FEM_MHD%icomp_sgs_term,                             &
     &        Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%iak_diff_sgs,  &
     &        Csims_FEM_MHD%iphys_elediff_vec, Csims_FEM_MHD%sgs_coefs, &
     &        Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,    &
     &        FEM_filters%filtering, SGS_MHD_wk%mk_MHD,                 &
     &        s_package%Tmatrix, ak_MHD, MGCG_WK,                       &
     &        SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,             &
     &        SGS_MHD_wk%rhs_mat, nod_fld, m_SR)
!
!$omp parallel
          call add_scalars_smp(nod_fld%n_point,                         &
     &                         ref_fld%d_fld(1,iref_base%i_temp),       &
     &                         nod_fld%d_fld(1,iphys%base%i_per_temp),  &
     &                         nod_fld%d_fld(1,iphys%base%i_temp))
!$omp end parallel
        else
!          call check_surface_param_smp('cal_temperature_field start',  &
!     &        my_rank, sf_grp, geofem%group%surf_nod_grp)
          if (iflag_debug.eq.1) write(*,*) 'cal_temperature_field T'
          call cal_temperature_field                                    &
     &       (iphys%base%i_temp, time_d%dt, FEM_prm,                    &
     &        SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,     &
     &        geofem%mesh, geofem%group, MHD_mesh%fluid,                &
     &        MHD_prop%ht_prop, MHD_prop%ref_param_T,                   &
     &        nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs,                       &
     &        iref_grad, ref_fld, iphys, iphys_LES,                     &
     &        SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,            &
     &        SGS_MHD_wk%fem_int, FEM_filters%FEM_elens,                &
     &        Csims_FEM_MHD%icomp_sgs_term,                             &
     &        Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%iak_diff_sgs,  &
     &        Csims_FEM_MHD%iphys_elediff_vec, Csims_FEM_MHD%sgs_coefs, &
     &        Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,    &
     &        FEM_filters%filtering, SGS_MHD_wk%mk_MHD,                 &
     &        s_package%Tmatrix, ak_MHD, MGCG_WK,                       &
     &        SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,             &
     &        SGS_MHD_wk%rhs_mat, nod_fld, m_SR)
!
          if (iphys%base%i_per_temp .gt. 0) then
!$omp parallel
            call subtract_scalars_smp(nod_fld%n_point,                  &
     &                          nod_fld%d_fld(1,iphys%base%i_temp),     &
     &                          ref_fld%d_fld(1,iref_base%i_temp),      &
     &                          nod_fld%d_fld(1,iphys%base%i_per_temp))
!$omp end parallel
          end if
        end if
!
        call update_with_temperature                                    &
     &    (time_d%i_time_step, time_d%dt, FEM_prm,                      &
     &     SGS_par%iflag_SGS_initial, SGS_par%i_step_sgs_coefs,         &
     &     SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,        &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, surf_bcs%Tsf_bcs, &
     &     iphys, iphys_LES, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int, FEM_filters,         &
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%icomp_diff_base,  &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%rhs_mat, nod_fld, Csims_FEM_MHD%diff_coefs,       &
     &     m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
      end if
!
      end subroutine temperature_evolution
!
!-----------------------------------------------------------------------
!
      subroutine cal_temperature_field(i_field, dt,                     &
     &         FEM_prm, SGS_param, cmt_param, filter_param,             &
     &         mesh, group, fluid, property, ref_param,                 &
     &         nod_bcs, sf_bcs, iref_grad, ref_fld,                     &
     &         iphys, iphys_LES, iphys_ele_base, ele_fld, fem_int,      &
     &         FEM_elens, icomp_sgs_term, iak_diff_base, iak_diff_SGS,  &
     &         iphys_elediff_vec, sgs_coefs, sgs_coefs_nod,             &
     &         diff_coefs, filtering, mk_MHD, Smatrix, ak_MHD,          &
     &         MGCG_WK, FEM_SGS_wk, mhd_fem_wk, rhs_mat,                &
     &         nod_fld, m_SR)
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: dt
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
      type(gradient_field_address), intent(in) :: iref_grad
      type(phys_data), intent(in) :: ref_fld
!
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
      type(filtering_data_type), intent(in) :: filtering
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_term_address), intent(in) :: icomp_sgs_term
      type(base_field_address), intent(in) :: iak_diff_base
      type(SGS_term_address), intent(in) :: iak_diff_SGS
      type(base_field_address), intent(in) :: iphys_elediff_vec
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(in) :: diff_coefs
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
      integer(kind = kint) :: n_int_evo
      integer(kind = kint) :: iflag_SGS_flux
      integer(kind = kint) :: itype_Csym_flux
      integer(kind = kint) :: ifilter_final
      integer(kind = kint) :: iflag_commute_flux
      integer(kind = kint) :: iflag_commute_field
!
      integer(kind = kint) :: i_scalar
      integer(kind = kint) :: i_velo
      integer(kind = kint) :: i_gref
      integer(kind = kint) :: i_tensor
      integer(kind = kint) :: i_filter_s
      integer(kind = kint) :: i_filter_v
      integer(kind = kint) :: i_pre_advect
      integer(kind = kint) :: iak_diff
      integer(kind = kint) :: i_diff_SGS
      integer(kind = kint) :: icomp_sgs_flux
!
      real(kind = kreal) :: eps_4_crank
!
!
      itype_Csym_flux = SGS_param%itype_Csym_h_flux
      i_filter_s =     iphys_LES%filter_fld%i_temp
      icomp_sgs_flux = icomp_sgs_term%i_SGS_h_flux
!
      iflag_supg = FEM_prm%iflag_temp_supg
      n_int_evo =  FEM_prm%npoint_t_evo_int
      iflag_SGS_flux = SGS_param%iflag_SGS_h_flux
      ifilter_final =  SGS_param%ifilter_final
      iflag_commute_flux =  cmt_param%iflag_c_hf
      iflag_commute_field = cmt_param%iflag_c_temp
      i_scalar =   iphys%base%i_temp
      i_velo =     iphys%base%i_velo
      i_gref =     iref_grad%i_grad_temp
      i_pre_advect = iphys%exp_work%i_pre_heat
      i_tensor =   iphys_LES%SGS_term%i_SGS_h_flux
      iak_diff =   iak_diff_base%i_temp
      i_diff_SGS = iak_diff_SGS%i_SGS_h_flux
      call cal_scalar_field_pre(i_field, dt,                            &
     &          iflag_supg, n_int_evo, iflag_SGS_flux, itype_Csym_flux, &
     &          ifilter_final, iflag_commute_flux, iflag_commute_field, &
     &          i_scalar, i_velo, i_gref, i_tensor,                     &
     &          i_filter_s, i_filter_v, i_pre_advect,                   &
     &          iak_diff, i_diff_SGS, icomp_sgs_flux, eps_4_crank,      &
     &          FEM_prm, SGS_param, filter_param, mesh, group,          &
     &          fluid, property, ref_param, nod_bcs, sf_bcs,            &
     &          ref_fld, iphys_ele_base, ele_fld, fem_int, FEM_elens,   &
     &          iphys_elediff_vec, sgs_coefs, sgs_coefs_nod,            &
     &          diff_coefs, filtering, mk_MHD%mlump_fl, Smatrix,        &
     &          ak_MHD%ak_d_temp,  MGCG_WK, FEM_SGS_wk%wk_filter,       &
     &          mhd_fem_wk, rhs_mat, nod_fld, m_SR)
!
      end subroutine cal_temperature_field
!
! ----------------------------------------------------------------------
!
      subroutine cal_scalar_field_pre(i_field, dt,                      &
     &          iflag_supg, n_int_evo, iflag_SGS_flux, itype_Csym_flux, &
     &          ifilter_final, iflag_commute_flux, iflag_commute_field, &
     &          i_scalar, i_velo, i_gref, i_tensor,                     &
     &          i_filter_s, i_filter_v, i_pre_advect,                   &
     &          iak_diff, i_diff_SGS, icomp_sgs_flux, eps_4_crank,      &
     &          FEM_prm, SGS_param, filter_param, mesh, group,          &
     &          fluid, property, ref_param, nod_bcs, sf_bcs,            &
     &          ref_fld, iphys_ele_base, ele_fld, fem_int, FEM_elens,   &
     &          iphys_elediff_vec, sgs_coefs, sgs_coefs_nod,            &
     &          diff_coefs, filtering, mlump_fl, Smatrix, ak_diffuse,   &
     &          MGCG_WK, wk_filter, mhd_fem_wk, rhs_mat, nod_fld, m_SR)
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
      integer(kind = kint), intent(in) :: n_int_evo
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
      integer(kind = kint), intent(in) :: iak_diff
      integer(kind = kint), intent(in) :: i_diff_SGS
      integer(kind = kint), intent(in) :: icomp_sgs_flux
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
      type(base_field_address), intent(in) :: iphys_elediff_vec
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(filtering_data_type), intent(in) :: filtering
      type(lumped_mass_matrices), intent(in) :: mlump_fl
      type(MHD_MG_matrix), intent(in) :: Smatrix
!
      real(kind = kreal), intent(in) :: ak_diffuse(mesh%ele%numele)
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(mesh_SR), intent(inout) :: m_SR
!
!
!      call check_jacobians_triquad(mesh%ele, fem_int%jcs%jac_3d)
!
      if (iflag_SGS_flux .ne. id_SGS_none) then
        call cal_sgs_heat_flux(iflag_supg, n_int_evo, dt,               &
     &      iflag_SGS_flux, itype_Csym_flux, i_field, i_filter_s,       &
     &      i_velo, i_filter_v, i_tensor, icomp_sgs_flux,               &
     &      iphys_elediff_vec%i_velo, SGS_param, filter_param,          &
     &      mesh%nod_comm, mesh%node, mesh%ele, fluid,                  &
     &      iphys_ele_base, ele_fld, fem_int%jcs, fem_int%rhs_tbl,      &
     &      FEM_elens, filtering, sgs_coefs, sgs_coefs_nod, mlump_fl,   &
     &      wk_filter, mhd_fem_wk, rhs_mat%fem_wk,                      &
     &      rhs_mat%f_l, rhs_mat%f_nl, nod_fld,                         &
     &       m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
      end if
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, 3, i_tensor)
!
      call s_sel_int_scalar_ele                                         &
     &   (i_field, dt, iflag_supg, n_int_evo, iflag_SGS_flux,           &
     &    ifilter_final, iflag_commute_flux, iflag_commute_field,       &
     &    i_scalar, i_velo, i_gref, i_tensor, iak_diff, i_diff_SGS,     &
     &    mesh, group, fluid, property, ref_param, sf_bcs, ref_fld,     &
     &    iphys_ele_base, ele_fld, fem_int%jcs, fem_int%rhs_tbl,        &
     &    FEM_elens, diff_coefs, ak_diffuse, mhd_fem_wk,                &
     &    rhs_mat, nod_fld)
!
!
      call s_sel_cal_scalar_pre(iflag_supg, i_field, dt,                &
     &    iflag_commute_field, i_pre_advect, iak_diff, eps_4_crank,     &
     &    FEM_prm, SGS_param, mesh, fluid, property, nod_bcs,           &
     &    iphys_ele_base, ele_fld, fem_int%jcs,                         &
     &    fem_int%rhs_tbl, FEM_elens,  diff_coefs, mlump_fl,            &
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
