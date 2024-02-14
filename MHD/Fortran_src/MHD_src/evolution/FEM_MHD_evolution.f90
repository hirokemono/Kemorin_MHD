!>@file   FEM_MHD_evolution.f90
!!        module FEM_MHD_evolution
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine fields_evolution(time_d, FEM_prm, SGS_par,           &
!!     &          geofem, MHD_mesh, MHD_prop, nod_bcs, surf_bcs,        &
!!     &          iref_base, iref_grad, ref_fld, iphys, iphys_LES,      &
!!     &          ak_MHD, FEM_filters, s_package, MGCG_WK, SGS_MHD_wk,  &
!!     &          nod_fld, Csims_FEM_MHD, fem_sq, m_SR)
!!      subroutine update_fields(time_d, FEM_prm, SGS_par,              &
!!     &         geofem, MHD_mesh, nod_bcs, surf_bcs, iphys, iphys_LES, &
!!     &         FEM_filters, SGS_MHD_wk, nod_fld, Csims_FEM_MHD, m_SR)
!!      subroutine reset_update_flag(nod_fld, sgs_coefs, diff_coefs)
!!
!!      subroutine fields_evo_for_FEM_SPH(time_d, FEM_prm, SGS_par,     &
!!     &          geofem, MHD_mesh, MHD_prop, nod_bcs, surf_bcs,        &
!!     &          iref_base, iref_grad, ref_fld, iphys, iphys_LES,      &
!!     &          ak_MHD, FEM_filters, s_package, MGCG_WK, SGS_MHD_wk,  &
!!     &          nod_fld, Csims_FEM_MHD, fem_sq, v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(base_field_address), intent(in) :: iref_base
!!        type(gradient_field_address), intent(in) :: iref_grad
!!        type(phys_data), intent(in) :: ref_fld
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(SGS_term_address), intent(in) :: iak_sgs_term
!!        type(SGS_term_address), intent(in) :: icomp_sgs_term
!!        type(base_field_address), intent(in) :: iak_diff_base
!!        type(base_field_address), intent(in) :: icomp_diff_base
!!        type(base_field_address), intent(in) :: iphys_elediff_vec
!!        type(base_field_address), intent(in) :: iphys_elediff_fil
!!        type(MHD_matrices_pack), intent(in) :: s_package
!!        type(MGCG_data), intent(inout) :: MGCG_WK
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!      subroutine set_perturbation_to_scalar(MHD_prop,                 &
!!     &          iref_base, ref_fld, iphys, nod_fld)
!!         type(MHD_evolution_param), intent(in) :: MHD_prop
!!         type(base_field_address), intent(in) :: iref_base
!!         type(phys_data), intent(in) :: ref_fld
!!         type(phys_address), intent(in) :: iphys
!!         type(phys_data), intent(inout) :: nod_fld
!!      subroutine reset_update_flag(nod_fld, sgs_coefs, diff_coefs)
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!@endverbatim
!
      module FEM_MHD_evolution
!
      use m_precision
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_control_parameter
      use t_physical_property
      use t_reference_scalar_param
      use t_time_data
      use t_mesh_data
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_grad_field_labels
      use t_SGS_model_addresses
      use t_table_FEM_const
      use t_material_property
      use t_SGS_model_coefs
      use t_FEM_MHD_filter_data
      use t_surface_bc_data_MHD
      use t_bc_data_MHD
      use t_MHD_matrices_pack
      use t_MGCG_data
      use t_MHD_mass_matrices
      use t_SGS_model_coefs
      use t_FEM_MHD_mean_square
      use t_work_FEM_SGS_MHD
      use t_FEM_SGS_model_coefs
      use t_mesh_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fields_evolution(time_d, FEM_prm, SGS_par,             &
     &          geofem, MHD_mesh, MHD_prop, nod_bcs, surf_bcs,          &
     &          iref_base, iref_grad, ref_fld, iphys, iphys_LES,        &
     &          ak_MHD, FEM_filters, s_package, MGCG_WK, SGS_MHD_wk,    &
     &          nod_fld, Csims_FEM_MHD, fem_sq, m_SR)
!
      use calypso_mpi
      use cal_temperature
      use cal_velocity
      use cal_magnetic_field
      use cal_light_element
      use cal_add_smp
      use cal_subtract_smp
!
      use update_with_scalars
      use update_with_velo
      use update_with_vector_p
      use update_with_magne
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
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if (iflag_debug.eq.1) write(*,*) 'reset_update_flag'
      call reset_update_flag                                            &
     &   (nod_fld, Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs)
!
!     ---- magnetic field update
!
      if(MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'cal_vector_potential'
        call cal_vector_potential                                       &
     &     (time_d%dt, FEM_prm, SGS_par, geofem%mesh, geofem%group,     &
     &      MHD_mesh%conduct, MHD_prop%cd_prop, nod_bcs%Bnod_bcs,       &
     &      surf_bcs%Asf_bcs, surf_bcs%Fsf_bcs, iphys, iphys_LES,       &
     &      SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,              &
     &      SGS_MHD_wk%fem_int, Csims_FEM_MHD, FEM_filters,             &
     &      SGS_MHD_wk%mk_MHD, s_package%Bmatrix, s_package%Fmatrix,    &
     &      ak_MHD%ak_d_magne, MGCG_WK, SGS_MHD_wk%FEM_SGS_wk,          &
     &      SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, fem_sq,          &
     &      nod_fld, m_SR)

        call update_with_vector_potential                               &
     &    (time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, MHD_mesh%conduct, &
     &     nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Fsf_bcs,        &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%iphys_ele_fil, SGS_MHD_wk%fem_int, FEM_filters,   &
     &     Csims_FEM_MHD%iak_diff_base,                                 &
     &     Csims_FEM_MHD%icomp_diff_base%i_magne,                       &
     &     Csims_FEM_MHD%iphys_elediff_vec%i_magne,                     &
     &     Csims_FEM_MHD%iphys_elediff_fil%i_magne,                     &
     &     SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,                &
     &     SGS_MHD_wk%rhs_mat, nod_fld, SGS_MHD_wk%ele_fld,             &
     &    Csims_FEM_MHD%diff_coefs, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
      else if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution)  &
     &     then
!
!        call check_surface_param_smp('cal_magnetic_field start',       &
!     &      my_rank, sf_grp, geofem%group%surf_nod_grp)
        if (iflag_debug.eq.1) write(*,*) 's_cal_magnetic_field'
        call s_cal_magnetic_field                                       &
     &    (time_d%dt, FEM_prm, SGS_par, geofem%mesh, geofem%group,      &
     &     MHD_mesh%conduct, MHD_prop%cd_prop,                          &
     &     nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs,                          &
     &     surf_bcs%Bsf_bcs, surf_bcs%Fsf_bcs, iphys, iphys_LES,        &
     &     SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,               &
     &     SGS_MHD_wk%fem_int, Csims_FEM_MHD, FEM_filters,              &
     &     Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod,        &
     &     Csims_FEM_MHD%diff_coefs, SGS_MHD_wk%mk_MHD,                 &
     &     s_package%Bmatrix, s_package%Fmatrix, ak_MHD%ak_d_magne,     &
     &     MGCG_WK, SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,       &
     &     SGS_MHD_wk%rhs_mat, fem_sq, nod_fld, m_SR)
        call update_with_magnetic_field                                 &
     &    (time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, MHD_mesh%conduct, &
     &     surf_bcs%Bsf_bcs, surf_bcs%Fsf_bcs,                          &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%iphys_ele_fil, SGS_MHD_wk%fem_int, FEM_filters,   &
     &     Csims_FEM_MHD%iak_diff_base%i_magne,                         &
     &     Csims_FEM_MHD%icomp_diff_base%i_magne,                       &
     &     Csims_FEM_MHD%iphys_elediff_vec%i_magne,                     &
     &     Csims_FEM_MHD%iphys_elediff_fil%i_magne,                     &
     &     SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,                &
     &     SGS_MHD_wk%rhs_mat, nod_fld, SGS_MHD_wk%ele_fld,             &
     &    Csims_FEM_MHD%diff_coefs, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
      end if
!
!     ---- temperature update
      call temperature_evolution                                        &
     &   (time_d, FEM_prm, SGS_par, geofem, MHD_mesh,                   &
     &    MHD_prop%ht_prop, MHD_prop%ref_param_T,                       &
     &    nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs,                           &
     &    iref_base, iref_grad, ref_fld, iphys, iphys_LES,              &
     &    ak_MHD%ak_d_temp, FEM_filters, s_package%Tmatrix,             &
     &    Csims_FEM_MHD%icomp_sgs_term%i_SGS_h_flux,                    &
     &    Csims_FEM_MHD%iak_diff_base%i_temp,                           &
     &    Csims_FEM_MHD%icomp_diff_base%i_temp,                         &
     &    Csims_FEM_MHD%iak_diff_SGS%i_SGS_h_flux,                      &
     &    Csims_FEM_MHD%iphys_elediff_vec%i_velo,                       &
     &    Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod,         &
     &    MGCG_WK, SGS_MHD_wk, nod_fld, Csims_FEM_MHD%diff_coefs, m_SR)
!
!     ----- composition update
      call light_element_evolution                                      &
     &   (time_d, FEM_prm, SGS_par, geofem, MHD_mesh,                   &
     &    MHD_prop%cp_prop, MHD_prop%ref_param_C,                       &
     &    nod_bcs%Cnod_bcs, surf_bcs%Csf_bcs,                           &
     &    iref_base, iref_grad, ref_fld, iphys, iphys_LES,              &
     &    ak_MHD%ak_d_composit, FEM_filters, s_package%Cmatrix,         &
     &    Csims_FEM_MHD%icomp_sgs_term%i_SGS_c_flux,                    &
     &    Csims_FEM_MHD%iak_diff_base%i_light,                          &
     &    Csims_FEM_MHD%icomp_diff_base%i_light,                        &
     &    Csims_FEM_MHD%iak_diff_SGS%i_SGS_c_flux,                      &
     &    Csims_FEM_MHD%iphys_elediff_vec%i_velo,                       &
     &    Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod,         &
     &    MGCG_WK, SGS_MHD_wk, nod_fld, Csims_FEM_MHD%diff_coefs, m_SR)
!
!     ---- velocity update
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug.eq.1) write(*,*) 'velocity_evolution'
        call velocity_evolution(time_d%time, time_d%dt,                 &
     &      FEM_prm, SGS_par, geofem%mesh, geofem%group,                &
     &      MHD_mesh%fluid,  MHD_prop%fl_prop, MHD_prop%cd_prop,        &
     &      nod_bcs%Vnod_bcs, surf_bcs%Vsf_bcs,                         &
     &      surf_bcs%Bsf_bcs, surf_bcs%Psf_bcs,                         &
     &      iphys, iphys_LES, SGS_MHD_wk%iphys_ele_base, ak_MHD,        &
     &      SGS_MHD_wk%fem_int, FEM_filters,                            &
     &      Csims_FEM_MHD%iak_sgs_term, Csims_FEM_MHD%icomp_sgs_term,   &
     &      Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%iak_diff_sgs,    &
     &      Csims_FEM_MHD%iphys_elediff_vec,                            &
     &      Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,      &
     &      SGS_MHD_wk%mk_MHD, s_package%Vmatrix, s_package%Pmatrix,    &
     &      MGCG_WK, SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,      &
     &      SGS_MHD_wk%rhs_mat, nod_fld, SGS_MHD_wk%ele_fld,            &
     &      Csims_FEM_MHD%sgs_coefs, fem_sq,                            &
     &      m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        call update_with_velocity(time_d%i_time_step, time_d%dt,        &
     &     FEM_prm, SGS_par, geofem%mesh, geofem%group, MHD_mesh%fluid, &
     &     surf_bcs%Vsf_bcs, surf_bcs%Psf_bcs,                          &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%iphys_ele_fil, SGS_MHD_wk%fem_int, FEM_filters,   &
     &     Csims_FEM_MHD%iak_diff_base%i_velo,                          &
     &     Csims_FEM_MHD%icomp_diff_base%i_velo,                        &
     &     Csims_FEM_MHD%iphys_elediff_vec%i_velo,                      &
     &     Csims_FEM_MHD%iphys_elediff_fil%i_velo,                      &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, nod_fld,          &
     &     SGS_MHD_wk%ele_fld, Csims_FEM_MHD%diff_coefs,                &
     &     m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
      end if
!
      end subroutine fields_evolution
!
!-----------------------------------------------------------------------
!
      subroutine update_fields(time_d, FEM_prm, SGS_par,                &
     &         geofem, MHD_mesh, nod_bcs, surf_bcs, iphys, iphys_LES,   &
     &         FEM_filters, SGS_MHD_wk, nod_fld, Csims_FEM_MHD, m_SR)
!
      use average_on_elements
      use update_with_scalars
      use update_with_velo
      use update_with_vector_p
      use update_with_magne
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(filters_on_FEM), intent(in) :: FEM_filters
!
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_scalar, i_pert
      integer(kind = kint) :: i_filter_s, i_SGS_wk_field
      integer(kind = kint) :: iphys_wfl_scalar, iphys_fefx_buo_gen
      integer(kind = kint) :: iak_diff, icomp_diff_t
!
      integer(kind = kint) :: iflag_supg, n_int_evo
      integer(kind = kint) :: iflag_SGS_flux
      integer(kind = kint) :: iflag_commute_field
!
!
      if(iphys%base%i_velo .ne. 0) then
        call update_with_velocity(time_d%i_time_step, time_d%dt,        &
     &     FEM_prm, SGS_par, geofem%mesh, geofem%group, MHD_mesh%fluid, &
     &     surf_bcs%Vsf_bcs, surf_bcs%Psf_bcs,                          &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%iphys_ele_fil, SGS_MHD_wk%fem_int, FEM_filters,   &
     &     Csims_FEM_MHD%iak_diff_base%i_velo,                          &
     &     Csims_FEM_MHD%icomp_diff_base%i_velo,                        &
     &     Csims_FEM_MHD%iphys_elediff_vec%i_velo,                      &
     &     Csims_FEM_MHD%iphys_elediff_fil%i_velo,                      &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, nod_fld,          &
     &     SGS_MHD_wk%ele_fld, Csims_FEM_MHD%diff_coefs,                &
     &     m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
      end if
!
      if(iphys%base%i_temp .ne. 0) then
        i_scalar =     iphys%base%i_temp
        i_pert =       iphys%base%i_per_temp
        i_filter_s =         iphys_LES%filter_fld%i_temp
        i_SGS_wk_field =     iphys_LES%SGS_wk%i_sgs_temp
        iphys_wfl_scalar =   iphys_LES%wide_filter_fld%i_temp
        iphys_fefx_buo_gen = iphys_LES%eflux_by_filter%i_buo_gen
        iak_diff =     Csims_FEM_MHD%iak_diff_base%i_temp
        icomp_diff_t = Csims_FEM_MHD%icomp_diff_base%i_temp
!
        iflag_supg = FEM_prm%iflag_temp_supg
        n_int_evo =  FEM_prm%npoint_t_evo_int
        iflag_SGS_flux =      SGS_par%model_p%SGS_heat%iflag_SGS_flux
        iflag_commute_field = SGS_par%model_p%SGS_heat%iflag_commute_field
        call update_with_scalar                                         &
     &    (time_d%i_time_step, time_d%dt, i_scalar, i_pert, i_filter_s, &
     &     i_SGS_wk_field, iphys_wfl_scalar, iphys_fefx_buo_gen,        &
     &     iflag_supg, n_int_evo, iflag_SGS_flux, iflag_commute_field,  &
     &     SGS_par%iflag_SGS_initial, SGS_par%i_step_sgs_coefs,         &
     &     SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,        &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, surf_bcs%Tsf_bcs, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int, FEM_filters,         &
     &     iak_diff, icomp_diff_t,                                      &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%rhs_mat, nod_fld, Csims_FEM_MHD%diff_coefs,       &
     &     m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
      end if
!
      if(iphys%base%i_light .ne. 0) then
        i_scalar =     iphys%base%i_light
        i_pert =       iphys%base%i_per_light
        i_filter_s =         iphys_LES%filter_fld%i_light
        i_SGS_wk_field =     iphys_LES%SGS_wk%i_sgs_composit
        iphys_wfl_scalar =   iphys_LES%wide_filter_fld%i_light
        iphys_fefx_buo_gen = iphys_LES%eflux_by_filter%i_c_buo_gen
        iak_diff =      Csims_FEM_MHD%iak_diff_base%i_light
        icomp_diff_t =  Csims_FEM_MHD%icomp_diff_base%i_light
!
        iflag_supg = FEM_prm%iflag_comp_supg
        n_int_evo =  FEM_prm%npoint_t_evo_int
        iflag_SGS_flux =      SGS_par%model_p%SGS_light%iflag_SGS_flux
        iflag_commute_field = SGS_par%model_p%SGS_light%iflag_commute_field
        call update_with_scalar                                         &
     &    (time_d%i_time_step, time_d%dt, i_scalar, i_pert, i_filter_s, &
     &     i_SGS_wk_field, iphys_wfl_scalar, iphys_fefx_buo_gen,        &
     &     iflag_supg, n_int_evo, iflag_SGS_flux, iflag_commute_field,  &
     &     SGS_par%iflag_SGS_initial, SGS_par%i_step_sgs_coefs,         &
     &     SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,        &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, surf_bcs%Csf_bcs, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int, FEM_filters,         &
     &     iak_diff, icomp_diff_t,                                      &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%rhs_mat, nod_fld, Csims_FEM_MHD%diff_coefs,       &
     &     m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
      end if
!
      if(iphys%base%i_vecp .ne. 0) then
        call update_with_vector_potential                               &
     &    (time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, MHD_mesh%conduct, &
     &     nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Fsf_bcs,        &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%iphys_ele_fil, SGS_MHD_wk%fem_int, FEM_filters,   &
     &     Csims_FEM_MHD%iak_diff_base,                                 &
     &     Csims_FEM_MHD%icomp_diff_base%i_magne,                       &
     &     Csims_FEM_MHD%iphys_elediff_vec%i_magne,                     &
     &     Csims_FEM_MHD%iphys_elediff_fil%i_magne,                     &
     &     SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,                &
     &     SGS_MHD_wk%rhs_mat, nod_fld, SGS_MHD_wk%ele_fld,             &
     &    Csims_FEM_MHD%diff_coefs, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
      else if(iphys%base%i_magne.ne.0) then
        call update_with_magnetic_field                                 &
     &    (time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, MHD_mesh%conduct, &
     &     surf_bcs%Bsf_bcs, surf_bcs%Fsf_bcs,                          &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%iphys_ele_fil, SGS_MHD_wk%fem_int, FEM_filters,   &
     &     Csims_FEM_MHD%iak_diff_base%i_magne,                         &
     &     Csims_FEM_MHD%icomp_diff_base%i_magne,                       &
     &     Csims_FEM_MHD%iphys_elediff_vec%i_magne,                     &
     &     Csims_FEM_MHD%iphys_elediff_fil%i_magne,                     &
     &     SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,                &
     &     SGS_MHD_wk%rhs_mat, nod_fld, SGS_MHD_wk%ele_fld,             &
     &    Csims_FEM_MHD%diff_coefs, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
      end if
!
      end subroutine update_fields
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fields_evo_for_FEM_SPH(time_d, FEM_prm, SGS_par,       &
     &          geofem, MHD_mesh, MHD_prop, nod_bcs, surf_bcs,          &
     &          iref_base, iref_grad, ref_fld, iphys, iphys_LES,        &
     &          ak_MHD, FEM_filters, s_package, MGCG_WK, SGS_MHD_wk,    &
     &          nod_fld, Csims_FEM_MHD, fem_sq, m_SR)
!
      use cal_temperature
      use cal_velocity
      use cal_light_element
      use cal_add_smp
      use cal_subtract_smp
!
      use update_with_scalars
      use update_with_velo
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
!
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if (iflag_debug.eq.1) write(*,*) 'reset_update_flag'
      call reset_update_flag                                            &
     &   (nod_fld, Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%diff_coefs)
!
!     ---- temperature update
      call temperature_evolution                                        &
     &   (time_d, FEM_prm, SGS_par, geofem, MHD_mesh,                   &
     &    MHD_prop%ht_prop, MHD_prop%ref_param_T,                       &
     &    nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs,                           &
     &    iref_base, iref_grad, ref_fld, iphys, iphys_LES,              &
     &    ak_MHD%ak_d_temp, FEM_filters, s_package%Tmatrix,             &
     &    Csims_FEM_MHD%icomp_sgs_term%i_SGS_h_flux,                    &
     &    Csims_FEM_MHD%iak_diff_base%i_temp,                           &
     &    Csims_FEM_MHD%icomp_diff_base%i_temp,                         &
     &    Csims_FEM_MHD%iak_diff_SGS%i_SGS_h_flux,                      &
     &    Csims_FEM_MHD%iphys_elediff_vec%i_velo,                       &
     &    Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod,         &
     &    MGCG_WK, SGS_MHD_wk, nod_fld, Csims_FEM_MHD%diff_coefs, m_SR)
!
!     ----- composition update
      call light_element_evolution                                      &
     &   (time_d, FEM_prm, SGS_par, geofem, MHD_mesh,                   &
     &    MHD_prop%cp_prop, MHD_prop%ref_param_C,                       &
     &    nod_bcs%Cnod_bcs, surf_bcs%Csf_bcs,                           &
     &    iref_base, iref_grad, ref_fld, iphys, iphys_LES,              &
     &    ak_MHD%ak_d_composit, FEM_filters, s_package%Cmatrix,         &
     &    Csims_FEM_MHD%icomp_sgs_term%i_SGS_c_flux,                    &
     &    Csims_FEM_MHD%iak_diff_base%i_light,                          &
     &    Csims_FEM_MHD%icomp_diff_base%i_light,                        &
     &    Csims_FEM_MHD%iak_diff_SGS%i_SGS_c_flux,                      &
     &    Csims_FEM_MHD%iphys_elediff_vec%i_velo,                       &
     &    Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod,         &
     &    MGCG_WK, SGS_MHD_wk, nod_fld, Csims_FEM_MHD%diff_coefs, m_SR)
!
!     ---- velocity update
!
      if (MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'velocity_evolution'
        call velocity_evolution(time_d%time, time_d%dt, FEM_prm,        &
     &      SGS_par, geofem%mesh, geofem%group, MHD_mesh%fluid,         &
     &      MHD_prop%fl_prop, MHD_prop%cd_prop, nod_bcs%Vnod_bcs,       &
     &      surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs, surf_bcs%Psf_bcs,       &
     &      iphys, iphys_LES, SGS_MHD_wk%iphys_ele_base, ak_MHD,        &
     &      SGS_MHD_wk%fem_int, FEM_filters,                            &
     &      Csims_FEM_MHD%iak_sgs_term, Csims_FEM_MHD%icomp_sgs_term,   &
     &      Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%iak_diff_sgs,    &
     &      Csims_FEM_MHD%iphys_elediff_vec,                            &
     &      Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,      &
     &      SGS_MHD_wk%mk_MHD, s_package%Vmatrix, s_package%Pmatrix,    &
     &      MGCG_WK, SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,      &
     &      SGS_MHD_wk%rhs_mat, nod_fld, SGS_MHD_wk%ele_fld,            &
     &      Csims_FEM_MHD%sgs_coefs, fem_sq,                            &
     &      m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        call update_with_velocity(time_d%i_time_step, time_d%dt,        &
     &     FEM_prm, SGS_par, geofem%mesh, geofem%group, MHD_mesh%fluid, &
     &     surf_bcs%Vsf_bcs, surf_bcs%Psf_bcs,                          &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%iphys_ele_fil, SGS_MHD_wk%fem_int, FEM_filters,   &
     &     Csims_FEM_MHD%iak_diff_base%i_velo,                          &
     &     Csims_FEM_MHD%icomp_diff_base%i_velo,                        &
     &     Csims_FEM_MHD%iphys_elediff_vec%i_velo,                      &
     &     Csims_FEM_MHD%iphys_elediff_fil%i_velo,                      &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, nod_fld,          &
     &     SGS_MHD_wk%ele_fld, Csims_FEM_MHD%diff_coefs,                &
     &     m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
      end if
!
      end subroutine fields_evo_for_FEM_SPH
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_perturbation_to_scalar(MHD_prop,                   &
     &          iref_base, ref_fld, iphys, nod_fld)
!
      use cal_subtract_smp
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(base_field_address), intent(in) :: iref_base
      type(phys_data), intent(in) :: ref_fld
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if(MHD_prop%ref_param_T%iflag_reference .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1)  write(*,*) 'set_2_perturbation_temp'
!$omp parallel
          call subtract_scalars_smp(nod_fld%n_point,                    &
     &                          nod_fld%d_fld(1,iphys%base%i_temp),     &
     &                          ref_fld%d_fld(1,iref_base%i_temp),      &
     &                          nod_fld%d_fld(1,iphys%base%i_per_temp))
!$omp end parallel
      end if
      if(MHD_prop%ref_param_C%iflag_reference .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1)  write(*,*) 'set_2_perturbation_comp'
!$omp parallel
          call subtract_scalars_smp(nod_fld%n_point,                    &
     &                         nod_fld%d_fld(1,iphys%base%i_light),     &
     &                         ref_fld%d_fld(1,iref_base%i_light),      &
     &                         nod_fld%d_fld(1,iphys%base%i_per_light))
!$omp end parallel
      end if
!
      end subroutine set_perturbation_to_scalar
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_update_flag(nod_fld, sgs_coefs, diff_coefs)
!
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!     reset monitoring flag
!
      nod_fld%iflag_update(1:nod_fld%ntot_phys) = 0
      sgs_coefs%iflag_field(1:sgs_coefs%num_field) = 0
      diff_coefs%iflag_field(1:diff_coefs%num_field) = 0
!
      end subroutine reset_update_flag
!
!-----------------------------------------------------------------------
!
      end module FEM_MHD_evolution
