!>@file   FEM_MHD_evolution.f90
!!        module FEM_MHD_evolution
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine fields_evolution                                     &
!!     &         (time_d, FEM_prm, SGS_par, geofem, MHD_mesh,           &
!!     &          MHD_prop, nod_bcs, surf_bcs, iphys, iphys_LES,        &
!!     &          ak_MHD, FEM_filters, s_package, MGCG_WK, SGS_MHD_wk,  &
!!     &          nod_fld, Csims_FEM_MHD, fem_sq, v_sol)
!!      subroutine update_fields(time_d, FEM_prm, SGS_par,              &
!!     &         geofem, MHD_mesh, nod_bcs, surf_bcs, iphys, iphys_LES, &
!!     &         FEM_filters, SGS_MHD_wk, nod_fld, Csims_FEM_MHD, v_sol)
!!      subroutine reset_update_flag(nod_fld, sgs_coefs, diff_coefs)
!!
!!      subroutine fields_evo_for_FEM_SPH(time_d, FEM_prm, SGS_par,     &
!!     &          geofem, fluid, MHD_prop, nod_bcs, surf_bcs,           &
!!     &          iphys, iphys_LES, ak_MHD, FEM_filters,                &
!!     &          s_package, MGCG_WK, SGS_MHD_wk, nod_fld,              &
!!     &          Csims_FEM_MHD, fem_sq, v_sol)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
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
!!
!!      subroutine set_perturbation_to_scalar(MHD_prop, iphys, nod_fld)
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
      use t_vector_for_solver
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fields_evolution                                       &
     &         (time_d, FEM_prm, SGS_par, geofem, MHD_mesh,             &
     &          MHD_prop, nod_bcs, surf_bcs, iphys, iphys_LES,          &
     &          ak_MHD, FEM_filters, s_package, MGCG_WK, SGS_MHD_wk,    &
     &          nod_fld, Csims_FEM_MHD, fem_sq, v_sol)
!
      use calypso_mpi
      use cal_temperature
      use cal_velocity
      use cal_magnetic_field
      use cal_light_element
      use copy_nodal_fields
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
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      if (iflag_debug.eq.1) write(*,*) 'reset_update_flag'
      call reset_update_flag                                            &
     &   (nod_fld, Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs)
      call calypso_mpi_barrier
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
     &      nod_fld, v_sol)

        call update_with_vector_potential                               &
     &    (time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, MHD_mesh%conduct, &
     &     nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Fsf_bcs,        &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%iphys_ele_fil, SGS_MHD_wk%fem_int, FEM_filters,   &
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%icomp_diff_base,  &
     &     Csims_FEM_MHD%iphys_elediff_vec,                             &
     &     Csims_FEM_MHD%iphys_elediff_fil,                             &
     &     SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,                &
     &     SGS_MHD_wk%rhs_mat, nod_fld, SGS_MHD_wk%ele_fld,             &
     &     Csims_FEM_MHD%diff_coefs, v_sol)
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
     &     SGS_MHD_wk%mk_MHD, s_package%Bmatrix, s_package%Fmatrix,     &
     &     ak_MHD%ak_d_magne, MGCG_WK, SGS_MHD_wk%FEM_SGS_wk,           &
     &     SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat,                   &
     &     fem_sq, nod_fld, v_sol)
        call update_with_magnetic_field                                 &
     &    (time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, MHD_mesh%conduct, &
     &     surf_bcs%Bsf_bcs, surf_bcs%Fsf_bcs,                          &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%iphys_ele_fil, SGS_MHD_wk%fem_int, FEM_filters,   &
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%icomp_diff_base,  &
     &     Csims_FEM_MHD%iphys_elediff_vec,                             &
     &     Csims_FEM_MHD%iphys_elediff_fil,                             &
     &     SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,                &
     &     SGS_MHD_wk%rhs_mat, nod_fld, SGS_MHD_wk%ele_fld,             &
     &     Csims_FEM_MHD%diff_coefs, v_sol)
      end if
!
!     ---- temperature update
!
      if(MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        if(MHD_prop%ref_param_T%iflag_reference .ne. id_no_ref_temp)    &
     &   then
          if(iflag_debug.eq.1) write(*,*) 'cal_temperature_field theta'
          call cal_temperature_field(iphys%base%i_per_temp, time_d%dt,  &
     &       FEM_prm, SGS_par, geofem%mesh, geofem%group,               &
     &       MHD_mesh%fluid, MHD_prop%ht_prop, MHD_prop%ref_param_T,    &
     &       nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, iphys, iphys_LES,      &
     &       SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,             &
     &       SGS_MHD_wk%fem_int, FEM_filters%FEM_elens,                 &
     &       Csims_FEM_MHD%icomp_sgs_term, Csims_FEM_MHD%iak_diff_base, &
     &       Csims_FEM_MHD%iak_diff_sgs,                                &
     &       Csims_FEM_MHD%iphys_elediff_vec,                           &
     &       Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%sgs_coefs_nod,  &
     &       Csims_FEM_MHD%diff_coefs,                                  &
     &       FEM_filters%filtering, SGS_MHD_wk%mk_MHD,                  &
     &       s_package%Tmatrix, ak_MHD, MGCG_WK,                        &
     &       SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,              &
     &       SGS_MHD_wk%rhs_mat, nod_fld, v_sol)
!
          call add_2_nod_scalars(nod_fld,                               &
     &        iphys%base%i_ref_t, iphys%base%i_per_temp,                &
     &        iphys%base%i_temp)
        else
!          call check_surface_param_smp('cal_temperature_field start',  &
!     &        my_rank, sf_grp, geofem%group%surf_nod_grp)
          if (iflag_debug.eq.1) write(*,*) 'cal_temperature_field T'
          call cal_temperature_field(iphys%base%i_temp, time_d%dt,      &
     &        FEM_prm, SGS_par, geofem%mesh, geofem%group,              &
     &        MHD_mesh%fluid, MHD_prop%ht_prop, MHD_prop%ref_param_T,   &
     &        nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, iphys, iphys_LES,     &
     &        SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,            &
     &        SGS_MHD_wk%fem_int, FEM_filters%FEM_elens,                &
     &        Csims_FEM_MHD%icomp_sgs_term,                             &
     &        Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%iak_diff_sgs,  &
     &        Csims_FEM_MHD%iphys_elediff_vec,                          &
     &        Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%sgs_coefs_nod, &
     &        Csims_FEM_MHD%diff_coefs,                                 &
     &        FEM_filters%filtering, SGS_MHD_wk%mk_MHD,                 &
     &        s_package%Tmatrix, ak_MHD, MGCG_WK,                       &
     &        SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,             &
     &        SGS_MHD_wk%rhs_mat, nod_fld, v_sol)
!
        if (iphys%base%i_per_temp .gt. 0) then
          call subtract_2_nod_scalars(nod_fld,                          &
     &        iphys%base%i_temp, iphys%base%i_ref_t,                    &
     &        iphys%base%i_per_temp)
        end if
      end if
!
        call update_with_temperature                                    &
     &    (time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, surf_bcs%Tsf_bcs, &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%force_by_filter, iphys_LES%eflux_by_filter,        &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int, FEM_filters,         &
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%icomp_diff_base,  &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%rhs_mat, nod_fld, Csims_FEM_MHD%diff_coefs,       &
     &     v_sol)
      end if
!
!     ----- composition update
!
      if(MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(MHD_prop%ref_param_C%iflag_reference .ne. id_no_ref_temp)    &
     &   then
          if(iflag_debug.eq.1) write(*,*) 's_cal_light_element part'
          call s_cal_light_element(iphys%base%i_per_light, time_d%dt,   &
     &       FEM_prm, SGS_par, geofem%mesh, geofem%group,               &
     &       MHD_mesh%fluid, MHD_prop%cp_prop, MHD_prop%ref_param_C,    &
     &       nod_bcs%Cnod_bcs, surf_bcs%Csf_bcs, iphys, iphys_LES,      &
     &       SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,             &
     &       SGS_MHD_wk%fem_int, FEM_filters%FEM_elens,                 &
     &       Csims_FEM_MHD%icomp_sgs_term, Csims_FEM_MHD%iak_diff_base, &
     &       Csims_FEM_MHD%iak_diff_sgs,                                &
     &       Csims_FEM_MHD%iphys_elediff_vec,                           &
     &       Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%sgs_coefs_nod,  &
     &       Csims_FEM_MHD%diff_coefs,                                  &
     &       FEM_filters%filtering, SGS_MHD_wk%mk_MHD,                  &
     &       s_package%Cmatrix, ak_MHD, MGCG_WK, SGS_MHD_wk%FEM_SGS_wk, &
     &       SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, nod_fld,        &
     &       v_sol)
!
          call add_2_nod_scalars(nod_fld, iphys%base%i_ref_c,           &
     &        iphys%base%i_per_light, iphys%base%i_light)
        else
          if(iflag_debug.eq.1) write(*,*) 's_cal_light_element C'
          call s_cal_light_element(iphys%base%i_light, time_d%dt,       &
     &       FEM_prm, SGS_par, geofem%mesh, geofem%group,               &
     &       MHD_mesh%fluid, MHD_prop%cp_prop, MHD_prop%ref_param_C,    &
     &       nod_bcs%Cnod_bcs, surf_bcs%Csf_bcs, iphys, iphys_LES,      &
     &       SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,             &
     &       SGS_MHD_wk%fem_int, FEM_filters%FEM_elens,                 &
     &       Csims_FEM_MHD%icomp_sgs_term, Csims_FEM_MHD%iak_diff_base, &
     &       Csims_FEM_MHD%iak_diff_sgs,                                &
     &       Csims_FEM_MHD%iphys_elediff_vec,                           &
     &       Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%sgs_coefs_nod,  &
     &       Csims_FEM_MHD%diff_coefs,                                  &
     &       FEM_filters%filtering, SGS_MHD_wk%mk_MHD,                  &
     &       s_package%Cmatrix, ak_MHD, MGCG_WK, SGS_MHD_wk%FEM_SGS_wk, &
     &       SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, nod_fld,        &
     &       v_sol)
!
          if (iphys%base%i_per_light .gt. 0) then
            call subtract_2_nod_scalars(nod_fld, iphys%base%i_light,    &
     &          iphys%base%i_ref_c, iphys%base%i_per_light)
          end if
        end if
!
        call update_with_dummy_scalar                                   &
     &    (time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, surf_bcs%Csf_bcs, &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int, FEM_filters,         &
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%icomp_diff_base,  &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%rhs_mat, nod_fld, Csims_FEM_MHD%diff_coefs,       &
     &     v_sol)
      end if
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
     &      Csims_FEM_MHD%sgs_coefs_nod, fem_sq, v_sol)
        call update_with_velocity(time_d%i_time_step, time_d%dt,        &
     &     FEM_prm, SGS_par, geofem%mesh, geofem%group, MHD_mesh%fluid, &
     &     surf_bcs%Vsf_bcs, surf_bcs%Psf_bcs,                          &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%iphys_ele_fil, SGS_MHD_wk%fem_int, FEM_filters,   &
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%icomp_diff_base,  &
     &     Csims_FEM_MHD%iphys_elediff_vec,                             &
     &     Csims_FEM_MHD%iphys_elediff_fil,                             &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, nod_fld,          &
     &     SGS_MHD_wk%ele_fld, Csims_FEM_MHD%diff_coefs, v_sol)
      end if
!
      end subroutine fields_evolution
!
!-----------------------------------------------------------------------
!
      subroutine update_fields(time_d, FEM_prm, SGS_par,                &
     &         geofem, MHD_mesh, nod_bcs, surf_bcs, iphys, iphys_LES,   &
     &         FEM_filters, SGS_MHD_wk, nod_fld, Csims_FEM_MHD, v_sol)
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
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      if(iphys%base%i_velo .ne. 0) then
        call update_with_velocity(time_d%i_time_step, time_d%dt,        &
     &     FEM_prm, SGS_par, geofem%mesh, geofem%group, MHD_mesh%fluid, &
     &     surf_bcs%Vsf_bcs, surf_bcs%Psf_bcs,                          &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%iphys_ele_fil, SGS_MHD_wk%fem_int, FEM_filters,   &
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%icomp_diff_base,  &
     &     Csims_FEM_MHD%iphys_elediff_vec,                             &
     &     Csims_FEM_MHD%iphys_elediff_fil,                             &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, nod_fld,          &
     &     SGS_MHD_wk%ele_fld, Csims_FEM_MHD%diff_coefs, v_sol)
      end if
!
      if(iphys%base%i_temp .ne. 0) then
        call update_with_temperature                                    &
     &    (time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, surf_bcs%Tsf_bcs, &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%force_by_filter, iphys_LES%eflux_by_filter,        &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int, FEM_filters,         &
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%icomp_diff_base,  &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%rhs_mat, nod_fld, Csims_FEM_MHD%diff_coefs,       &
     &     v_sol)
      end if
!
      if(iphys%base%i_light .ne. 0) then
        call update_with_dummy_scalar                                   &
     &    (time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, surf_bcs%Csf_bcs, &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int, FEM_filters,         &
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%icomp_diff_base,  &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%rhs_mat, nod_fld, Csims_FEM_MHD%diff_coefs,       &
     &     v_sol)
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
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%icomp_diff_base,  &
     &     Csims_FEM_MHD%iphys_elediff_vec,                             &
     &     Csims_FEM_MHD%iphys_elediff_fil,                             &
     &     SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,                &
     &     SGS_MHD_wk%rhs_mat, nod_fld, SGS_MHD_wk%ele_fld,             &
     &     Csims_FEM_MHD%diff_coefs, v_sol)
      else if(iphys%base%i_magne.ne.0) then
        call update_with_magnetic_field                                 &
     &    (time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     geofem%mesh, geofem%group, MHD_mesh%fluid, MHD_mesh%conduct, &
     &     surf_bcs%Bsf_bcs, surf_bcs%Fsf_bcs,                          &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%iphys_ele_fil, SGS_MHD_wk%fem_int, FEM_filters,   &
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%icomp_diff_base,  &
     &     Csims_FEM_MHD%iphys_elediff_vec,                             &
     &     Csims_FEM_MHD%iphys_elediff_fil,                             &
     &     SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,                &
     &     SGS_MHD_wk%rhs_mat, nod_fld, SGS_MHD_wk%ele_fld,             &
     &     Csims_FEM_MHD%diff_coefs, v_sol)
      end if
!
      end subroutine update_fields
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fields_evo_for_FEM_SPH(time_d, FEM_prm, SGS_par,       &
     &          geofem, fluid, MHD_prop, nod_bcs, surf_bcs,             &
     &          iphys, iphys_LES, ak_MHD, FEM_filters,                  &
     &          s_package, MGCG_WK, SGS_MHD_wk, nod_fld,                &
     &          Csims_FEM_MHD, fem_sq, v_sol)
!
      use cal_temperature
      use cal_velocity
      use cal_light_element
      use copy_nodal_fields
!
      use update_with_scalars
      use update_with_velo
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
      type(field_geometry_data), intent(in) :: fluid
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
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
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      if (iflag_debug.eq.1) write(*,*) 'reset_update_flag'
      call reset_update_flag                                            &
     &   (nod_fld, Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%diff_coefs)
!
!     ---- temperature update
!
      if(MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        if(MHD_prop%ref_param_T%iflag_reference .ne. id_no_ref_temp)    &
     &   then
          if (iflag_debug.eq.1) write(*,*) 'cal_temperature_field'
          call cal_temperature_field(iphys%base%i_per_temp, time_d%dt,  &
     &        FEM_prm, SGS_par, geofem%mesh, geofem%group,              &
     &        fluid, MHD_prop%ht_prop, MHD_prop%ref_param_T,            &
     &        nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, iphys, iphys_LES,     &
     &        SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,            &
     &        SGS_MHD_wk%fem_int, FEM_filters%FEM_elens,                &
     &        Csims_FEM_MHD%icomp_sgs_term,                             &
     &        Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%iak_diff_sgs,  &
     &        Csims_FEM_MHD%iphys_elediff_vec, Csims_FEM_MHD%sgs_coefs, &
     &        Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,    &
     &        FEM_filters%filtering, SGS_MHD_wk%mk_MHD,                 &
     &        s_package%Tmatrix, ak_MHD, MGCG_WK,                       &
     &        SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,             &
     &        SGS_MHD_wk%rhs_mat, nod_fld, v_sol)
!
          call add_2_nod_scalars(nod_fld,                               &
     &        iphys%base%i_ref_t, iphys%base%i_per_temp,                &
     &        iphys%base%i_temp)
        else
          if (iflag_debug.eq.1) write(*,*) 'cal_temperature_field'
          call cal_temperature_field(iphys%base%i_temp, time_d%dt,      &
     &        FEM_prm, SGS_par, geofem%mesh, geofem%group,              &
     &        fluid, MHD_prop%ht_prop, MHD_prop%ref_param_T,            &
     &        nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, iphys, iphys_LES,     &
     &        SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,            &
     &        SGS_MHD_wk%fem_int, FEM_filters%FEM_elens,                &
     &        Csims_FEM_MHD%icomp_sgs_term,                             &
     &        Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%iak_diff_sgs,  &
     &        Csims_FEM_MHD%iphys_elediff_vec, Csims_FEM_MHD%sgs_coefs, &
     &        Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,    &
     &        FEM_filters%filtering, SGS_MHD_wk%mk_MHD,                 &
     &        s_package%Tmatrix, ak_MHD, MGCG_WK,                       &
     &        SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,             &
     &        SGS_MHD_wk%rhs_mat, nod_fld, v_sol)
!
          if (iphys%base%i_per_temp .gt. 0) then
            call subtract_2_nod_scalars(nod_fld, iphys%base%i_temp,     &
     &          iphys%base%i_ref_t, iphys%base%i_per_temp)
          end if
        end if
!
        call update_with_temperature                                    &
     &    (time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     geofem%mesh, geofem%group, fluid, surf_bcs%Tsf_bcs,          &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%force_by_filter, iphys_LES%eflux_by_filter,        &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int, FEM_filters,         &
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%icomp_diff_base,  &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%rhs_mat, nod_fld, Csims_FEM_MHD%diff_coefs,       &
     &     v_sol)
      end if
!
!     ----- composition update
!
      if (MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(MHD_prop%ref_param_C%iflag_reference .ne. id_no_ref_temp)    &
     &   then
          if(iflag_debug.eq.1) write(*,*) 's_cal_light_element part'
          call s_cal_light_element(iphys%base%i_per_light, time_d%dt,   &
     &       FEM_prm, SGS_par, geofem%mesh, geofem%group,               &
     &       fluid, MHD_prop%cp_prop, MHD_prop%ref_param_C,             &
     &       nod_bcs%Cnod_bcs, surf_bcs%Csf_bcs, iphys, iphys_LES,      &
     &       SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,             &
     &       SGS_MHD_wk%fem_int, FEM_filters%FEM_elens,                 &
     &       Csims_FEM_MHD%icomp_sgs_term, Csims_FEM_MHD%iak_diff_base, &
     &       Csims_FEM_MHD%iak_diff_sgs,                                &
     &       Csims_FEM_MHD%iphys_elediff_vec, Csims_FEM_MHD%sgs_coefs,  &
     &       Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,     &
     &       FEM_filters%filtering, SGS_MHD_wk%mk_MHD,                  &
     &       s_package%Cmatrix, ak_MHD, MGCG_WK, SGS_MHD_wk%FEM_SGS_wk, &
     &       SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, nod_fld, v_sol)
!
          call add_2_nod_scalars(nod_fld, iphys%base%i_ref_c,           &
     &        iphys%base%i_per_light, iphys%base%i_light)
        else
          if (iflag_debug.eq.1) write(*,*) 's_cal_light_element'
          call s_cal_light_element(iphys%base%i_light, time_d%dt,       &
     &       FEM_prm, SGS_par, geofem%mesh, geofem%group,               &
     &       fluid, MHD_prop%cp_prop, MHD_prop%ref_param_C,             &
     &       nod_bcs%Cnod_bcs, surf_bcs%Csf_bcs, iphys, iphys_LES,      &
     &       SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,             &
     &       SGS_MHD_wk%fem_int, FEM_filters%FEM_elens,                 &
     &       Csims_FEM_MHD%icomp_sgs_term, Csims_FEM_MHD%iak_diff_base, &
     &       Csims_FEM_MHD%iak_diff_sgs,                                &
     &       Csims_FEM_MHD%iphys_elediff_vec, Csims_FEM_MHD%sgs_coefs,  &
     &       Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,     &
     &       FEM_filters%filtering, SGS_MHD_wk%mk_MHD,                  &
     &       s_package%Cmatrix, ak_MHD, MGCG_WK, SGS_MHD_wk%FEM_SGS_wk, &
     &       SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, nod_fld, v_sol)
!
          if (iphys%base%i_per_light .gt. 0) then
            call subtract_2_nod_scalars(nod_fld, iphys%base%i_light,    &
     &          iphys%base%i_ref_c, iphys%base%i_per_light)
          end if
        end if
!
        call update_with_dummy_scalar                                   &
     &    (time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     geofem%mesh, geofem%group, fluid, surf_bcs%Csf_bcs,          &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int, FEM_filters,         &
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%icomp_diff_base,  &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%rhs_mat, nod_fld, Csims_FEM_MHD%diff_coefs,       &
     &     v_sol)
      end if
!
!     ---- velocity update
!
      if (MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'velocity_evolution'
        call velocity_evolution(time_d%time, time_d%dt,                 &
     &      FEM_prm, SGS_par, geofem%mesh, geofem%group, fluid,         &
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
     &      Csims_FEM_MHD%sgs_coefs, fem_sq, v_sol)
        call update_with_velocity(time_d%i_time_step, time_d%dt,        &
     &     FEM_prm, SGS_par, geofem%mesh, geofem%group, fluid,          &
     &     surf_bcs%Vsf_bcs, surf_bcs%Psf_bcs,                          &
     &     iphys%base, iphys_LES%filter_fld, iphys_LES%wide_filter_fld, &
     &     iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                 &
     &     SGS_MHD_wk%iphys_ele_fil, SGS_MHD_wk%fem_int, FEM_filters,   &
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%icomp_diff_base,  &
     &     Csims_FEM_MHD%iphys_elediff_vec,                             &
     &     Csims_FEM_MHD%iphys_elediff_fil,                             &
     &     SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                    &
     &     SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, nod_fld,          &
     &     SGS_MHD_wk%ele_fld, Csims_FEM_MHD%diff_coefs, v_sol)
      end if
!
      end subroutine fields_evo_for_FEM_SPH
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_perturbation_to_scalar(MHD_prop, iphys, nod_fld)
!
      use copy_nodal_fields
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if(MHD_prop%ref_param_T%iflag_reference .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1)  write(*,*) 'set_2_perturbation_temp'
        call subtract_2_nod_scalars(nod_fld, iphys%base%i_temp,         &
     &      iphys%base%i_ref_t, iphys%base%i_per_temp)
      end if
      if(MHD_prop%ref_param_C%iflag_reference .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1)  write(*,*) 'set_2_perturbation_comp'
        call subtract_2_nod_scalars                                     &
     &     (nod_fld, iphys%base%i_light, iphys%base%i_ref_c,            &
     &      iphys%base%i_per_light)
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
