!>@file   update_after_evolution.f90
!!        module update_after_evolution
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine fields_evolution
!!     &         (time_d, FEM_prm, SGS_par, femmesh, ele_mesh,          &
!!     &          MHD_mesh, MHD_prop, nod_bcs, surf_bcs,                &
!!     &          iphys, iphys_ele, ak_MHD, fem_int, FEM_elens,         &
!!     &          filtering, wide_filtering, layer_tbl, mk_MHD,         &
!!     &          s_package, MGCG_WK, wk_cor, wk_lsq, wk_sgs, wk_diff,  &
!!     &          wk_filter,  mhd_fem_wk, rhs_mat, nod_fld, ele_fld,    &
!!     &          Csims_FEM_MHD, fem_sq)
!!      subroutine update_fields(time_d, FEM_prm, SGS_par,              &
!!     &          femmesh, ele_mesh, MHD_mesh, nod_bcs, surf_bcs,       &
!!     &          iphys, iphys_ele, fem_int, FEM_elens,                 &
!!     &          filtering, wide_filtering, layer_tbl, mk_MHD,         &
!!     &          wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk,       &
!!     &          rhs_mat, nod_fld, ele_fld, Csims_FEM_MHD)
!!      subroutine reset_update_flag(nod_fld, sgs_coefs, diff_coefs)
!!
!!      subroutine fields_evolution_4_FEM_SPH                           &
!!     &         (time_d, FEM_prm, SGS_par, femmesh, ele_mesh,          &
!!     &          fluid, MHD_prop, nod_bcs, surf_bcs, iphys, iphys_ele, &
!!     &          ak_MHD, fem_int, FEM_elens, filtering, wide_filtering,&
!!     &          layer_tbl, mk_MHD, s_package, MGCG_WK, wk_cor, wk_lsq,&
!!     &          wk_sgs, wk_diff, wk_filter, mhd_fem_wk, rhs_mat,      &
!!     &          nod_fld, ele_fld, Csims_FEM_MHD, fem_sq)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: femmesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(MHD_matrices_pack), intent(in) :: s_package
!!        type(MGCG_data), intent(inout) :: MGCG_WK
!!        type(dynamis_correlation_data), intent(inout) :: wk_cor
!!        type(dynamis_least_suare_data), intent(inout) :: wk_lsq
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!!        type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!@endverbatim
!
      module update_after_evolution
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
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_filtering_data
      use t_material_property
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_work_layer_correlate
      use t_layering_ele_list
      use t_MHD_boundary_data
      use t_bc_data_MHD
      use t_MHD_matrices_pack
      use t_MGCG_data
      use t_MHD_finite_element_mat
      use t_MHD_mass_matricxes
      use t_FEM_SGS_model_coefs
      use t_work_FEM_integration
      use t_FEM_MHD_mean_square
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
     &         (time_d, FEM_prm, SGS_par, femmesh, ele_mesh,            &
     &          MHD_mesh, MHD_prop, nod_bcs, surf_bcs,                  &
     &          iphys, iphys_ele, ak_MHD, fem_int, FEM_elens,           &
     &          filtering, wide_filtering, layer_tbl, mk_MHD,           &
     &          s_package, MGCG_WK, wk_cor, wk_lsq, wk_sgs, wk_diff,    &
     &          wk_filter,  mhd_fem_wk, rhs_mat, nod_fld, ele_fld,      &
     &          Csims_FEM_MHD, fem_sq)
!
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
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
      type(layering_tbl), intent(in) :: layer_tbl
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
      type(MHD_matrices_pack), intent(in) :: s_package
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
!
      if (iflag_debug.eq.1) write(*,*) 'reset_update_flag'
      call reset_update_flag                                            &
     &   (nod_fld, Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%diff_coefs)
!
!     ---- magnetic field update
!
      if(MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'cal_magne_vector_potential'
        call cal_vector_potential(time_d%dt, FEM_prm, SGS_par,          &
     &     femmesh%mesh, femmesh%group, ele_mesh%surf,                  &
     &     MHD_mesh%conduct, MHD_prop%cd_prop,                          &
     &     nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Fsf_bcs, iphys, &
     &     iphys_ele, ele_fld, fem_int%jcs, fem_int%rhs_tbl,            &
     &     FEM_elens, Csims_FEM_MHD%icomp_sgs, Csims_FEM_MHD%ifld_diff, &
     &     Csims_FEM_MHD%iphys_elediff, Csims_FEM_MHD%sgs_coefs,        &
     &     Csims_FEM_MHD%diff_coefs, filtering, fem_int%m_lump,         &
     &     mk_MHD%mlump_cd, s_package%Bmatrix, s_package%Fmatrix,       &
     &     ak_MHD%ak_d_magne, MGCG_WK, wk_filter, mhd_fem_wk,           &
     &     rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl,  &
     &     fem_sq, nod_fld)
        call update_with_vector_potential                               &
     &    (Csims_FEM_MHD%ifld_diff%i_magne,                             &
     &     Csims_FEM_MHD%icomp_diff%i_magne,                            &
     &     Csims_FEM_MHD%iphys_elediff%i_magne,                         &
     &     Csims_FEM_MHD%iphys_elediff%i_filter_magne,                  &
     &     time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     femmesh%mesh, femmesh%group, ele_mesh%surf,                  &
     &     MHD_mesh%fluid, MHD_mesh%conduct, layer_tbl,                 &
     &     nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Fsf_bcs,        &
     &     iphys, iphys_ele, fem_int%jcs, fem_int%rhs_tbl, FEM_elens,   &
     &     filtering, wide_filtering, fem_int%m_lump,                   &
     &     wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk,              &
     &     rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl,  &
     &     nod_fld, ele_fld, Csims_FEM_MHD%diff_coefs)
!
      else if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution)  &
     &     then
!
!        call check_surface_param_smp('cal_magnetic_field start',       &
!     &      my_rank, sf_grp, femmesh%group%surf_nod_grp)
        if (iflag_debug.eq.1) write(*,*) 's_cal_magnetic_field'
        call s_cal_magnetic_field                                       &
     &    (time_d%dt, FEM_prm, SGS_par, femmesh%mesh, femmesh%group,    &
     &     ele_mesh%surf, MHD_mesh%conduct, MHD_prop%cd_prop,           &
     &     nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Bsf_bcs,        &
     &     surf_bcs%Fsf_bcs, iphys, iphys_ele, ele_fld,                 &
     &     fem_int%jcs, fem_int%rhs_tbl, FEM_elens,                     &
     &     Csims_FEM_MHD%icomp_sgs, Csims_FEM_MHD%ifld_diff,            &
     &     Csims_FEM_MHD%iphys_elediff, Csims_FEM_MHD%sgs_coefs,        &
     &     Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,       &
     &     filtering, fem_int%m_lump, mk_MHD%mlump_cd,                  &
     &     s_package%Bmatrix, s_package%Fmatrix, ak_MHD%ak_d_magne,     &
     &     MGCG_WK, wk_filter, mhd_fem_wk, rhs_mat%fem_wk,              &
     &     rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl, fem_sq, nod_fld)
        call update_with_magnetic_field                                 &
     &    (Csims_FEM_MHD%ifld_diff%i_magne,                             &
     &     Csims_FEM_MHD%icomp_diff%i_magne,                            &
     &     Csims_FEM_MHD%iphys_elediff%i_magne,                         &
     &     Csims_FEM_MHD%iphys_elediff%i_filter_magne,                  &
     &     time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     femmesh%mesh, femmesh%group, ele_mesh%surf,                  &
     &     MHD_mesh%fluid, MHD_mesh%conduct, layer_tbl,                 &
     &     surf_bcs%Bsf_bcs, surf_bcs%Fsf_bcs, iphys, iphys_ele,        &
     &     fem_int%jcs, fem_int%rhs_tbl, FEM_elens,                     &
     &     filtering, wide_filtering, fem_int%m_lump,                   &
     &     wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk,              &
     &     rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl,  &
     &     nod_fld, ele_fld, Csims_FEM_MHD%diff_coefs)
      end if
!
!     ---- temperature update
!
      if(MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        if(MHD_prop%ref_param_T%iflag_reference .ne. id_no_ref_temp)    &
     &   then
          if(iflag_debug.eq.1) write(*,*) 'cal_temperature_field theta'
          call cal_temperature_field                                    &
     &      (iphys%i_par_temp, time_d%dt, FEM_prm,                      &
     &       SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,      &
     &       femmesh%mesh, femmesh%group, ele_mesh%surf,                &
     &       MHD_mesh%fluid, MHD_prop%ht_prop, MHD_prop%ref_param_T,    &
     &       nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs,                        &
     &       iphys, iphys_ele, ele_fld, fem_int%jcs, fem_int%rhs_tbl,   &
     &       FEM_elens, Csims_FEM_MHD%icomp_sgs,                        &
     &       Csims_FEM_MHD%ifld_diff, Csims_FEM_MHD%iphys_elediff,      &
     &       Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod,      &
     &       Csims_FEM_MHD%diff_coefs, filtering, mk_MHD%mlump_fl,      &
     &       s_package%Tmatrix, ak_MHD%ak_d_temp, MGCG_WK,              &
     &       wk_filter, mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%surf_wk,    &
     &       rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
          call add_2_nod_scalars(nod_fld,                               &
     &        iphys%i_ref_t, iphys%i_par_temp, iphys%i_temp)
        else
!          call check_surface_param_smp('cal_temperature_field start',  &
!     &        my_rank, sf_grp, femmesh%group%surf_nod_grp)
          if (iflag_debug.eq.1) write(*,*) 'cal_temperature_field T'
          call cal_temperature_field(iphys%i_temp, time_d%dt, FEM_prm,  &
     &        SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,     &
     &        femmesh%mesh, femmesh%group, ele_mesh%surf,               &
     &        MHD_mesh%fluid, MHD_prop%ht_prop, MHD_prop%ref_param_T,   &
     &        nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs,                       &
     &        iphys, iphys_ele, ele_fld, fem_int%jcs, fem_int%rhs_tbl,  &
     &        FEM_elens, Csims_FEM_MHD%icomp_sgs,                       &
     &        Csims_FEM_MHD%ifld_diff, Csims_FEM_MHD%iphys_elediff,     &
     &        Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod,     &
     &        Csims_FEM_MHD%diff_coefs, filtering, mk_MHD%mlump_fl,     &
     &        s_package%Tmatrix, ak_MHD%ak_d_temp, MGCG_WK,             &
     &        wk_filter, mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%surf_wk,   &
     &        rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
        if (iphys%i_par_temp .gt. 0) then
          call subtract_2_nod_scalars(nod_fld,                          &
     &        iphys%i_temp, iphys%i_ref_t, iphys%i_par_temp)
        end if
      end if
!
        call update_with_temperature                                    &
     &     (Csims_FEM_MHD%ifld_diff%i_temp,                             &
     &      Csims_FEM_MHD%icomp_diff%i_temp,                            &
     &      time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,            &
     &      femmesh%mesh, femmesh%group, ele_mesh%surf,                 &
     &      MHD_mesh%fluid, surf_bcs%Tsf_bcs,                           &
     &      iphys, iphys_ele, ele_fld, fem_int%jcs,                     &
     &      fem_int%rhs_tbl, FEM_elens, filtering, wide_filtering,      &
     &      layer_tbl, mk_MHD%mlump_fl, wk_cor, wk_lsq, wk_diff,        &
     &      wk_filter, rhs_mat%fem_wk, rhs_mat%surf_wk,                 &
     &      rhs_mat%f_l, rhs_mat%f_nl, nod_fld,                         &
     &      Csims_FEM_MHD%diff_coefs)
      end if
!
!     ----- composition update
!
      if(MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(MHD_prop%ref_param_C%iflag_reference .ne. id_no_ref_temp)    &
     &   then
          if(iflag_debug.eq.1) write(*,*) 's_cal_light_element part'
          call s_cal_light_element                                      &
     &       (iphys%i_par_light, time_d%dt, FEM_prm,                    &
     &        SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,     &
     &        femmesh%mesh, femmesh%group, ele_mesh%surf,               &
     &        MHD_mesh%fluid, MHD_prop%cp_prop, MHD_prop%ref_param_C,   &
     &        nod_bcs%Cnod_bcs, surf_bcs%Csf_bcs,                       &
     &        iphys, iphys_ele, ele_fld, fem_int%jcs, fem_int%rhs_tbl,  &
     &        FEM_elens, Csims_FEM_MHD%icomp_sgs,                       &
     &        Csims_FEM_MHD%ifld_diff, Csims_FEM_MHD%iphys_elediff,     &
     &        Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod,     &
     &        Csims_FEM_MHD%diff_coefs, filtering, mk_MHD%mlump_fl,     &
     &        s_package%Cmatrix, ak_MHD%ak_d_composit, MGCG_WK,         &
     &        wk_filter, mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%surf_wk,   &
     &        rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
          call add_2_nod_scalars(nod_fld,                               &
     &        iphys%i_ref_c, iphys%i_par_light, iphys%i_light)
        else
          if(iflag_debug.eq.1) write(*,*) 's_cal_light_element C'
          call s_cal_light_element(iphys%i_light, time_d%dt, FEM_prm,   &
     &        SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,     &
     &        femmesh%mesh, femmesh%group, ele_mesh%surf,               &
     &        MHD_mesh%fluid, MHD_prop%cp_prop, MHD_prop%ref_param_C,   &
     &        nod_bcs%Cnod_bcs, surf_bcs%Csf_bcs,                       &
     &        iphys, iphys_ele, ele_fld, fem_int%jcs, fem_int%rhs_tbl,  &
     &        FEM_elens, Csims_FEM_MHD%icomp_sgs,                       &
     &        Csims_FEM_MHD%ifld_diff, Csims_FEM_MHD%iphys_elediff,     &
     &        Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod,     &
     &        Csims_FEM_MHD%diff_coefs, filtering, mk_MHD%mlump_fl,     &
     &        s_package%Cmatrix, ak_MHD%ak_d_composit, MGCG_WK,         &
     &        wk_filter, mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%surf_wk,   &
     &        rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
          if (iphys%i_par_light .gt. 0) then
            call subtract_2_nod_scalars(nod_fld,                        &
     &          iphys%i_light, iphys%i_ref_c, iphys%i_par_light)
          end if
        end if
!
        call update_with_dummy_scalar                                   &
     &     (Csims_FEM_MHD%ifld_diff%i_light,                            &
     &      Csims_FEM_MHD%icomp_diff%i_light,                           &
     &      time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,            &
     &      femmesh%mesh, femmesh%group, ele_mesh%surf,                 &
     &      MHD_mesh%fluid, surf_bcs%Csf_bcs, iphys,                    &
     &      iphys_ele, ele_fld, fem_int%jcs, fem_int%rhs_tbl,           &
     &      FEM_elens, filtering, wide_filtering, layer_tbl,            &
     &      mk_MHD%mlump_fl, wk_cor, wk_lsq, wk_diff, wk_filter,        &
     &      rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl, &
     &      nod_fld, Csims_FEM_MHD%diff_coefs)
      end if
!
!     ---- velocity update
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug.eq.1) write(*,*) 'velocity_evolution'
        call velocity_evolution                                         &
     &     (time_d%time, time_d%dt, FEM_prm, SGS_par,                   &
     &      femmesh%mesh, femmesh%group, ele_mesh%surf,                 &
     &      MHD_mesh%fluid,  MHD_prop%fl_prop, MHD_prop%cd_prop,        &
     &      nod_bcs%Vnod_bcs, surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs,       &
     &      surf_bcs%Psf_bcs, iphys, iphys_ele, ak_MHD, fem_int,        &
     &      FEM_elens, Csims_FEM_MHD%ifld_sgs,                          &
     &      Csims_FEM_MHD%icomp_sgs, Csims_FEM_MHD%ifld_diff,           &
     &      Csims_FEM_MHD%iphys_elediff, Csims_FEM_MHD%sgs_coefs_nod,   &
     &      Csims_FEM_MHD%diff_coefs, filtering, layer_tbl,             &
     &      mk_MHD%mlump_fl, s_package%Vmatrix, s_package%Pmatrix,      &
     &      MGCG_WK, wk_lsq, wk_sgs, wk_filter, mhd_fem_wk, rhs_mat,    &
     &      nod_fld, ele_fld, Csims_FEM_MHD%sgs_coefs, fem_sq)
        call update_with_velocity                                       &
     &     (Csims_FEM_MHD%ifld_diff%i_velo,                             &
     &      Csims_FEM_MHD%icomp_diff%i_velo,                            &
     &      Csims_FEM_MHD%iphys_elediff%i_velo,                         &
     &      Csims_FEM_MHD%iphys_elediff%i_filter_velo,                  &
     &      time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,            &
     &      femmesh%mesh, femmesh%group, ele_mesh%surf,                 &
     &      MHD_mesh%fluid, surf_bcs%Vsf_bcs, surf_bcs%Psf_bcs,         &
     &      iphys, iphys_ele, fem_int%jcs, fem_int%rhs_tbl, FEM_elens,  &
     &      filtering, wide_filtering, layer_tbl, mk_MHD%mlump_fl,      &
     &      wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk,             &
     &      rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl, &
     &      nod_fld, ele_fld, Csims_FEM_MHD%diff_coefs)
      end if
!
      end subroutine fields_evolution
!
!-----------------------------------------------------------------------
!
      subroutine update_fields(time_d, FEM_prm, SGS_par,                &
     &          femmesh, ele_mesh, MHD_mesh, nod_bcs, surf_bcs,         &
     &          iphys, iphys_ele, fem_int, FEM_elens,                   &
     &          filtering, wide_filtering, layer_tbl, mk_MHD,           &
     &          wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk,         &
     &          rhs_mat, nod_fld, ele_fld, Csims_FEM_MHD)
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
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
      type(layering_tbl), intent(in) :: layer_tbl
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!
!
      if (iphys%i_velo .ne. 0) then
        call update_with_velocity                                       &
     &     (Csims_FEM_MHD%ifld_diff%i_velo,                             &
     &      Csims_FEM_MHD%icomp_diff%i_velo,                            &
     &      Csims_FEM_MHD%iphys_elediff%i_velo,                         &
     &      Csims_FEM_MHD%iphys_elediff%i_filter_velo,                  &
     &      time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,            &
     &      femmesh%mesh, femmesh%group, ele_mesh%surf,                 &
     &      MHD_mesh%fluid, surf_bcs%Vsf_bcs,                           &
     &      surf_bcs%Psf_bcs, iphys, iphys_ele, fem_int%jcs,            &
     &      fem_int%rhs_tbl, FEM_elens, filtering, wide_filtering,      &
     &      layer_tbl, mk_MHD%mlump_fl, wk_cor, wk_lsq, wk_diff,        &
     &      wk_filter, mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%surf_wk,     &
     &      rhs_mat%f_l, rhs_mat%f_nl, nod_fld, ele_fld,                &
     &      Csims_FEM_MHD%diff_coefs)
      end if
!
      if (iphys%i_temp .ne. 0) then
        call update_with_temperature                                    &
     &     (Csims_FEM_MHD%ifld_diff%i_temp,                             &
     &      Csims_FEM_MHD%icomp_diff%i_temp,                            &
     &      time_d%i_time_step, time_d%dt,                              &
     &      FEM_prm, SGS_par, femmesh%mesh, femmesh%group,              &
     &      ele_mesh%surf, MHD_mesh%fluid,                              &
     &      surf_bcs%Tsf_bcs, iphys, iphys_ele, ele_fld,                &
     &      fem_int%jcs, fem_int%rhs_tbl, FEM_elens,                    &
     &      filtering, wide_filtering, layer_tbl, mk_MHD%mlump_fl,      &
     &      wk_cor, wk_lsq, wk_diff, wk_filter,                         &
     &      rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl, &
     &      nod_fld, Csims_FEM_MHD%diff_coefs)
      end if
!
      if (iphys%i_light .ne. 0) then
        call update_with_dummy_scalar                                   &
     &     (Csims_FEM_MHD%ifld_diff%i_light,                            &
     &      Csims_FEM_MHD%icomp_diff%i_light,                           &
     &      time_d%i_time_step, time_d%dt,                              &
     &      FEM_prm, SGS_par, femmesh%mesh, femmesh%group,              &
     &      ele_mesh%surf, MHD_mesh%fluid,                              &
     &      surf_bcs%Csf_bcs, iphys, iphys_ele, ele_fld,                &
     &      fem_int%jcs, fem_int%rhs_tbl, FEM_elens,                    &
     &      filtering, wide_filtering, layer_tbl, mk_MHD%mlump_fl,      &
     &      wk_cor, wk_lsq, wk_diff, wk_filter,                         &
     &      rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl, &
     &      nod_fld, Csims_FEM_MHD%diff_coefs)
      end if
!
      if (iphys%i_vecp .ne. 0) then
        call update_with_vector_potential                               &
     &    (Csims_FEM_MHD%ifld_diff%i_magne,                             &
     &     Csims_FEM_MHD%icomp_diff%i_magne,                            &
     &     Csims_FEM_MHD%iphys_elediff%i_magne,                         &
     &     Csims_FEM_MHD%iphys_elediff%i_filter_magne,                  &
     &     time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     femmesh%mesh, femmesh%group, ele_mesh%surf,                  &
     &     MHD_mesh%fluid, MHD_mesh%conduct, layer_tbl,                 &
     &     nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Fsf_bcs,        &
     &     iphys, iphys_ele, fem_int%jcs, fem_int%rhs_tbl,              &
     &     FEM_elens, filtering, wide_filtering, fem_int%m_lump,        &
     &     wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk,              &
     &     rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl,  &
     &     nod_fld, ele_fld, Csims_FEM_MHD%diff_coefs)
      else if (iphys%i_magne.ne.0) then
        call update_with_magnetic_field                                 &
     &    (Csims_FEM_MHD%ifld_diff%i_magne,                             &
     &     Csims_FEM_MHD%icomp_diff%i_magne,                            &
     &     Csims_FEM_MHD%iphys_elediff%i_magne,                         &
     &     Csims_FEM_MHD%iphys_elediff%i_filter_magne,                  &
     &     time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     femmesh%mesh, femmesh%group, ele_mesh%surf,                  &
     &     MHD_mesh%fluid, MHD_mesh%conduct, layer_tbl,                 &
     &     surf_bcs%Bsf_bcs, surf_bcs%Fsf_bcs, iphys, iphys_ele,        &
     &     fem_int%jcs, fem_int%rhs_tbl, FEM_elens,                     &
     &     filtering, wide_filtering, fem_int%m_lump,                   &
     &     wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk,              &
     &     rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl,  &
     &     nod_fld, ele_fld, Csims_FEM_MHD%diff_coefs)
      end if
!
      end subroutine update_fields
!
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
!-----------------------------------------------------------------------
!
      subroutine fields_evolution_4_FEM_SPH                             &
     &         (time_d, FEM_prm, SGS_par, femmesh, ele_mesh,            &
     &          fluid, MHD_prop, nod_bcs, surf_bcs, iphys, iphys_ele,   &
     &          ak_MHD, fem_int, FEM_elens, filtering, wide_filtering,  &
     &          layer_tbl, mk_MHD, s_package, MGCG_WK, wk_cor, wk_lsq,  &
     &          wk_sgs, wk_diff, wk_filter, mhd_fem_wk, rhs_mat,        &
     &          nod_fld, ele_fld, Csims_FEM_MHD, fem_sq)
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
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(field_geometry_data), intent(in) :: fluid
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
      type(layering_tbl), intent(in) :: layer_tbl
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
      type(MHD_matrices_pack), intent(in) :: s_package
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
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
          call cal_temperature_field                                    &
     &       (iphys%i_par_temp, time_d%dt, FEM_prm,                     &
     &        SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,     &
     &        femmesh%mesh, femmesh%group, ele_mesh%surf, fluid,        &
     &        MHD_prop%ht_prop, MHD_prop%ref_param_T,                   &
     &        nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, iphys, iphys_ele,     &
     &        ele_fld, fem_int%jcs, fem_int%rhs_tbl, FEM_elens,         &
     &        Csims_FEM_MHD%icomp_sgs, Csims_FEM_MHD%ifld_diff,         &
     &        Csims_FEM_MHD%iphys_elediff, Csims_FEM_MHD%sgs_coefs,     &
     &        Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,    &
     &        filtering, mk_MHD%mlump_fl, s_package%Tmatrix,            &
     &        ak_MHD%ak_d_temp, MGCG_WK, wk_filter, mhd_fem_wk,         &
     &        rhs_mat%fem_wk, rhs_mat%surf_wk,                          &
     &        rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
          call add_2_nod_scalars(nod_fld,                               &
     &        iphys%i_ref_t, iphys%i_par_temp, iphys%i_temp)
        else
          if (iflag_debug.eq.1) write(*,*) 'cal_temperature_field'
          call cal_temperature_field(iphys%i_temp, time_d%dt, FEM_prm,  &
     &        SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,     &
     &        femmesh%mesh, femmesh%group, ele_mesh%surf, fluid,        &
     &        MHD_prop%ht_prop, MHD_prop%ref_param_T,                   &
     &        nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, iphys, iphys_ele,     &
     &        ele_fld, fem_int%jcs, fem_int%rhs_tbl, FEM_elens,         &
     &        Csims_FEM_MHD%icomp_sgs, Csims_FEM_MHD%ifld_diff,         &
     &        Csims_FEM_MHD%iphys_elediff, Csims_FEM_MHD%sgs_coefs,     &
     &        Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,    &
     &        filtering, mk_MHD%mlump_fl, s_package%Tmatrix,            &
     &        ak_MHD%ak_d_temp, MGCG_WK, wk_filter, mhd_fem_wk,         &
     &        rhs_mat%fem_wk, rhs_mat%surf_wk,                          &
     &        rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
          if (iphys%i_par_temp .gt. 0) then
            call subtract_2_nod_scalars(nod_fld,                        &
     &          iphys%i_temp, iphys%i_ref_t, iphys%i_par_temp)
          end if
        end if
!
        call update_with_temperature                                    &
     &     (Csims_FEM_MHD%ifld_diff%i_temp,                             &
     &      Csims_FEM_MHD%icomp_diff%i_temp,                            &
     &      time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,            &
     &      femmesh%mesh, femmesh%group, ele_mesh%surf,                 &
     &      fluid, surf_bcs%Tsf_bcs,                                    &
     &      iphys, iphys_ele, ele_fld, fem_int%jcs,                     &
     &      fem_int%rhs_tbl, FEM_elens, filtering, wide_filtering,      &
     &      layer_tbl, mk_MHD%mlump_fl, wk_cor, wk_lsq, wk_diff,        &
     &      wk_filter, rhs_mat%fem_wk, rhs_mat%surf_wk,                 &
     &      rhs_mat%f_l, rhs_mat%f_nl, nod_fld,                         &
     &      Csims_FEM_MHD%diff_coefs)
      end if
!
!     ----- composition update
!
      if (MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(MHD_prop%ref_param_C%iflag_reference .ne. id_no_ref_temp)    &
     &   then
          if(iflag_debug.eq.1) write(*,*) 's_cal_light_element part'
          call s_cal_light_element                                      &
     &       (iphys%i_par_light, time_d%dt, FEM_prm,                    &
     &        SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,     &
     &        femmesh%mesh, femmesh%group, ele_mesh%surf, fluid,        &
     &        MHD_prop%cp_prop, MHD_prop%ref_param_C,                   &
     &        nod_bcs%Cnod_bcs, surf_bcs%Csf_bcs, iphys, iphys_ele,     &
     &        ele_fld, fem_int%jcs, fem_int%rhs_tbl, FEM_elens,         &
     &        Csims_FEM_MHD%icomp_sgs, Csims_FEM_MHD%ifld_diff,         &
     &        Csims_FEM_MHD%iphys_elediff, Csims_FEM_MHD%sgs_coefs,     &
     &        Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,    &
     &        filtering, mk_MHD%mlump_fl, s_package%Cmatrix,            &
     &        ak_MHD%ak_d_composit, MGCG_WK, wk_filter, mhd_fem_wk,     &
     &        rhs_mat%fem_wk, rhs_mat%surf_wk,                          &
     &        rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
          call add_2_nod_scalars(nod_fld,                               &
     &        iphys%i_ref_c, iphys%i_par_light, iphys%i_light)
        else
          if (iflag_debug.eq.1) write(*,*) 's_cal_light_element'
          call s_cal_light_element(iphys%i_light, time_d%dt, FEM_prm,   &
     &        SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,     &
     &        femmesh%mesh, femmesh%group, ele_mesh%surf, fluid,        &
     &        MHD_prop%cp_prop, MHD_prop%ref_param_C,                   &
     &        nod_bcs%Cnod_bcs, surf_bcs%Csf_bcs, iphys, iphys_ele,     &
     &        ele_fld, fem_int%jcs, fem_int%rhs_tbl, FEM_elens,         &
     &        Csims_FEM_MHD%icomp_sgs, Csims_FEM_MHD%ifld_diff,         &
     &        Csims_FEM_MHD%iphys_elediff, Csims_FEM_MHD%sgs_coefs,     &
     &        Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,    &
     &        filtering, mk_MHD%mlump_fl, s_package%Cmatrix,            &
     &        ak_MHD%ak_d_composit, MGCG_WK, wk_filter, mhd_fem_wk,     &
     &        rhs_mat%fem_wk, rhs_mat%surf_wk,                          &
     &        rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
          if (iphys%i_par_light .gt. 0) then
            call subtract_2_nod_scalars(nod_fld,                        &
     &          iphys%i_light, iphys%i_ref_c, iphys%i_par_light)
          end if
        end if
!
        call update_with_dummy_scalar                                   &
     &    (Csims_FEM_MHD%ifld_diff%i_light,                             &
     &     Csims_FEM_MHD%icomp_diff%i_light,                            &
     &     time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,             &
     &     femmesh%mesh, femmesh%group, ele_mesh%surf,                  &
     &     fluid, surf_bcs%Csf_bcs, iphys,                              &
     &     iphys_ele, ele_fld, fem_int%jcs, fem_int%rhs_tbl,            &
     &     FEM_elens, filtering, wide_filtering, layer_tbl,             &
     &     mk_MHD%mlump_fl, wk_cor, wk_lsq, wk_diff, wk_filter,         &
     &     rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl,  &
     &     nod_fld, Csims_FEM_MHD%diff_coefs)
      end if
!
!     ---- velocity update
!
      if (MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'velocity_evolution'
        call velocity_evolution                                         &
     &     (time_d%time, time_d%dt, FEM_prm, SGS_par,                   &
     &      femmesh%mesh, femmesh%group, ele_mesh%surf, fluid,          &
     &      MHD_prop%fl_prop, MHD_prop%cd_prop, nod_bcs%Vnod_bcs,       &
     &      surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs, surf_bcs%Psf_bcs,       &
     &      iphys, iphys_ele, ak_MHD, fem_int, FEM_elens,               &
     &      Csims_FEM_MHD%ifld_sgs, Csims_FEM_MHD%icomp_sgs,            &
     &      Csims_FEM_MHD%ifld_diff, Csims_FEM_MHD%iphys_elediff,       &
     &      Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,      &
     &      filtering, layer_tbl, mk_MHD%mlump_fl,                      &
     &      s_package%Vmatrix, s_package%Pmatrix,                       &
     &      MGCG_WK, wk_lsq, wk_sgs, wk_filter, mhd_fem_wk, rhs_mat,    &
     &      nod_fld, ele_fld, Csims_FEM_MHD%sgs_coefs, fem_sq)
        call update_with_velocity                                       &
     &     (Csims_FEM_MHD%ifld_diff%i_velo,                             &
     &      Csims_FEM_MHD%icomp_diff%i_velo,                            &
     &      Csims_FEM_MHD%iphys_elediff%i_velo,                         &
     &      Csims_FEM_MHD%iphys_elediff%i_filter_velo,                  &
     &      time_d%i_time_step, time_d%dt, FEM_prm, SGS_par,            &
     &      femmesh%mesh, femmesh%group, ele_mesh%surf, fluid,          &
     &      surf_bcs%Vsf_bcs, surf_bcs%Psf_bcs, iphys, iphys_ele,       &
     &      fem_int%jcs, fem_int%rhs_tbl, FEM_elens,                    &
     &      filtering, wide_filtering, layer_tbl, mk_MHD%mlump_fl,      &
     &      wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk,             &
     &      rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl, &
     &      nod_fld, ele_fld, Csims_FEM_MHD%diff_coefs)
      end if
!
      end subroutine fields_evolution_4_FEM_SPH
!
!-----------------------------------------------------------------------
!
      end module update_after_evolution
