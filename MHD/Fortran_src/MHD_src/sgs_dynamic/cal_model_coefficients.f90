!>@file   cal_model_coefficients.f90
!!        module cal_model_coefficients
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate model coefficients
!!
!!@verbatim
!!      subroutine s_cal_model_coefficients(time_d, FEM_prm, SGS_par,   &
!!     &          geofem, MHD_mesh, MHD_prop, nod_bcs, surf_bcs,        &
!!     &          iphys, iphys_LES, iak_sgs_term, icomp_sgs_term,       &
!!     &          iak_diff_sgs, icomp_diff_sgs,                         &
!!     &          iphys_elediff_vec, iphys_elediff_fil,                 &
!!     &          fem_int, FEM_filters, SGS_MHD_wk, nod_fld,            &
!!     &          sgs_coefs, sgs_coefs_nod, diff_coefs, m_SR)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(SGS_term_address), intent(in) :: iak_sgs_term
!!        type(SGS_term_address), intent(in) :: icomp_sgs_term
!!        type(SGS_term_address), intent(in) :: iak_diff_sgs
!!        type(SGS_term_address), intent(in) :: icomp_diff_sgs
!!        type(base_field_address), intent(in) :: iphys_elediff_vec
!!        type(base_field_address), intent(in) :: iphys_elediff_fil
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module cal_model_coefficients
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_control_parameter
      use t_physical_property
      use t_time_data
      use t_mesh_data
      use t_geometry_data_MHD
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_table_FEM_const
      use t_FEM_MHD_filter_data
      use t_MHD_mass_matrices
      use t_int_surface_data
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
      use t_bc_data_MHD
      use t_surface_bc_data_MHD
      use t_FEM_SGS_model_coefs
      use t_work_FEM_SGS_MHD
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
      subroutine s_cal_model_coefficients(time_d, FEM_prm, SGS_par,     &
     &          geofem, MHD_mesh, MHD_prop, nod_bcs, surf_bcs,          &
     &          iphys, iphys_LES, iak_sgs_term, icomp_sgs_term,         &
     &          iak_diff_sgs, icomp_diff_sgs,                           &
     &          iphys_elediff_vec, iphys_elediff_fil,                   &
     &          fem_int, FEM_filters, SGS_MHD_wk, nod_fld,              &
     &          sgs_coefs, sgs_coefs_nod, diff_coefs, m_SR)
!
      use cal_sgs_heat_flux_dynamic
      use cal_sgs_h_flux_dynamic_simi
      use cal_diff_coef_sgs_hf
      use cal_sgs_mom_flux_dynamic
      use cal_sgs_m_flux_dynamic_simi
      use cal_diff_coef_sgs_mf
      use cal_sgs_maxwell_dynamic
      use cal_diff_coef_sgs_mxwl
      use cal_sgs_induction_dynamic
      use cal_diff_coef_sgs_induct
      use cal_sgs_uxb_dynamic_simi
      use t_IO_step_parameter
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) ::   geofem
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(SGS_term_address), intent(in) :: iak_sgs_term
      type(SGS_term_address), intent(in) :: icomp_sgs_term
      type(SGS_term_address), intent(in) :: iak_diff_sgs
      type(SGS_term_address), intent(in) :: icomp_diff_sgs
      type(base_field_address), intent(in) :: iphys_elediff_vec
      type(base_field_address), intent(in) :: iphys_elediff_fil
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
!
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(SGS_par%model_p%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
      if(output_flag(time_d%i_time_step, SGS_par%i_step_sgs_coefs)      &
     &   .eqv. .FALSE.) return
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &        'set Csim', time_d%i_time_step, SGS_par%i_step_sgs_coefs
!
      if(MHD_prop%ht_prop%iflag_scheme .ne. id_no_evolution) then
        if(SGS_par%model_p%SGS_heat%iflag_SGS_flux                      &
     &       .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_sf_dynamic temp'
          call cal_sgs_sf_dynamic                                       &
     &       (FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int,        &
     &        time_d%dt, SGS_par%model_p%SGS_heat%itype_Csym_flux,      &
     &        SGS_par%model_p%SGS_heat%SGS_factor,                      &
     &        iphys_LES%SGS_wk%i_sgs_temp, iphys_LES%filter_fld%i_temp, &
     &        iphys%base%i_velo, iphys_LES%filter_fld%i_velo,           &
     &        iphys_LES%SGS_term%i_SGS_h_flux,                          &
     &        iak_sgs_term%i_SGS_h_flux, icomp_sgs_term%i_SGS_h_flux,   &
     &        SGS_par, geofem%mesh, iphys_LES%SGS_wk,                   &
     &        SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,            &
     &        MHD_mesh%fluid, fem_int, FEM_filters,                     &
     &        iphys_elediff_vec%i_velo, iphys_elediff_fil%i_velo,       &
     &        sgs_coefs_nod, SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,  &
     &        SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, nod_fld,       &
     &        sgs_coefs, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
        else if(SGS_par%model_p%SGS_heat%iflag_SGS_flux                 &
     &                        .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &          write(*,*) 's_cal_sgs_s_flux_dynamic_simi temp'
          call s_cal_sgs_s_flux_dynamic_simi(FEM_prm%npoint_t_evo_int,  &
     &        SGS_par%model_p%SGS_heat%itype_Csym_flux,                 &
     &        iphys_LES%SGS_wk%i_sgs_temp, iphys_LES%filter_fld%i_temp, &
     &        iphys_LES%wide_filter_fld%i_temp, iphys%base%i_velo,      &
     &        iphys_LES%filter_fld%i_velo,                              &
     &        iphys_LES%SGS_term%i_SGS_h_flux,                          &
     &        iak_sgs_term%i_SGS_h_flux, icomp_sgs_term%i_SGS_h_flux,   &
     &        SGS_par, geofem%mesh, iphys_LES%SGS_wk, fem_int,          &
     &        FEM_filters, SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%rhs_mat,   &
     &        nod_fld, sgs_coefs, sgs_coefs_nod,                        &
     &        m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        end if
!
        if(SGS_par%model_p%SGS_heat%iflag_commute_flux                  &
     &      .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_sf'
          call s_cal_diff_coef_sgs_sf                                   &
     &       (SGS_par%model_p%SGS_heat%itype_Csym_flux,                 &
     &        FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int,        &
     &        time_d%dt, iphys_LES%SGS_wk%i_sgs_temp,                   &
     &        iphys_LES%filter_fld%i_temp, iphys%base%i_velo,           &
     &        iphys_LES%filter_fld%i_velo,                              &
     &        iphys_LES%SGS_term%i_SGS_h_flux,                          &
     &        iak_diff_sgs%i_SGS_h_flux, icomp_sgs_term%i_SGS_h_flux,   &
     &        icomp_diff_sgs%i_SGS_h_flux, iphys_elediff_fil%i_velo,    &
     &        SGS_par, geofem%mesh, geofem%group,                       &
     &        nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, iphys_LES%SGS_wk,     &
     &        SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,            &
     &        MHD_mesh%fluid, fem_int, FEM_filters,                     &
     &        sgs_coefs, SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,      &
     &        SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, nod_fld,       &
     &        diff_coefs, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        end if
      end if
!
!
      if(MHD_prop%cp_prop%iflag_scheme .ne. id_no_evolution) then
        if(SGS_par%model_p%SGS_light%iflag_SGS_flux                     &
     &     .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_sf_dynamic comp'
          call cal_sgs_sf_dynamic                                       &
     &       (FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int,        &
     &        time_d%dt, SGS_par%model_p%SGS_light%itype_Csym_flux,     &
     &        SGS_par%model_p%SGS_light%SGS_factor,                     &
     &        iphys_LES%SGS_wk%i_sgs_composit,                          &
     &        iphys_LES%filter_fld%i_light,                             &
     &        iphys%base%i_velo, iphys_LES%filter_fld%i_velo,           &
     &        iphys_LES%SGS_term%i_SGS_c_flux,                          &
     &        iak_sgs_term%i_SGS_c_flux, icomp_sgs_term%i_SGS_c_flux,   &
     &        SGS_par, geofem%mesh, iphys_LES%SGS_wk,                   &
     &        SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,            &
     &        MHD_mesh%fluid, fem_int, FEM_filters,                     &
     &        iphys_elediff_vec%i_velo, iphys_elediff_fil%i_velo,       &
     &        sgs_coefs_nod, SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,  &
     &        SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, nod_fld,       &
     &        sgs_coefs, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
        else if(SGS_par%model_p%SGS_light%iflag_SGS_flux                &
     &                       .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &          write(*,*) 's_cal_sgs_s_flux_dynamic_simi comp'
          call s_cal_sgs_s_flux_dynamic_simi(FEM_prm%npoint_t_evo_int,  &
     &        SGS_par%model_p%SGS_light%itype_Csym_flux,                &
     &        iphys_LES%SGS_wk%i_sgs_composit,                          &
     &        iphys_LES%filter_fld%i_light,                             &
     &        iphys_LES%wide_filter_fld%i_light, iphys%base%i_velo,     &
     &        iphys_LES%filter_fld%i_velo,                              &
     &        iphys_LES%SGS_term%i_SGS_c_flux,                          &
     &        iak_sgs_term%i_SGS_c_flux, icomp_sgs_term%i_SGS_c_flux,   &
     &        SGS_par, geofem%mesh, iphys_LES%SGS_wk, fem_int,          &
     &        FEM_filters, SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%rhs_mat,   &
     &        nod_fld, sgs_coefs, sgs_coefs_nod,                        &
     &        m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        end if
!
        if(SGS_par%model_p%SGS_heat%iflag_commute_flux                  &
     &      .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_sf'
          call s_cal_diff_coef_sgs_sf                                   &
     &       (SGS_par%model_p%SGS_light%itype_Csym_flux,                &
     &        FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int,        &
     &        time_d%dt, iphys_LES%SGS_wk%i_sgs_composit,               &
     &        iphys_LES%filter_fld%i_light, iphys%base%i_velo,          &
     &        iphys_LES%filter_fld%i_velo,                              &
     &        iphys_LES%SGS_term%i_SGS_c_flux,                          &
     &        iak_diff_sgs%i_SGS_c_flux, icomp_sgs_term%i_SGS_c_flux,   &
     &        icomp_diff_sgs%i_SGS_c_flux, iphys_elediff_fil%i_velo,    &
     &        SGS_par, geofem%mesh, geofem%group,                       &
     &        nod_bcs%Tnod_bcs, surf_bcs%Tsf_bcs, iphys_LES%SGS_wk,     &
     &        SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,            &
     &        MHD_mesh%fluid, fem_int, FEM_filters,                     &
     &        sgs_coefs, SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,      &
     &        SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, nod_fld,       &
     &        diff_coefs, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        end if
      end if
!
      if(MHD_prop%fl_prop%iflag_scheme .ne. id_no_evolution) then
        if (SGS_par%model_p%SGS_momentum%iflag_SGS_flux                 &
     &      .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_m_flux_dynamic'
          call cal_sgs_m_flux_dynamic                                   &
     &       (time_d%dt, FEM_prm, SGS_par, geofem%mesh,                 &
     &        iphys%base, iphys_LES%filter_fld, iphys_LES%SGS_term,     &
     &        iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,              &
     &        SGS_MHD_wk%ele_fld, MHD_mesh%fluid, fem_int, FEM_filters, &
     &        iak_sgs_term%i_SGS_m_flux, icomp_sgs_term%i_SGS_m_flux,   &
     &        iphys_elediff_vec%i_velo, iphys_elediff_fil%i_velo,       &
     &        sgs_coefs_nod, SGS_MHD_wk%mk_MHD,                         &
     &        SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,             &
     &        SGS_MHD_wk%rhs_mat, nod_fld, sgs_coefs,                   &
     &        m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        else if(SGS_par%model_p%SGS_momentum%iflag_SGS_flux             &
     &                        .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 's_cal_sgs_m_flux_dynamic_simi'
          call s_cal_sgs_m_flux_dynamic_simi                            &
     &       (FEM_prm, SGS_par, geofem%mesh,                            &
     &        iphys%base, iphys_LES%filter_fld,                         &
     &        iphys_LES%wide_filter_fld, iphys_LES%SGS_term,            &
     &        iphys_LES%SGS_wk, fem_int, FEM_filters,                   &
     &        iak_sgs_term, icomp_sgs_term, SGS_MHD_wk%FEM_SGS_wk,      &
     &        SGS_MHD_wk%rhs_mat, nod_fld, sgs_coefs,                   &
     &        sgs_coefs_nod, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        end if
!
        if(SGS_par%model_p%SGS_momentum%iflag_commute_flux              &
     &      .eq. id_SGS_commute_ON) then
          if(iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_mf'
          call s_cal_diff_coef_sgs_mf                                   &
     &     (iak_diff_sgs%i_SGS_m_flux, icomp_sgs_term%i_SGS_m_flux,     &
     &      icomp_diff_sgs%i_SGS_m_flux, iphys_elediff_fil%i_velo,      &
     &      time_d%dt, FEM_prm, SGS_par, geofem%mesh, geofem%group,     &
     &      nod_bcs%Vnod_bcs, surf_bcs%Vsf_bcs, iphys%base,             &
     &      iphys_LES%filter_fld, iphys_LES%SGS_term, iphys_LES%SGS_wk, &
     &      SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,              &
     &      MHD_mesh%fluid, fem_int, FEM_filters, sgs_coefs,            &
     &      SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                   &
     &      SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat,                  &
     &      nod_fld, diff_coefs, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        end if
      end if
!
!
      if(MHD_prop%fl_prop%iflag_4_lorentz) then
        if(SGS_par%model_p%iflag_SGS_lorentz .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)                                         &
     &       write(*,*) 'cal_sgs_maxwell_t_dynamic'
          call cal_sgs_maxwell_t_dynamic                                &
     &      (time_d%dt, FEM_prm, SGS_par, geofem%mesh,                  &
     &       iphys%base, iphys_LES%filter_fld, iphys_LES%SGS_term,      &
     &       iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,               &
     &       SGS_MHD_wk%ele_fld, MHD_mesh%fluid, fem_int, FEM_filters,  &
     &       iak_sgs_term%i_SGS_Lorentz, icomp_sgs_term%i_SGS_Lorentz,  &
     &       iphys_elediff_vec%i_magne, iphys_elediff_fil%i_magne,      &
     &       sgs_coefs_nod, SGS_MHD_wk%mk_MHD,                          &
     &       SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,              &
     &       SGS_MHD_wk%rhs_mat, nod_fld, sgs_coefs,                    &
     &       m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        else if(SGS_par%model_p%iflag_SGS_lorentz                       &
     &                        .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &       write(*,*) 'cal_sgs_maxwell_dynamic_simi'
          call cal_sgs_maxwell_dynamic_simi                             &
     &      (FEM_prm, SGS_par, geofem%mesh, iphys%base,                 &
     &       iphys_LES%filter_fld, iphys_LES%wide_filter_fld,           &
     &       iphys_LES%SGS_term, iphys_LES%SGS_wk,                      &
     &       fem_int, FEM_filters, iak_sgs_term, icomp_sgs_term,        &
     &       SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%rhs_mat, nod_fld,        &
     &       sgs_coefs, sgs_coefs_nod, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        end if
!
        if(SGS_par%commute_p%iflag_c_lorentz .eq. id_SGS_commute_ON)    &
     &   then
          if (iflag_debug.eq.1) write(*,*) 's_cal_diff_coef_sgs_mxwl'
          call s_cal_diff_coef_sgs_mxwl                                 &
     &     (time_d%dt, FEM_prm, SGS_par, geofem%mesh, geofem%group,     &
     &      MHD_mesh%fluid, nod_bcs%Vnod_bcs, surf_bcs%Bsf_bcs,         &
     &      iphys%base, iphys_LES%filter_fld, iphys_LES%SGS_term,       &
     &      iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,                &
     &      SGS_MHD_wk%ele_fld, fem_int, FEM_filters,                   &
     &      iak_diff_sgs%i_SGS_Lorentz, icomp_diff_sgs%i_SGS_Lorentz,   &
     &      icomp_sgs_term%i_SGS_Lorentz, iphys_elediff_fil%i_magne,    &
     &      sgs_coefs, SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,        &
     &      SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, nod_fld,         &
     &      diff_coefs, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        end if
      end if
!
!
!
      if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if(SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'cal_sgs_induct_t_dynamic'
          call cal_sgs_induct_t_dynamic                                 &
     &      (time_d%dt, FEM_prm, SGS_par, geofem%mesh,                  &
     &       iphys%base, iphys_LES%filter_fld, iphys_LES%SGS_term,      &
     &       iphys_LES%SGS_wk, SGS_MHD_wk%iphys_ele_base,               &
     &       SGS_MHD_wk%ele_fld, MHD_mesh%conduct, MHD_prop%cd_prop,    &
     &       fem_int, FEM_filters, iak_sgs_term%i_SGS_induction,        &
     &       icomp_sgs_term%i_SGS_induction,                            &
     &       iphys_elediff_vec, iphys_elediff_fil,                      &
     &       sgs_coefs_nod, SGS_MHD_wk%mk_MHD,                          &
     &       SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,              &
     &       SGS_MHD_wk%rhs_mat, nod_fld, sgs_coefs,                    &
     &       m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        else if(SGS_par%model_p%iflag_SGS_uxb                           &
     &                            .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'cal_sgs_induct_t_dynamic_simi'
          call cal_sgs_induct_t_dynamic_simi                            &
     &       (FEM_prm, SGS_par, geofem%mesh, iphys%base,                &
     &        iphys_LES%filter_fld, iphys_LES%wide_filter_fld,          &
     &        iphys_LES%SGS_term, iphys_LES%SGS_wk,                     &
     &        fem_int, FEM_filters, iak_sgs_term%i_SGS_induction,       &
     &        icomp_sgs_term%i_SGS_induction, SGS_MHD_wk%FEM_SGS_wk,    &
     &        SGS_MHD_wk%rhs_mat, nod_fld, sgs_coefs, sgs_coefs_nod,    &
     &        m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        end if
!
        if(SGS_par%commute_p%iflag_c_uxb .eq. id_SGS_commute_ON) then
          if(iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_induct'
          call s_cal_diff_coef_sgs_induct                               &
     &      (icomp_sgs_term%i_SGS_induction,                            &
     &       icomp_diff_sgs%i_SGS_induction, iphys_elediff_fil,         &
     &       time_d%dt, FEM_prm, SGS_par, geofem%mesh, geofem%group,    &
     &       MHD_mesh%fluid, MHD_mesh%conduct, MHD_prop%cd_prop,        &
     &       surf_bcs%Bsf_bcs, iphys%base, iphys_LES%filter_fld,        &
     &       iphys_LES%SGS_term, iphys_LES%SGS_wk,                      &
     &       SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,             &
     &       fem_int, sgs_coefs, FEM_filters, SGS_MHD_wk%mk_MHD,        &
     &       SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,              &
     &       SGS_MHD_wk%rhs_mat, nod_fld, diff_coefs%Cdiff_SGS_uxb,     &
     &       m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        end if
!
      else if(MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution)  &
     &  then
!
        if(SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_uxb_dynamic'
          call cal_sgs_uxb_dynamic                                      &
     &       (time_d%dt, FEM_prm, SGS_par, geofem%mesh,                 &
     &        iphys%base, iphys_LES%filter_fld,                         &
     &        iphys_LES%SGS_term, iphys_LES%SGS_wk,                     &
     &        SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,            &
     &        MHD_mesh%conduct, MHD_prop%cd_prop, fem_int, FEM_filters, &
     &        iak_sgs_term%i_SGS_induction,                             &
     &        icomp_sgs_term%i_SGS_induction, iphys_elediff_vec%i_velo, &
     &        iphys_elediff_fil%i_velo, SGS_MHD_wk%mk_MHD,              &
     &        SGS_MHD_wk%FEM_SGS_wk, SGS_MHD_wk%mhd_fem_wk,             &
     &        SGS_MHD_wk%rhs_mat, nod_fld, sgs_coefs,                   &
     &        m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        else if(SGS_par%model_p%iflag_SGS_uxb                           &
     &                         .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)  write(*,*)                             &
     &                          's_cal_sgs_uxb_dynamic_simi'
          call s_cal_sgs_uxb_dynamic_simi                               &
     &       (FEM_prm, SGS_par, geofem%mesh, iphys%base,                &
     &        iphys_LES%filter_fld, iphys_LES%wide_filter_fld,          &
     &        iphys_LES%SGS_wk, fem_int, FEM_filters,                   &
     &        iak_sgs_term%i_SGS_induction,                             &
     &        icomp_sgs_term%i_SGS_induction, SGS_MHD_wk%FEM_SGS_wk,    &
     &        nod_fld, sgs_coefs, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        end if
      end if
!
      end subroutine s_cal_model_coefficients
!
!-----------------------------------------------------------------------
!
      end module cal_model_coefficients
