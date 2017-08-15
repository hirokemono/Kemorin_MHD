!FEM_analyzer_snap_tmp.f90
!      module FEM_analyzer_snap_tmp
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_initialize_snap_tmp                              &
!!     &         (MHD_files, bc_FEM_IO, MHD_step, femmesh, ele_mesh,    &
!!     &          FEM_filters, SGS_MHD_wk, range, fem_ucd, fem_sq)
!!        type(IO_boundary), intent(in) :: bc_FEM_IO
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(mesh_data), intent(inout) :: femmesh
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(filters_on_FEM), intent(inout) :: FEM_filters
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(maximum_informations), intent(inout) :: range
!!        type(ucd_file_data), intent(inout) :: fem_ucd
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!      subroutine FEM_analyze_snap_tmp                                 &
!!     &         (i_step, MHD_files, femmesh, ele_mesh, FEM_filters,    &
!!     &          MHD_step, visval, SGS_MHD_wk, fem_ucd, fem_sq)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(VIZ_step_params), intent(inout) :: MHD_step
!!      subroutine FEM_finalize_snap_tmp                                &
!!     &         (MHD_files, MHD_step, range, fem_ucd)
!
      module FEM_analyzer_snap_tmp
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
      use m_control_parameter
      use m_SGS_control_parameter
      use t_mesh_data
      use t_ucd_file
      use t_time_data
      use t_IO_step_parameter
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_ucd_file
      use t_cal_max_indices
      use t_FEM_MHD_mean_square
      use t_FEM_MHD_filter_data
      use t_work_FEM_SGS_MHD
!
      use calypso_mpi
!
      implicit none
!
      type(time_data), save, private :: SNAP_time_IO
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine FEM_initialize_snap_tmp                                &
     &         (MHD_files, bc_FEM_IO, MHD_step, femmesh, ele_mesh,      &
     &          FEM_filters, SGS_MHD_wk, range, fem_ucd, fem_sq)
!
      use m_node_phys_data
      use m_geometry_data_MHD
      use m_physical_property
      use m_element_phys_data
      use m_bc_data_velo
      use m_bc_data_list
      use m_SGS_control_parameter
      use m_finite_element_matrix
      use t_boundary_field_IO
!
      use initialize_4_snapshot
      use FEM_MHD_ucd_data
!
      use node_monitor_IO
      use open_sgs_model_coefs
!
      type(IO_boundary), intent(in) :: bc_FEM_IO
      type(MHD_file_IO_params), intent(inout) :: MHD_files
!
      type(mesh_data), intent(inout) :: femmesh
      type(element_geometry), intent(inout) :: ele_mesh
      type(filters_on_FEM), intent(inout) :: FEM_filters
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(maximum_informations), intent(inout) :: range
      type(ucd_file_data), intent(inout) :: fem_ucd
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'init_analyzer_snap'
      call init_analyzer_snap                                           &
     &   (MHD_files%fst_file_IO, FEM_prm1, SGS_par1, bc_FEM_IO,         &
     &    MHD_step, femmesh%mesh, femmesh%group, ele_mesh, MHD_mesh1,   &
     &    FEM_filters, MHD_prop1, ak_MHD, MHD_BC1, FEM_MHD1_BCs,        &
     &    Csims_FEM_MHD1, iphys_nod1, nod_fld1, iphys_ele, ele_fld1,    &
     &    SNAP_time_IO, MHD_step%rst_step, fem_int1, mk_MHD1,           &
     &    SGS_MHD_wk, fem_sq, label_sim)
!
      call output_grd_file_w_org_connect                                &
     &   (MHD_step%ucd_step, femmesh%mesh, MHD_mesh1, nod_fld1,         &
     &    MHD_files%ucd_file_IO, fem_ucd)
!
      call alloc_phys_range(nod_fld1%ntot_phys_viz, range)
!
      end subroutine FEM_initialize_snap_tmp
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_snap_tmp                                   &
     &         (i_step, MHD_files, femmesh, ele_mesh, FEM_filters,      &
     &          MHD_step, visval, SGS_MHD_wk, fem_ucd, fem_sq)
!
      use m_physical_property
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
      use m_finite_element_matrix
      use m_bc_data_velo
      use m_flexible_time_step
      use m_fem_mhd_restart
!
      use input_control
      use nod_phys_send_recv
      use lead_physical_values
      use update_after_evolution
      use FEM_MHD_evolution
      use cal_model_coefficients
      use chenge_step_4_dynamic
      use copy_nodal_fields
!
      use time_step_data_IO_control
      use node_monitor_IO
      use sgs_model_coefs_IO
      use output_viz_file_control
!
      use check_deltat_by_prev_rms
      use output_viz_file_control
!
      integer(kind=kint ), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(filters_on_FEM), intent(in) :: FEM_filters
!
      integer(kind=kint ), intent(inout) :: visval
      type(MHD_step_param), intent(inout) :: MHD_step
!
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(ucd_file_data), intent(inout) :: fem_ucd
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      integer(kind = kint) :: iflag
!
!     ---- Load field data --- 
!
      call reset_update_flag(nod_fld1,                                  &
     &    Csims_FEM_MHD1%sgs_coefs, Csims_FEM_MHD1%diff_coefs)
      flex_p1%istep_max_dt = i_step
      if (my_rank.eq.0) write(*,*) 'step: ', flex_p1%istep_max_dt
!
      if (MHD_step%rst_step%increment .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'input_restart_4_snapshot'
        call input_restart_4_snapshot                                   &
     &     (flex_p1%istep_max_dt, MHD_files%fst_file_IO,                &
     &      femmesh%mesh%node, nod_fld1, SNAP_time_IO,                  &
     &      MHD_step%rst_step)
!
      else if (MHD_step%ucd_step%increment .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'read_udt_4_snap'
        call read_udt_4_snap                                            &
     &     (flex_p1%istep_max_dt, MHD_files%org_ucd_file_IO,            &
     &      nod_fld1, SNAP_time_IO, MHD_step%ucd_step)
!
        MHD_step%time_d%time = MHD_step%init_d%time                     &
     &             + MHD_step%time_d%dt * dble(flex_p1%istep_max_dt)
        MHD_step%time_d%i_time_step = flex_p1%istep_max_dt
      end if
!
!     ---- magnetic field update
!
      if (MHD_prop1%ref_param_T%iflag_reference                         &
     & .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1)  write(*,*) 'set_2_perturbation_temp'
        call subtract_2_nod_scalars(nod_fld1,                           &
     &      iphys_nod1%i_temp, iphys_nod1%i_ref_t,                      &
     &      iphys_nod1%i_par_temp)
      end if
      if (MHD_prop1%ref_param_C%iflag_reference                         &
     & .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1)  write(*,*) 'set_2_perturbation_comp'
        call subtract_2_nod_scalars(nod_fld1,                           &
     &      iphys_nod1%i_light, iphys_nod1%i_ref_c,                     &
     &      iphys_nod1%i_par_light)
      end if
!
!     ---------------------
!
      if (iflag_debug.eq.1)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(femmesh%mesh, nod_fld1)
!
      if (iflag_debug .eq. 1)  write(*,*) 'update_FEM_fields'
      call update_FEM_fields(MHD_step%time_d,                           &
     &    FEM_prm1, SGS_par1, femmesh, ele_mesh, MHD_mesh1,             &
     &    FEM_MHD1_BCs%nod_bcs, FEM_MHD1_BCs%surf_bcs,                  &
     &    iphys_nod1, iphys_ele, fem_int1, FEM_filters, mk_MHD1,        &
     &    SGS_MHD_wk, nod_fld1, ele_fld1, Csims_FEM_MHD1)
!
!     ----- Evaluate model coefficients
!
      if (SGS_par1%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*) 's_cal_model_coefficients'
        call s_cal_model_coefficients                                   &
     &     (MHD_step%time_d, FEM_prm1, SGS_par1,                        &
     &      femmesh, ele_mesh, MHD_mesh1, MHD_prop1,                    &
     &      FEM_MHD1_BCs%nod_bcs, FEM_MHD1_BCs%surf_bcs,                &
     &      iphys_nod1, iphys_ele, ele_fld1, fem_int1, FEM_filters,     &
     &      mk_MHD1, SGS_MHD_wk, nod_fld1, Csims_FEM_MHD1)
      end if
!
!     ========  Data output
!
      iflag = lead_field_data_flag(flex_p1%istep_max_dt, MHD_step)
      if(iflag .eq. 0) then
        call lead_fields_by_FEM                                         &
     &     (MHD_step%time_d, FEM_prm1, SGS_par1, femmesh,               &
     &      ele_mesh, MHD_mesh1, MHD_prop1, FEM_MHD1_BCs,               &
     &      iphys_nod1, iphys_ele, ak_MHD, fem_int1, FEM_filters,       &
     &      mk_MHD1, SGS_MHD_wk, nod_fld1, ele_fld1, Csims_FEM_MHD1)
      end if
!
      if (iflag_debug.eq.1)  write(*,*) 'lead_specital_SGS'
      call lead_specital_SGS(MHD_step, FEM_prm1, SGS_par1,              &
     &    femmesh%mesh, ele_mesh, femmesh%group, MHD_mesh1, MHD_prop1,  &
     &    FEM_MHD1_BCs%surf_bcs, iphys_nod1, iphys_ele, ak_MHD,         &
     &    fem_int1, FEM_filters%FEM_elens, FEM_filters%filtering,       &
     &    Csims_FEM_MHD1, mk_MHD1, SGS_MHD_wk%FEM_SGS_wk,               &
     &    SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat,                    &
     &    nod_fld1, ele_fld1)
!
!     -----Output monitor date
!
      iflag = output_IO_flag(flex_p1%istep_max_dt, MHD_step%rms_step)
      if(iflag .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'output_time_step_control'
        call output_time_step_control                                   &
     &     (FEM_prm1, MHD_step%time_d, femmesh%mesh, MHD_mesh1,         &
     &      MHD_prop1%fl_prop, MHD_prop1%cd_prop, iphys_nod1,           &
     &      nod_fld1, iphys_ele, ele_fld1, fem_int1%jcs,                &
     &      fem_sq%i_rms, fem_sq%j_ave, fem_sq%i_msq,                   &
     &      SGS_MHD_wk%rhs_mat, SGS_MHD_wk%mhd_fem_wk, fem_sq%msq)
      end if
!
      iflag = output_IO_flag(flex_p1%istep_max_dt, MHD_step%point_step)
      if(iflag .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'output_monitor_control'
        call output_monitor_control                                     &
     &     (MHD_step%time_d, femmesh%mesh%node, nod_fld1)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 's_output_sgs_model_coefs'
      call s_output_sgs_model_coefs(flex_p1%istep_max_dt,               &
     &    MHD_step, SGS_par1, MHD_prop1%cd_prop,                        &
     &    SGS_MHD_wk%FEM_SGS_wk)
!
!     ---- Output voulme field data
!
      if (iflag_debug.eq.1) write(*,*) 's_output_ucd_file_control'
      call s_output_ucd_file_control                                    &
     &   (MHD_files%ucd_file_IO, flex_p1%istep_max_dt,                  &
     &    MHD_step%time_d, MHD_step%ucd_step, fem_ucd)
!
!     ----
!
      if     (flex_p1%iflag_flexible_step .eq. iflag_flex_step) then
        visval = viz_file_step_4_flex(MHD_step%time_d,                  &
     &                                MHD_step%viz_step)
      else
        visval = viz_file_step_4_fix(flex_p1%istep_max_dt,              &
     &                               MHD_step%viz_step)
      end if
!
      end subroutine FEM_analyze_snap_tmp
!
! ----------------------------------------------------------------------
!
      subroutine FEM_finalize_snap_tmp                                  &
     &         (MHD_files, MHD_step, range, fem_ucd)
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
      type(maximum_informations), intent(inout) :: range
      type(ucd_file_data), intent(inout) :: fem_ucd
!
!
      if(MHD_step%ucd_step%increment .gt. 0) then
        call finalize_output_ucd(MHD_files%ucd_file_IO, fem_ucd)
        call dealloc_phys_range(range)
      end if
!      call close_boundary_monitor(my_rank)
!
      end subroutine FEM_finalize_snap_tmp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine lead_specital_SGS                                      &
     &       (MHD_step, FEM_prm, SGS_par, mesh, ele_mesh, group,        &
     &        MHD_mesh, MHD_prop, sf_bcs, iphys, iphys_ele, ak_MHD,     &
     &        fem_int, FEM_elens, filtering, Csims_FEM_MHD, mk_MHD,     &
     &        FEM_SGS_wk, mhd_fem_wk, rhs_mat, nod_fld, ele_fld)
!
      use copy_nodal_fields
      use cvt_sph_vector_2_xyz_smp
      use cvt_xyz_vector_2_sph_smp
      use cvt_sph_tensor_2_xyz_smp
      use cvt_xyz_tensor_2_sph_smp
      use coordinate_convert_4_sph
!
      use products_nodal_fields_smp
      use cal_momentum_terms
      use cal_sgs_4_monitor
      use int_sgs_induction
!
      type(MHD_step_param), intent(in) :: MHD_step
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(mesh_groups), intent(in) ::   group
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(surface_boundarty_conditions), intent(in) :: sf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
!
!
!$omp parallel
      call overwrite_nodal_xyz_2_sph_smp                                &
     &   (mesh%node, nod_fld%ntot_phys,                                 &
     &    iphys%i_SGS_m_flux, n_sym_tensor, nod_fld%d_fld)
!$omp end parallel
!
      call clear_field_data(nod_fld, n_sym_tensor, iphys%i_SGS_m_flux)
!
!$omp parallel
      call overwrite_nodal_sph_2_xyz_smp                                &
     &   (mesh%node, nod_fld%ntot_phys,                                 &
     &    iphys%i_SGS_m_flux, n_sym_tensor, nod_fld%d_fld)
!$omp end parallel
!
      if (iphys%i_SGS_div_m_flux .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead radial', trim(fhd_div_SGS_m_flux)
        call cal_terms_4_momentum(iphys%i_SGS_div_m_flux,               &
     &      Csims_FEM_MHD%ifld_diff%i_mom_flux,                         &
     &      Csims_FEM_MHD%ifld_diff%i_lorentz, MHD_step%time_d%dt,      &
     &      FEM_prm, SGS_par%model_p, SGS_par%commute_p,                &
     &      mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,          &
     &      group%surf_grp, MHD_mesh%fluid,                             &
     &      MHD_prop%fl_prop, MHD_prop%cd_prop,                         &
     &      sf_bcs%Vsf_bcs, sf_bcs%Bsf_bcs, iphys, iphys_ele, ak_MHD,   &
     &      fem_int, FEM_elens, Csims_FEM_MHD%diff_coefs,               &
     &      mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld, ele_fld)
      end if
!
!$omp parallel
      if (iphys%i_reynolds_wk .gt. 0) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_velo, iphys%i_SGS_div_m_flux, iphys%i_reynolds_wk,  &
     &      nod_fld)
      end if
!
      call overwrite_nodal_xyz_2_sph_smp                                &
     &   (mesh%node, nod_fld%ntot_phys,                                 &
     &    iphys%i_velo, n_vector, nod_fld%d_fld)
!$omp end parallel

      call clear_field_data(nod_fld, n_vector, iphys%i_velo)
!
!$omp parallel
      call overwrite_nodal_sph_2_xyz_smp                                &
     &   (mesh%node, nod_fld%ntot_phys,                                 &
     &    iphys%i_velo, n_vector, nod_fld%d_fld)
!$omp end parallel
!
      if (iphys%i_SGS_vp_induct .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_SGS_vp_induct)
        call cal_sgs_uxb_2_monitor                                      &
     &     (Csims_FEM_MHD%icomp_sgs%i_induction,                        &
     &      Csims_FEM_MHD%iphys_elediff%i_velo, MHD_step%time_d%dt,     &
     &      FEM_prm, SGS_par%model_p, SGS_par%filter_p,                 &
     &      mesh%nod_comm, mesh%node, mesh%ele,                         &
     &      MHD_mesh%conduct, MHD_prop%cd_prop,                         &
     &      iphys, iphys_ele, ele_fld, fem_int%jcs%jac_3d,              &
     &      fem_int%rhs_tbl, FEM_elens, filtering,                      &
     &      Csims_FEM_MHD%sgs_coefs, mk_MHD%mlump_cd,                   &
     &      FEM_SGS_wk%wk_filter, mhd_fem_wk, rhs_mat%fem_wk,           &
     &      rhs_mat%f_l, rhs_mat%f_nl, nod_fld)

      end if
!
      if (iphys%i_SGS_induction .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_SGS_induction)
        call int_vol_sgs_induction(FEM_prm,                             &
     &      mesh%nod_comm, mesh%node, mesh%ele, MHD_mesh%conduct,       &
     &      iphys, fem_int%jcs%jac_3d, fem_int%rhs_tbl,                 &
     &      mk_MHD%mlump_cd, mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_nl,  &
     &      nod_fld)
      end if
!
!$omp parallel
      if (iphys%i_SGS_me_gen .gt. 0) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_magne, iphys%i_SGS_induction, iphys%i_SGS_me_gen,   &
     &      nod_fld)
      end if
!$omp end parallel
!
      end subroutine lead_specital_SGS
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_snap_tmp
