!FEM_analyzer_snap_tmp.f90
!      module FEM_analyzer_snap_tmp
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_initialize_snap_tmp(MHD_files, MHD_step, femmesh,&
!!     &          iphys_nod, nod_fld, FEM_model, ak_MHD, FEM_SGS,       &
!!     &          SGS_MHD_wk, MHD_IO, fem_sq, label_sim)
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(mesh_data), intent(inout) :: femmesh
!!        type(FEM_SGS_structure), intent(inout) :: FEM_SGS
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!      subroutine FEM_analyze_snap_tmp(i_step, MHD_files, FEM_model,   &
!!     &          femmesh, iphys_nod, ak_MHD, MHD_step, visval, FEM_SGS,&
!!     &          SGS_MHD_wk, nod_fld, MHD_IO, fem_sq)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(VIZ_step_params), intent(inout) :: MHD_step
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!      subroutine FEM_finalize_snap_tmp(MHD_files, MHD_step, MHD_IO)
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!
      module FEM_analyzer_snap_tmp
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_FEM_MHD_model_data
      use t_control_parameter
      use t_material_property
      use t_ucd_file
      use t_IO_step_parameter
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_MHD_IO_data
      use t_cal_max_indices
      use t_FEM_MHD_mean_square
      use t_FEM_SGS_structure
      use t_work_FEM_SGS_MHD
!
      use calypso_mpi
!
      implicit none
!
      type(time_data), save, private :: SNAP_time_IO
!
       private :: lead_specital_SGS
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine FEM_initialize_snap_tmp(MHD_files, MHD_step, femmesh,  &
     &          iphys_nod, nod_fld, FEM_model, ak_MHD, FEM_SGS,         &
     &          SGS_MHD_wk, MHD_IO, fem_sq, label_sim)
!
      use t_boundary_field_IO
!
      use initialize_4_snapshot
      use FEM_MHD_ucd_data
!
      use node_monitor_IO
      use open_sgs_model_coefs
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
!
      type(mesh_data), intent(inout) :: femmesh
      type(phys_address), intent(inout) :: iphys_nod
      type(phys_data), intent(inout) :: nod_fld
      type(FEM_MHD_model_data), intent(inout) :: FEM_model
      type(coefs_4_MHD_type), intent(inout) :: ak_MHD
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      character(len=kchara), intent(inout)   :: label_sim
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'init_analyzer_snap'
      call init_analyzer_snap(MHD_files,                                &
     &   FEM_model%FEM_prm, FEM_SGS%SGS_par, FEM_model%bc_FEM_IO,       &
     &   MHD_step, femmesh%mesh, femmesh%group,                         &
     &   FEM_model%MHD_mesh, FEM_SGS%FEM_filters, FEM_model%MHD_prop,   &
     &   ak_MHD, FEM_model%MHD_BC, FEM_model%FEM_MHD_BCs,               &
     &   FEM_SGS%Csims, iphys_nod, nod_fld, SNAP_time_IO,               &
     &   MHD_step%rst_step, SGS_MHD_wk, fem_sq, MHD_IO%rst_IO,          &
     &   label_sim)
!
      call output_grd_file_w_org_connect                                &
     &   (MHD_step%ucd_step, femmesh%mesh, FEM_model%MHD_mesh, nod_fld, &
     &    MHD_files%ucd_file_IO, MHD_IO%fem_ucd)
!
      call alloc_phys_range(nod_fld%ntot_phys_viz, MHD_IO%range)
!
      end subroutine FEM_initialize_snap_tmp
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_snap_tmp(i_step, MHD_files, FEM_model,     &
     &          femmesh, iphys_nod, ak_MHD, MHD_step, visval, FEM_SGS,  &
     &          SGS_MHD_wk, nod_fld, MHD_IO, fem_sq)
!
      use m_fem_mhd_restart
      use t_FEM_MHD_mean_square
!
      use input_control
      use nod_phys_send_recv
      use lead_physical_values
      use update_after_evolution
      use FEM_MHD_evolution
      use chenge_step_4_dynamic
      use copy_nodal_fields
!
      use node_monitor_IO
      use FEM_sgs_model_coefs_IO
      use output_viz_file_control
!
      use check_deltat_by_prev_rms
      use output_viz_file_control
!
      integer(kind=kint ), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(mesh_data), intent(in) :: femmesh
      type(phys_address), intent(in) :: iphys_nod
      type(FEM_MHD_model_data), intent(in) :: FEM_model
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
!
      integer(kind=kint ), intent(inout) :: visval
      type(MHD_step_param), intent(inout) :: MHD_step
!
      type(phys_data), intent(inout) :: nod_fld
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
!     ---- Load field data --- 
!
      call reset_update_flag                                            &
     &   (nod_fld, FEM_SGS%Csims%sgs_coefs, FEM_SGS%Csims%diff_coefs)
      MHD_step%flex_p%istep_max_dt = i_step
      if (my_rank.eq.0) write(*,*)                                      &
     &        'step: ', MHD_step%flex_p%istep_max_dt
!
      if (MHD_step%rst_step%increment .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'input_restart_4_snapshot'
        call input_restart_4_snapshot                                   &
     &     (MHD_step%flex_p%istep_max_dt, MHD_files%fst_file_IO,        &
     &      femmesh%mesh%node, nod_fld, SNAP_time_IO,                   &
     &      MHD_step%rst_step, MHD_IO%rst_IO)
!
      else if (MHD_step%ucd_step%increment .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'read_udt_4_snap'
        call read_udt_4_snap                                            &
     &     (MHD_step%flex_p%istep_max_dt, MHD_files%org_ucd_file_IO,    &
     &      nod_fld, SNAP_time_IO, MHD_step%ucd_step)
!
        MHD_step%time_d%time = MHD_step%init_d%time                     &
     &       + MHD_step%time_d%dt * dble(MHD_step%flex_p%istep_max_dt)
        MHD_step%time_d%i_time_step = MHD_step%flex_p%istep_max_dt
      end if
!
!     ---------------------
!
      call set_perturbation_to_scalar                                   &
     &   (FEM_model%MHD_prop, iphys_nod, nod_fld)
!
!     ---------------------
!
      if (iflag_debug.eq.1)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(femmesh%mesh, nod_fld)
!
      if (iflag_debug .eq. 1)  write(*,*) 'update_FEM_fields'
      call update_FEM_fields                                            &
     &   (MHD_step%time_d, FEM_model%FEM_prm, FEM_SGS%SGS_par,          &
     &    femmesh, FEM_model%MHD_mesh, FEM_model%FEM_MHD_BCs,           &
     &    iphys_nod, FEM_SGS%FEM_filters, SGS_MHD_wk,                   &
     &    nod_fld, FEM_SGS%Csims)
!
!     ----- Evaluate model coefficients
!
      call cal_FEM_model_coefficients                                   &
     &   (MHD_step%time_d, FEM_model%FEM_prm, FEM_SGS%SGS_par,          &
     &    femmesh, FEM_model%MHD_mesh, FEM_model%MHD_prop,              &
     &    FEM_model%FEM_MHD_BCs, iphys_nod, FEM_SGS%FEM_filters,        &
     &    SGS_MHD_wk, nod_fld, FEM_SGS%Csims)
!
!     ========  Data output
!
      call lead_fields_by_FEM(MHD_step%flex_p%istep_max_dt,             &
     &    MHD_step, FEM_model%FEM_prm, FEM_SGS%SGS_par,                 &
     &    femmesh, FEM_model%MHD_mesh, FEM_model%MHD_prop,              &
     &    FEM_model%FEM_MHD_BCs, iphys_nod, ak_MHD,                     &
     &    FEM_SGS%FEM_filters, SGS_MHD_wk, nod_fld, FEM_SGS%Csims)
!
      if (iflag_debug.eq.1)  write(*,*) 'lead_specital_SGS'
      call lead_specital_SGS                                            &
     &  (MHD_step, FEM_model%FEM_prm, FEM_SGS%SGS_par,                  &
     &   femmesh%mesh, femmesh%group, FEM_model%MHD_mesh,               &
     &   FEM_model%MHD_prop, FEM_model%FEM_MHD_BCs%surf_bcs,            &
     &   iphys_nod, SGS_MHD_wk%iphys_ele, ak_MHD, SGS_MHD_wk%fem_int,   &
     &   FEM_SGS%FEM_filters%FEM_elens, FEM_SGS%FEM_filters%filtering,  &
     &   FEM_SGS%Csims, SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,       &
     &   SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat,                     &
     &   nod_fld, SGS_MHD_wk%ele_fld)
!
!     -----Output monitor date
!
      call output_time_step_control                                     &
     &   (MHD_step%flex_p%istep_max_dt, MHD_step%rms_step,              &
     &    FEM_model%FEM_prm, MHD_step%time_d,                           &
     &    femmesh%mesh, FEM_model%MHD_mesh, FEM_model%MHD_prop,         &
     &    iphys_nod, nod_fld, SGS_MHD_wk%iphys_ele,                     &
     &    SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int%jcs,                   &
     &    SGS_MHD_wk%rhs_mat, SGS_MHD_wk%mhd_fem_wk, fem_sq)
!
      call output_monitor_control(MHD_step%flex_p%istep_max_dt,         &
     &    MHD_step%point_step, MHD_step%time_d, femmesh%mesh, nod_fld)
!
      if (iflag_debug.eq.1) write(*,*) 's_output_sgs_model_coefs'
      call s_output_sgs_model_coefs(MHD_step%flex_p%istep_max_dt,       &
     &    MHD_step, FEM_SGS%SGS_par, FEM_model%MHD_prop,                &
     &    SGS_MHD_wk%FEM_SGS_wk)
!
!     ---- Output voulme field data
!
      if (iflag_debug.eq.1) write(*,*) 's_output_ucd_file_control'
      call s_output_ucd_file_control                                    &
     &   (MHD_files%ucd_file_IO, MHD_step%flex_p%istep_max_dt,          &
     &    MHD_step%time_d, MHD_step%ucd_step, MHD_IO%fem_ucd)
!
!     ----
!
      call MHD_viz_routine_flag_and_step                                &
     &   (MHD_step%flex_p, MHD_step%time_d, MHD_step%viz_step, visval)
!
      end subroutine FEM_analyze_snap_tmp
!
! ----------------------------------------------------------------------
!
      subroutine FEM_finalize_snap_tmp(MHD_files, MHD_step, MHD_IO)
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
      type(MHD_IO_data), intent(inout) :: MHD_IO
!
!
      if(MHD_step%ucd_step%increment .gt. 0) then
        call finalize_output_ucd(MHD_files%ucd_file_IO, MHD_IO%fem_ucd)
        call dealloc_phys_range(MHD_IO%range)
      end if
!      call close_boundary_monitor(my_rank)
!
      end subroutine FEM_finalize_snap_tmp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine lead_specital_SGS                                      &
     &       (MHD_step, FEM_prm, SGS_par, mesh, group,                  &
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
     &      mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,              &
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
     &      iphys, iphys_ele, ele_fld, fem_int%jcs, fem_int%rhs_tbl,    &
     &      FEM_elens, filtering, Csims_FEM_MHD%sgs_coefs,              &
     &      mk_MHD%mlump_cd, FEM_SGS_wk%wk_filter, mhd_fem_wk,          &
     &      rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)

      end if
!
      if (iphys%i_SGS_induction .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_SGS_induction)
        call int_vol_sgs_induction(FEM_prm, mesh%nod_comm,              &
     &      mesh%node, mesh%ele, MHD_mesh%conduct, iphys,               &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,     &
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
