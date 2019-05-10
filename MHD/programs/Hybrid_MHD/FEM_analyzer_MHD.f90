!
!      module FEM_analyzer_MHD
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_initialize_MHD(MHD_files, flex_MHD, MHD_step,    &
!!     &          fem,FEM_model, MHD_CG, FEM_SGS, SGS_MHD_wk,           &
!!     &          MHD_IO, fem_sq)
!!        type(mesh_data), intent(inout) :: fem
!!      subroutine FEM_analyze_MHD(MHD_files, fem, iphys_nod, FEM_model,&
!!     &          flex_MHD, MHD_step, visval, retval, nod_fld, MHD_CG,  &
!!     &          FEM_SGS, SGS_MHD_wk, MHD_IO, fem_sq)
!!      subroutine FEM_finalize_MHD(MHD_files, MHD_step, MHD_IO)
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(FEM_MHD_time_stepping), intent(inout) :: flex_MHD
!!        type(mesh_data), intent(in) :: fem
!!        type(phys_address), intent(inout) :: iphys_nod
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(FEM_MHD_solvers), intent(inout) :: MHD_CG
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      module FEM_analyzer_MHD
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
!
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_material_property
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_FEM_MHD_time_stepping
      use t_cal_max_indices
      use t_FEM_MHD_mean_square
      use t_FEM_MHD_solvers
      use t_MHD_IO_data
!
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine FEM_initialize_MHD(MHD_files, flex_MHD, MHD_step,      &
     &          fem,FEM_model, MHD_CG, FEM_SGS, SGS_MHD_wk,             &
     &          MHD_IO, fem_sq)
!
      use t_boundary_field_IO
!
      use input_control
      use initialization_4_MHD
      use lead_physical_values
      use update_after_evolution
      use FEM_MHD_evolution
!
      use nod_phys_send_recv
      use check_deltat_by_prev_rms
      use construct_matrices
!
      use chenge_step_4_dynamic
      use output_viz_file_control
      use FEM_MHD_ucd_data
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      type(FEM_MHD_time_stepping), intent(inout) :: flex_MHD
      type(MHD_step_param), intent(inout) :: MHD_step
!
      type(mesh_data), intent(inout) :: fem
      type(phys_address), intent(inout) :: iphys_nod
      type(phys_data), intent(inout) :: nod_fld
      type(FEM_MHD_model_data), intent(inout) :: FEM_model
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      integer(kind = kint) :: iflag
!
!   matrix assembling
!
      call init_analyzer_fl(MHD_files, FEM_model%bc_FEM_IO,             &
     &    FEM_model%FEM_prm, FEM_SGS%SGS_par, flex_MHD, MHD_step,       &
     &    fem%mesh, fem%group, FEM_model%MHD_mesh,                      &
     &    FEM_SGS%FEM_filters, FEM_model%MHD_prop, FEM_model%MHD_BC,    &
     &    FEM_model%FEM_MHD_BCs, FEM_SGS%Csims, iphys_nod, nod_fld,     &
     &    MHD_CG, SGS_MHD_wk, fem_sq, MHD_IO%rst_IO, label_sim)
!
      call nod_fields_send_recv(fem%mesh, nod_fld)
!
!   obtain elemental averages
!
      call reset_update_flag(nod_fld,                                   &
     &    FEM_SGS%Csims%sgs_coefs, FEM_SGS%Csims%diff_coefs)
      if (iflag_debug.eq.1) write(*,*) 'update_FEM_fields'
      call update_FEM_fields(MHD_step%time_d, FEM_model%FEM_prm,        &
     &    FEM_SGS%SGS_par, fem, FEM_model%MHD_mesh,                     &
     &    FEM_model%FEM_MHD_BCs, iphys_nod, FEM_SGS%FEM_filters,        &
     &    SGS_MHD_wk, nod_fld, FEM_SGS%Csims)
!
      call copy_model_coef_2_previous                                   &
     &   (FEM_SGS%SGS_par%model_p, FEM_SGS%SGS_par%commute_p,           &
     &    SGS_MHD_wk%FEM_SGS_wk)
!
!   construct matrix for Poisson and diffusion terms
!
      if (iflag_debug.eq.1) write(*,*) 'set_data_4_const_matrices'
      call set_data_4_const_matrices                                    &
     &   (fem, FEM_model%MHD_mesh, FEM_model%MHD_prop,                  &
     &    SGS_MHD_wk%fem_int, MHD_CG%MGCG_WK, MHD_CG%MHD_mat_tbls,      &
     &    MHD_CG%solver_pack)
      if (iflag_debug.eq.1) write(*,*) 'set_aiccg_matrices'
      call set_aiccg_matrices(MHD_step%time_d%dt,                       &
     &    FEM_model%FEM_prm, FEM_SGS%SGS_par, fem, FEM_model%MHD_mesh,  &
     &    FEM_model%FEM_MHD_BCs, FEM_model%MHD_prop,                    &
     &    SGS_MHD_wk%fem_int, FEM_SGS%FEM_filters%FEM_elens,            &
     &    FEM_SGS%Csims, SGS_MHD_wk%mk_MHD, SGS_MHD_wk%rhs_mat, MHD_CG)
!
!   time evolution loop start!
!
      call cal_FEM_model_coefficients                                   &
     &   (MHD_step%time_d, FEM_model%FEM_prm, FEM_SGS%SGS_par,          &
     &    fem, FEM_model%MHD_mesh, FEM_model%MHD_prop,                  &
     &    FEM_model%FEM_MHD_BCs, iphys_nod, FEM_SGS%FEM_filters,        &
     &    SGS_MHD_wk, nod_fld, FEM_SGS%Csims)
!
      call lead_fields_by_FEM(MHD_step%flex_p%istep_max_dt, MHD_step,   &
     &    FEM_model%FEM_prm, FEM_SGS%SGS_par, fem,                      &
     &    FEM_model%MHD_mesh, FEM_model%MHD_prop,                       &
     &    FEM_model%FEM_MHD_BCs, iphys_nod, MHD_CG%ak_MHD,              &
     &    FEM_SGS%FEM_filters, SGS_MHD_wk, nod_fld, FEM_SGS%Csims)
!
!     ---------------------
!
      FEM_SGS%SGS_par%iflag_SGS_initial = 0
!
      call s_check_deltat_by_prev_rms(MHD_step, fem%mesh,               &
     &    FEM_model%MHD_mesh, FEM_model%MHD_prop, iphys_nod, nod_fld,   &
     &    SGS_MHD_wk%fem_int, SGS_MHD_wk%rhs_mat, flex_MHD)
!
!    Open monitor files
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
!
      call output_grd_file_w_org_connect                                &
     &   (MHD_step%ucd_step, fem%mesh, FEM_model%MHD_mesh, nod_fld,     &
     &    ucd_param, MHD_IO%fem_ucd)
!
      call alloc_phys_range(nod_fld%ntot_phys_viz, MHD_IO%range)
!       call s_open_boundary_monitor(my_rank, fem%group%sf_grp)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
      end subroutine FEM_initialize_MHD
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_MHD(MHD_files, fem, iphys_nod, FEM_model,  &
     &          flex_MHD, MHD_step, visval, retval, nod_fld, MHD_CG,    &
     &          FEM_SGS, SGS_MHD_wk, MHD_IO, fem_sq)
!
      use t_FEM_MHD_mean_square
      use construct_matrices
      use lead_physical_values
      use update_after_evolution
      use chenge_step_4_dynamic
      use copy_nodal_fields
!
      use node_monitor_IO
      use FEM_sgs_model_coefs_IO
      use fem_mhd_rst_IO_control
      use output_viz_file_control
!
      use init_iccg_matrices
      use check_deltat_by_prev_rms
      use output_viz_file_control
      use FEM_flexible_time_step
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(mesh_data), intent(in) :: fem
      type(phys_address), intent(in) :: iphys_nod
!
      type(FEM_MHD_model_data), intent(in) :: FEM_model
!
      integer(kind=kint), intent(inout) :: visval
      type(FEM_MHD_time_stepping), intent(inout) :: flex_MHD
      type(MHD_step_param), intent(inout) :: MHD_step
!
      type(phys_data), intent(inout) :: nod_fld
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      integer(kind=kint ), intent(inout) :: retval
!
      integer(kind = kint) :: iflag
!
!
!     ---- step to next time!! --- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_new_time_and_step'
      call set_new_time_and_step                                        &
     &   (FEM_model%MHD_prop, iphys_nod, nod_fld,                       &
     &    MHD_step%flex_p, MHD_step%time_d)
!
      if (iflag_debug.eq.1) write(*,*) 'fields_evolution_4_FEM_SPH'
      call fields_evolution_4_FEM_SPH                                   &
     &   (MHD_step%time_d, FEM_model%FEM_prm, FEM_SGS%SGS_par,          &
     &    fem, FEM_model%MHD_mesh%fluid, FEM_model%MHD_prop,            &
     &    FEM_model%FEM_MHD_BCs, iphys_nod, MHD_CG%ak_MHD,              &
     &    FEM_SGS%FEM_filters, MHD_CG%solver_pack, MHD_CG%MGCG_WK,      &
     &    SGS_MHD_wk, nod_fld, FEM_SGS%Csims, fem_sq)
!
!     ----- Evaluate model coefficients
!
      call cal_FEM_model_coefficients                                   &
     &   (MHD_step%time_d, FEM_model%FEM_prm, FEM_SGS%SGS_par,          &
     &    fem, FEM_model%MHD_mesh, FEM_model%MHD_prop,                  &
     &    FEM_model%FEM_MHD_BCs, iphys_nod, FEM_SGS%FEM_filters,        &
     &    SGS_MHD_wk, nod_fld,  FEM_SGS%Csims)
!
!     ---------------------
!
      call s_check_flexible_time_step(fem%mesh, FEM_model%MHD_mesh,     &
     &    FEM_model%MHD_prop%cd_prop, iphys_nod, nod_fld,               &
     &    SGS_MHD_wk%fem_int, SGS_MHD_wk%rhs_mat, flex_MHD, MHD_step)
!
!     ========  Data output
!
      if(MHD_step%flex_p%istep_flex_to_max .eq. 0) then
        call lead_fields_by_FEM(MHD_step%flex_p%istep_max_dt, MHD_step, &
     &     FEM_model%FEM_prm, FEM_SGS%SGS_par, fem, FEM_model%MHD_mesh, &
     &     FEM_model%MHD_prop, FEM_model%FEM_MHD_BCs,                   &
     &     iphys_nod, MHD_CG%ak_MHD, FEM_SGS%FEM_filters, SGS_MHD_wk,   &
     &     nod_fld, FEM_SGS%Csims)
!
!     -----Output monitor date
!
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
!
        call output_time_step_control                                   &
     &     (MHD_step%flex_p%istep_max_dt, MHD_step%rms_step,            &
     &      FEM_model%FEM_prm, MHD_step%time_d, fem%mesh,               &
     &      FEM_model%MHD_mesh, FEM_model%MHD_prop,                     &
     &      iphys_nod, nod_fld, SGS_MHD_wk%iphys_ele,                   &
     &      SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int%jcs,                 &
     &      SGS_MHD_wk%rhs_mat, SGS_MHD_wk%mhd_fem_wk, fem_sq)
!
        call output_monitor_control(MHD_step%flex_p%istep_max_dt,       &
     &      MHD_step%point_step, MHD_step%time_d, fem%mesh, nod_fld)
!
        if (iflag_debug.eq.1) write(*,*) 's_output_sgs_model_coefs'
        call s_output_sgs_model_coefs(MHD_step%flex_p%istep_max_dt,     &
     &      MHD_step, FEM_SGS%SGS_par, FEM_model%MHD_prop,              &
     &      SGS_MHD_wk%FEM_SGS_wk)
!
!     ---- Output voulme field data
!
        if (iflag_debug.eq.1) write(*,*) 's_output_ucd_file_control'
        call s_output_ucd_file_control                                  &
     &     (ucd_param, MHD_step%flex_p%istep_max_dt,                    &
     &      MHD_step%time_d, MHD_step%ucd_step, MHD_IO%fem_ucd)
!
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
      end if
!
!
      MHD_step%finish_d%elapsed_local                                   &
     &    = MPI_WTIME() - MHD_step%finish_d%started_time
      call MPI_allREDUCE (MHD_step%finish_d%elapsed_local,              &
     &    MHD_step%finish_d%elapsed_max, 1, CALYPSO_REAL,               &
     &    MPI_MAX, CALYPSO_COMM, ierr_MPI)
      if(iflag_debug.gt.0) write(*,*) 'total_time',                     &
     &  MHD_step%finish_d%elapsed_local, MHD_step%finish_d%elapsed_time
!
!     ---- Output restart field data
!
      if(MHD_step%finish_d%i_end_step .eq. -1) then
        if(MHD_step%finish_d%elapsed_max                                &
     &     .gt. MHD_step%finish_d%elapsed_time) retval = 0
      end if
!
!     ----
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      call output_MHD_restart_file_ctl                                  &
     &   (retval, FEM_SGS%SGS_par, MHD_files, MHD_step%time_d,          &
     &    MHD_step%flex_p, fem%mesh, iphys_nod, SGS_MHD_wk%FEM_SGS_wk,  &
     &    MHD_step%rst_step, nod_fld, MHD_IO%rst_IO)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!   Finish by specific step
      if(MHD_step%finish_d%i_end_step .ne. -1) then
        if(MHD_step%flex_p%iflag_flexible_step .eq. iflag_flex_step)    &
     &   then
          if(MHD_step%time_d%time .gt. MHD_step%flex_p%time_to_finish)  &
     &        retval = 0
        else
          if(MHD_step%flex_p%istep_max_dt                               &
     &         .ge. MHD_step%finish_d%i_end_step) retval = 0
        end if
      end if
!
!   Set visualization flag and step
      call MHD_viz_routine_flag_and_step                                &
     &   (MHD_step%flex_p, MHD_step%time_d, MHD_step%viz_step, visval)
!
!     --------------------- 
!
      call s_chenge_step_4_dynamic                                      &
     &   (my_rank, MHD_step%time_d%i_time_step,                         &
     &    FEM_SGS%SGS_par, SGS_MHD_wk)
!
      if ( retval .ne. 0 ) then
        if (iflag_debug.eq.1) write(*,*) 'update_matrices'
        call update_matrices                                            &
     &   (MHD_step%time_d, FEM_model%FEM_prm, FEM_SGS%SGS_par,          &
     &    fem, FEM_model%MHD_mesh, FEM_model%FEM_MHD_BCs,               &
     &    FEM_model%MHD_prop, SGS_MHD_wk%fem_int,                       &
     &    FEM_SGS%FEM_filters%FEM_elens, FEM_SGS%Csims,                 &
     &    MHD_step%flex_p, SGS_MHD_wk%mk_MHD,                           &
     &    SGS_MHD_wk%rhs_mat, MHD_CG)
      end if
!
      end subroutine FEM_analyze_MHD
!
! ----------------------------------------------------------------------
!
      subroutine FEM_finalize_MHD(MHD_files, MHD_step, MHD_IO)
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
      end subroutine FEM_finalize_MHD
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_MHD
