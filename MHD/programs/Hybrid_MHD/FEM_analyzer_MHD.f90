!
!      module FEM_analyzer_MHD
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_initialize_MHD(MHD_files, flex_MHD, MHD_step,    &
!!     &          FEM_model, FEM_MHD, MHD_CG, FEM_SGS, SGS_MHD_wk,      &
!!     &          MHD_IO, fem_sq, m_SR)
!!      subroutine FEM_analyze_MHD(MHD_files, FEM_model, flex_MHD,      &
!!     &          MHD_step, retval, FEM_MHD, MHD_CG,                    &
!!     &          FEM_SGS, SGS_MHD_wk, MHD_IO, fem_sq, m_SR)
!!      subroutine FEM_finalize_MHD(MHD_files, MHD_step, MHD_IO)
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(FEM_MHD_time_stepping), intent(inout) :: flex_MHD
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
!!        type(FEM_MHD_solvers), intent(inout) :: MHD_CG
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!        type(mesh_SR), intent(inout) :: m_SR
!
      module FEM_analyzer_MHD
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
!
      use t_FEM_mesh_field_data
      use t_SGS_model_addresses
      use t_material_property
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_FEM_MHD_time_stepping
      use t_cal_max_indices
      use t_FEM_MHD_mean_square
      use t_FEM_MHD_solvers
      use t_MHD_IO_data
      use t_mesh_SR
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
     &          FEM_model, FEM_MHD, MHD_CG, FEM_SGS, SGS_MHD_wk,        &
     &          MHD_IO, fem_sq, m_SR)
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
      type(FEM_MHD_model_data), intent(inout) :: FEM_model
      type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: iflag
!
!   matrix assembling
!
      call init_analyzer_fl(MHD_files, FEM_model%bc_FEM_IO,             &
     &    FEM_model%FEM_prm, FEM_SGS%SGS_par, flex_MHD, MHD_step,       &
     &    FEM_MHD%geofem, FEM_model%MHD_mesh,                           &
     &    FEM_SGS%FEM_filters, FEM_model%MHD_prop, FEM_model%MHD_BC,    &
     &    FEM_model%FEM_MHD_BCs, FEM_SGS%Csims,                         &
     &    FEM_MHD%iref_base, FEM_MHD%iref_grad, FEM_MHD%ref_fld,        &
     &    FEM_MHD%iphys, FEM_SGS%iphys_LES, FEM_MHD%field,              &
     &    MHD_CG, SGS_MHD_wk, fem_sq, MHD_IO%rst_IO, m_SR, label_sim)
!
      call nod_fields_send_recv(FEM_MHD%geofem%mesh, FEM_MHD%field,     &
     &                          m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
!   obtain elemental averages
!
      call reset_update_flag(FEM_MHD%field,                             &
     &    FEM_SGS%Csims%sgs_coefs, FEM_SGS%Csims%diff_coefs)
      if (iflag_debug.eq.1) write(*,*) 'update_FEM_fields'
      call update_FEM_fields(MHD_step%time_d, FEM_model%FEM_prm,        &
     &    FEM_SGS%SGS_par, FEM_MHD%geofem, FEM_model%MHD_mesh,          &
     &    FEM_model%FEM_MHD_BCs, FEM_MHD%iphys, FEM_SGS%iphys_LES,      &
     &    FEM_SGS%FEM_filters, SGS_MHD_wk, FEM_MHD%field,               &
     &    FEM_SGS%Csims, m_SR)
!
      call copy_model_coef_2_previous                                   &
     &   (FEM_SGS%SGS_par%model_p, FEM_SGS%SGS_par%commute_p,           &
     &    SGS_MHD_wk%FEM_SGS_wk)
!
!   construct matrix for Poisson and diffusion terms
!
      if (iflag_debug.eq.1) write(*,*) 'set_data_4_const_matrices'
      call set_data_4_const_matrices                                    &
     &   (FEM_MHD%geofem, FEM_model%MHD_mesh, FEM_model%MHD_prop,       &
     &    SGS_MHD_wk%fem_int, MHD_CG%MGCG_WK, MHD_CG%MHD_mat_tbls,      &
     &    MHD_CG%solver_pack)
      if (iflag_debug.eq.1) write(*,*) 'set_aiccg_matrices'
      call set_aiccg_matrices                                           &
     &   (MHD_step%time_d%dt, FEM_model%FEM_prm, FEM_SGS%SGS_par,       &
     &    FEM_MHD%geofem, FEM_model%MHD_mesh, FEM_model%FEM_MHD_BCs,    &
     &    FEM_model%MHD_prop, SGS_MHD_wk%fem_int,                       &
     &    FEM_SGS%FEM_filters%FEM_elens, FEM_SGS%Csims,                 &
     &    SGS_MHD_wk%mk_MHD, SGS_MHD_wk%rhs_mat, MHD_CG)
!
!   time evolution loop start!
!
      call cal_FEM_model_coefficients                                   &
     &   (MHD_step%time_d, FEM_model%FEM_prm, FEM_SGS%SGS_par,          &
     &    FEM_MHD%geofem, FEM_model%MHD_mesh, FEM_model%MHD_prop,       &
     &    FEM_model%FEM_MHD_BCs, FEM_MHD%iphys, FEM_SGS%iphys_LES,      &
     &    FEM_SGS%FEM_filters, SGS_MHD_wk, FEM_MHD%field,               &
     &    FEM_SGS%Csims, m_SR)
!
      call lead_fields_by_FEM(MHD_step%flex_p%istep_max_dt, MHD_step,   &
     &   FEM_model%FEM_prm, FEM_SGS%SGS_par, FEM_MHD%geofem,            &
     &   FEM_model%MHD_mesh, FEM_model%MHD_prop, FEM_model%FEM_MHD_BCs, &
     &   FEM_MHD%iphys, FEM_SGS%iphys_LES, MHD_CG%ak_MHD,               &
     &   FEM_SGS%FEM_filters, SGS_MHD_wk, FEM_MHD%field, FEM_SGS%Csims, &
     &   m_SR)
!
!     ---------------------
!
      FEM_SGS%SGS_par%iflag_SGS_initial = 0
!
      call s_check_deltat_by_prev_rms(MHD_step, FEM_MHD%geofem%mesh,    &
     &    FEM_model%MHD_mesh, FEM_model%MHD_prop,                       &
     &    FEM_MHD%iphys, FEM_MHD%field, SGS_MHD_wk%fem_int,             &
     &    SGS_MHD_wk%rhs_mat, flex_MHD)
!
!    Open monitor files
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
!
      call output_grd_file_w_org_connect                                &
     &   (MHD_step%ucd_step, FEM_MHD%geofem%mesh, FEM_model%MHD_mesh,   &
     &    FEM_MHD%field, ucd_param, MHD_IO%ucd, m_SR%SR_sig, m_SR%SR_i)
!
      call alloc_phys_range(FEM_MHD%field%ntot_phys_viz, MHD_IO%range)
!       call s_open_boundary_monitor                                    &
!     &    (my_rank, FEM_MHD%geofem%group%sf_grp)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
      end subroutine FEM_initialize_MHD
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_MHD(MHD_files, FEM_model, flex_MHD,        &
     &          MHD_step, retval, FEM_MHD, MHD_CG,                      &
     &          FEM_SGS, SGS_MHD_wk, MHD_IO, fem_sq, m_SR)
!
      use t_ucd_file
      use t_FEM_MHD_mean_square
      use t_flex_delta_t_parameter
!
      use calypso_mpi_real
      use construct_matrices
      use lead_physical_values
      use update_after_evolution
      use chenge_step_4_dynamic
      use copy_nodal_fields
!
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
      type(SGS_model_addresses), intent(in) :: iphys_LES
!
      type(FEM_MHD_model_data), intent(in) :: FEM_model
!
      type(FEM_MHD_time_stepping), intent(inout) :: flex_MHD
      type(MHD_step_param), intent(inout) :: MHD_step
!
      type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(mesh_SR), intent(inout) :: m_SR
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
     &   (FEM_model%MHD_prop, FEM_MHD%iphys, FEM_MHD%field,             &
     &    MHD_step%flex_p, MHD_step%time_d)
!
      if (iflag_debug.eq.1) write(*,*) 'fields_evolution_4_FEM_SPH'
      call fields_evolution_4_FEM_SPH                                   &
     &   (MHD_step%time_d, FEM_model%FEM_prm, FEM_SGS%SGS_par,          &
     &    FEM_MHD%geofem, FEM_model%MHD_mesh%fluid, FEM_model%MHD_prop, &
     &    FEM_model%FEM_MHD_BCs, FEM_MHD%iref_base, FEM_MHD%iref_grad,  &
     &    FEM_MHD%ref_fld, FEM_MHD%iphys, FEM_SGS%iphys_LES,            &
     &    MHD_CG%ak_MHD, FEM_SGS%FEM_filters, MHD_CG%solver_pack,       &
     &    MHD_CG%MGCG_WK, SGS_MHD_wk, FEM_MHD%field, FEM_SGS%Csims,     &
     &    fem_sq, m_SR)
!
!     ----- Evaluate model coefficients
!
      call cal_FEM_model_coefficients                                   &
     &   (MHD_step%time_d, FEM_model%FEM_prm, FEM_SGS%SGS_par,          &
     &    FEM_MHD%geofem, FEM_model%MHD_mesh, FEM_model%MHD_prop,       &
     &    FEM_model%FEM_MHD_BCs, FEM_MHD%iphys, FEM_SGS%iphys_LES,      &
     &    FEM_SGS%FEM_filters, SGS_MHD_wk, FEM_MHD%field,               &
     &    FEM_SGS%Csims, m_SR)
!
!     ---------------------
!
      call s_check_flexible_time_step                                   &
     &   (FEM_MHD%geofem, FEM_model%MHD_mesh,                           &
     &    FEM_model%MHD_prop%cd_prop, FEM_MHD%iphys, FEM_MHD%field,     &
     &    SGS_MHD_wk%fem_int, SGS_MHD_wk%rhs_mat, flex_MHD, MHD_step)
!
!     ========  Data output
!
      if(MHD_step%flex_p%istep_flex_to_max .eq. 0) then
      call lead_fields_by_FEM(MHD_step%flex_p%istep_max_dt, MHD_step,   &
     &    FEM_model%FEM_prm, FEM_SGS%SGS_par,                           &
     &    FEM_MHD%geofem, FEM_model%MHD_mesh,                           &
     &    FEM_model%MHD_prop, FEM_model%FEM_MHD_BCs,                    &
     &    FEM_MHD%iphys, FEM_SGS%iphys_LES, MHD_CG%ak_MHD,              &
     &    FEM_SGS%FEM_filters, SGS_MHD_wk, FEM_MHD%field,               &
     &    FEM_SGS%Csims, m_SR)
!
!     -----Output monitor date
!
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
!
        call output_time_step_control                                   &
     &     (MHD_step%flex_p%istep_max_dt, MHD_step%rms_step,            &
     &      FEM_model%FEM_prm, MHD_step%time_d, FEM_MHD%geofem,         &
     &      FEM_model%MHD_mesh, FEM_model%MHD_prop,                     &
     &      FEM_MHD%iphys, iphys_LES, FEM_MHD%field,                    &
     &      SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,              &
     &      SGS_MHD_wk%fem_int%jcs, SGS_MHD_wk%rhs_mat,                 &
     &      SGS_MHD_wk%mhd_fem_wk, fem_sq)
!
        call output_monitor_control(MHD_step%flex_p%istep_max_dt,       &
     &      MHD_step%point_step, MHD_step%time_d,                       &
     &      FEM_MHD%geofem%mesh, FEM_MHD%field, FEM_MHD%nod_mntr)
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
     &      MHD_step%ucd_step, MHD_step%time_d, MHD_IO%ucd)
!
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
      end if
!
!
      MHD_step%finish_d%elapsed_local                                   &
     &    = MPI_WTIME() - MHD_step%finish_d%started_time
      call calypso_mpi_allreduce_one_real                               &
     &   (MHD_step%finish_d%elapsed_local,                              &
     &    MHD_step%finish_d%elapsed_max, MPI_MAX)
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
     &    MHD_step%flex_p, FEM_MHD%geofem, FEM_MHD%iphys,               &
     &    SGS_MHD_wk%FEM_SGS_wk, MHD_step%rst_step, FEM_MHD%field,      &
     &    MHD_IO%rst_IO, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
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
     &    FEM_MHD%geofem, FEM_model%MHD_mesh, FEM_model%FEM_MHD_BCs,    &
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
      use t_ucd_file
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
      type(MHD_IO_data), intent(inout) :: MHD_IO
!
!
      if(MHD_step%ucd_step%increment .gt. 0) then
        call finalize_output_ucd(MHD_files%ucd_file_IO, MHD_IO%ucd)
        call dealloc_phys_range(MHD_IO%range)
      end if
!      call close_boundary_monitor(my_rank)
!
      end subroutine FEM_finalize_MHD
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_MHD
