!FEM_analyzer_snapshot.f90
!      module FEM_analyzer_snapshot
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_initialize_snapshot                              &
!!     &         (MHD_files, MHD_step, FEM_model, ak_MHD, FEM_MHD,      &
!!     &          FEM_SGS, SGS_MHD_wk, MHD_IO, fem_sq, m_SR)
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(FEM_MHD_model_data), intent(inout) :: FEM_model
!!        type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
!!        type(FEM_SGS_structure), intent(inout) :: FEM_SGS
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine FEM_analyze_snapshot                                 &
!!     &         (i_step, MHD_files, FEM_model, ak_MHD, MHD_step,       &
!!     &          FEM_SGS, SGS_MHD_wk, FEM_MHD, MHD_IO, fem_sq, m_SR)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(FEM_MHD_model_data), intent(in) :: FEM_model
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
!!        type(FEM_SGS_structure), intent(inout) :: FEM_SGS
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine FEM_finalize_snapshot(MHD_files, MHD_step, MHD_IO)
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!
      module FEM_analyzer_snapshot
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
!
      use t_time_data
      use t_FEM_mesh_field_data
      use t_SGS_model_addresses
      use t_material_property
      use t_ucd_file
      use t_IO_step_parameter
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_cal_max_indices
      use t_FEM_MHD_model_data
      use t_FEM_SGS_structure
      use t_FEM_MHD_mean_square
      use t_MHD_IO_data
      use t_work_FEM_SGS_MHD
      use t_mesh_SR
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
      subroutine FEM_initialize_snapshot                                &
     &         (MHD_files, MHD_step, FEM_model, ak_MHD, FEM_MHD,        &
     &          FEM_SGS, SGS_MHD_wk, MHD_IO, fem_sq, m_SR)
!
      use t_boundary_field_IO
!
      use initialize_4_snapshot
      use FEM_MHD_ucd_data
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
!
      type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
      type(FEM_MHD_model_data), intent(inout) :: FEM_model
      type(coefs_4_MHD_type), intent(inout) :: ak_MHD
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(mesh_SR), intent(inout) :: m_SR
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'init_analyzer_snap'
      call init_analyzer_snap(MHD_files,                                &
     &    FEM_model%FEM_prm, FEM_SGS%SGS_par, FEM_model%bc_FEM_IO,      &
     &    MHD_step, FEM_MHD%geofem, FEM_model%MHD_mesh,                 &
     &    FEM_SGS%FEM_filters, FEM_model%MHD_prop, ak_MHD,              &
     &    FEM_model%MHD_BC, FEM_model%FEM_MHD_BCs, FEM_SGS%Csims,       &
     &    FEM_MHD%iref_base, FEM_MHD%iref_grad, FEM_MHD%ref_fld,        &
     &    FEM_MHD%iphys, FEM_SGS%iphys_LES, FEM_MHD%field,              &
     &    SNAP_time_IO, MHD_step%rst_step, SGS_MHD_wk, fem_sq,          &
     &    MHD_IO%rst_IO, m_SR, FEM_MHD%label_sim)
!
      call output_grd_file_w_org_connect                                &
     &   (MHD_step%ucd_step, FEM_MHD%geofem%mesh, FEM_model%MHD_mesh,   &
     &    FEM_MHD%field, MHD_files%ucd_file_IO, MHD_IO%ucd,             &
     &    m_SR%SR_sig, m_SR%SR_i)
!
      call alloc_phys_range(FEM_MHD%field%ntot_phys_viz, MHD_IO%range)
!
      end subroutine FEM_initialize_snapshot
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_snapshot                                   &
     &         (i_step, MHD_files, FEM_model, ak_MHD, MHD_step,         &
     &          FEM_SGS, SGS_MHD_wk, FEM_MHD, MHD_IO, fem_sq, m_SR)
!
      use m_fem_mhd_restart
      use t_FEM_MHD_mean_square
!
      use nod_phys_send_recv
      use lead_physical_values
      use update_after_evolution
      use FEM_MHD_evolution
      use chenge_step_4_dynamic
      use copy_nodal_fields
      use input_control
!
      use FEM_sgs_model_coefs_IO
      use output_viz_file_control
!
      use check_deltat_by_prev_rms
      use output_viz_file_control
!
      integer(kind=kint ), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
!
      type(MHD_step_param), intent(inout) :: MHD_step
!
      type(FEM_MHD_model_data), intent(inout) :: FEM_model
      type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(mesh_SR), intent(inout) :: m_SR
!
!     ---- Load field data --- 
!
      call reset_update_flag(FEM_MHD%field,                             &
     &    FEM_SGS%Csims%sgs_coefs, FEM_SGS%Csims%diff_coefs)
      MHD_step%flex_p%istep_max_dt = i_step
      if(my_rank.eq.0) write(*,*)                                       &
     &    'step: ', MHD_step%flex_p%istep_max_dt
!
      if (MHD_step%rst_step%increment .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'input_restart_4_snapshot'
        call input_restart_4_snapshot                                   &
     &     (MHD_step%flex_p%istep_max_dt, MHD_files%fst_file_IO,        &
     &      FEM_MHD%geofem%mesh%node, FEM_MHD%field, SNAP_time_IO,      &
     &      MHD_step%rst_step, MHD_IO%rst_IO)
!
      else if (MHD_step%ucd_step%increment .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'read_udt_4_snap'
        call read_udt_4_snap                                            &
     &     (MHD_step%flex_p%istep_max_dt, MHD_step%ucd_step,            &
     &      MHD_files%org_ucd_file_IO, FEM_MHD%field, SNAP_time_IO)
!
        MHD_step%time_d%time = MHD_step%init_d%time                     &
     &       + MHD_step%time_d%dt * dble(MHD_step%flex_p%istep_max_dt)
        MHD_step%time_d%i_time_step = MHD_step%flex_p%istep_max_dt
      end if
!
!     ---------------------
!
      call set_perturbation_to_scalar(FEM_model%MHD_prop,               &
     &    FEM_MHD%iref_base, FEM_MHD%ref_fld,                           &
     &    FEM_MHD%iphys, FEM_MHD%field)
!
!     ---------------------
!
      if (iflag_debug.eq.1)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(FEM_MHD%geofem%mesh, FEM_MHD%field,     &
     &                          m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
      if (iflag_debug.eq.1)  write(*,*) 'update_FEM_fields'
      call update_FEM_fields(MHD_step%time_d,                           &
     &    FEM_model%FEM_prm, FEM_SGS%SGS_par, FEM_MHD%geofem,           &
     &    FEM_model%MHD_mesh, FEM_model%FEM_MHD_BCs,                    &
     &    FEM_MHD%iphys, FEM_SGS%iphys_LES, FEM_SGS%FEM_filters,        &
     &    SGS_MHD_wk, FEM_MHD%field, FEM_SGS%Csims, m_SR)
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
!     ========  Data output
!
      call lead_fields_by_FEM(MHD_step%flex_p%istep_max_dt,             &
     &    MHD_step, FEM_model, FEM_SGS%SGS_par, FEM_SGS%iphys_LES,      &
     &    ak_MHD, FEM_SGS%FEM_filters, FEM_MHD, SGS_MHD_wk,             &
     &    FEM_SGS%Csims, m_SR)
!
!     -----Output monitor date
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
!
      call output_time_step_control                                     &
     &   (MHD_step%flex_p%istep_max_dt, MHD_step%rms_step,              &
     &    FEM_model%FEM_prm, MHD_step%time_d, FEM_MHD%geofem,           &
     &    FEM_model%MHD_mesh, FEM_model%MHD_prop,                       &
     &    FEM_MHD%iphys, FEM_SGS%iphys_LES, FEM_MHD%field,              &
     &    SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%ele_fld,                &
     &    SGS_MHD_wk%fem_int%jcs, SGS_MHD_wk%rhs_mat,                   &
     &    SGS_MHD_wk%mhd_fem_wk, fem_sq)
!
      call output_monitor_control(MHD_step%flex_p%istep_max_dt,         &
     &    MHD_step%point_step, MHD_step%time_d,                         &
     &    FEM_MHD%geofem%mesh, FEM_MHD%field, FEM_MHD%nod_mntr)
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
     &    MHD_step%ucd_step, MHD_step%time_d, MHD_IO%ucd)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
      end subroutine FEM_analyze_snapshot
!
! ----------------------------------------------------------------------
!
      subroutine FEM_finalize_snapshot(MHD_files, MHD_step, MHD_IO)
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
!
      type(MHD_IO_data), intent(inout) :: MHD_IO
!
!
      if(MHD_step%ucd_step%increment .gt. 0) then
        call finalize_output_ucd(MHD_files%ucd_file_IO, MHD_IO%ucd)
        call dealloc_phys_range(MHD_IO%range)
      end if
!      call close_boundary_monitor(my_rank)
!
      end subroutine FEM_finalize_snapshot
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_snapshot
