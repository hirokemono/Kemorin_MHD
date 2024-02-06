!FEM_analyzer_filtered.f90
!      module FEM_analyzer_filtered
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_analyze_filtered(i_step, MHD_files, FEM_model,   &
!!     &          ak_MHD, MHD_step, FEM_SGS, SGS_MHD_wk, FEM_MHD, ucd,  &
!!     &          MHD_IO, fem_sq, m_SR)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(FEM_SGS_structure), intent(inout) :: FEM_SGS
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
!!        type(ucd_data), intent(inout) :: ucd
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!        type(mesh_SR), intent(inout) :: m_SR
!
      module FEM_analyzer_filtered
!
      use m_precision
      use m_work_time
      use t_time_data
      use t_FEM_mesh_field_data
      use t_FEM_MHD_model_data
      use t_material_property
      use t_IO_step_parameter
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_ucd_data
      use t_FEM_MHD_mean_square
      use t_FEM_SGS_structure
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
      subroutine FEM_analyze_filtered(i_step, MHD_files, FEM_model,     &
     &          ak_MHD, MHD_step, FEM_SGS, SGS_MHD_wk, FEM_MHD, ucd,    &
     &          MHD_IO, fem_sq, m_SR)
!
      use m_fem_mhd_restart
      use t_FEM_MHD_mean_square
      use t_ucd_file
!
      use nod_phys_send_recv
      use lead_physical_values
      use update_after_evolution
      use FEM_MHD_evolution
      use chenge_step_4_dynamic
      use copy_nodal_fields
!
      use FEM_sgs_model_coefs_IO
      use output_viz_file_control
      use filter_all_fields
      use input_control
!
      use check_deltat_by_prev_rms
      use output_viz_file_control
!
      integer(kind=kint ), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(FEM_MHD_model_data), intent(in) :: FEM_model
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
!
      type(MHD_step_param), intent(inout) :: MHD_step
!
      type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!
      type(ucd_data), intent(inout) :: ucd
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(mesh_SR), intent(inout) :: m_SR
!
!     ---- Load field data --- 
!
      call reset_update_flag(FEM_MHD%field, FEM_SGS%Csims%sgs_coefs,    &
     &                       FEM_SGS%Csims%diff_coefs)
      MHD_step%flex_p%istep_max_dt = i_step
      if (my_rank.eq.0) write(*,*)                                      &
     &        'step: ', MHD_step%flex_p%istep_max_dt
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
      call update_FEM_fields(MHD_step%time_d, FEM_model%FEM_prm,        &
     &    FEM_SGS%SGS_par, FEM_MHD%geofem, FEM_model%MHD_mesh,          &
     &    FEM_model%FEM_MHD_BCs, FEM_MHD%iphys, FEM_SGS%iphys_LES,      &
     &    FEM_SGS%FEM_filters, SGS_MHD_wk, FEM_MHD%field,               &
     &    FEM_SGS%Csims, m_SR)
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
      call lead_fields_by_FEM                                           &
     &   (MHD_step%flex_p%istep_max_dt, MHD_step, FEM_model%FEM_prm,    &
     &    FEM_SGS%SGS_par, FEM_MHD%geofem, FEM_model%MHD_mesh,          &
     &    FEM_model%MHD_prop, FEM_model%FEM_MHD_BCs, FEM_MHD%iphys,     &
     &    FEM_SGS%iphys_LES, ak_MHD, FEM_SGS%FEM_filters,               &
     &    SGS_MHD_wk, FEM_MHD%field, FEM_SGS%Csims, m_SR)
!
!     ----Filtering
      if (iflag_debug.eq.1) write(*,*) 'filtering_all_fields'
      call filtering_all_fields(FEM_SGS%SGS_par%filter_p,               &
     &    FEM_MHD%geofem%mesh%nod_comm, FEM_MHD%geofem%mesh%node,       &
     &    FEM_SGS%FEM_filters%filtering,                                &
     &    SGS_MHD_wk%FEM_SGS_wk%wk_filter,                              &
     &    FEM_MHD%field, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
!     -----Output monitor date
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
     &    MHD_step%ucd_step, MHD_step%time_d, ucd)
!
      end subroutine FEM_analyze_filtered
!
! ----------------------------------------------------------------------
!
      end module FEM_analyzer_filtered
