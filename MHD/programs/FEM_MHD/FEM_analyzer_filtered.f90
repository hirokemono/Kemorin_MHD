!FEM_analyzer_filtered.f90
!      module FEM_analyzer_filtered
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_analyze_filtered(i_step, MHD_files,              &
!!     &          fem, iphys, FEM_model, ak_MHD,                        &
!!     &          MHD_step, visval, FEM_SGS, SGS_MHD_wk,                &
!!     &          nod_fld, fem_ucd, MHD_IO, fem_sq)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(mesh_data), intent(in) :: fem
!!        type(phys_address), intent(in) :: iphys
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(FEM_SGS_structure), intent(inout) :: FEM_SGS
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(ucd_file_data), intent(inout) :: fem_ucd
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!
      module FEM_analyzer_filtered
!
      use m_precision
      use m_work_time
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_FEM_MHD_model_data
      use t_material_property
      use t_IO_step_parameter
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_ucd_file
      use t_FEM_MHD_mean_square
      use t_FEM_SGS_structure
      use t_MHD_IO_data
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
      subroutine FEM_analyze_filtered(i_step, MHD_files,                &
     &          fem, iphys, FEM_model, ak_MHD,                          &
     &          MHD_step, visval, FEM_SGS, SGS_MHD_wk,                  &
     &          nod_fld, fem_ucd, MHD_IO, fem_sq)
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
!
      use node_monitor_IO
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
      type(mesh_data), intent(in) :: fem
      type(phys_address), intent(in) :: iphys
      type(FEM_MHD_model_data), intent(in) :: FEM_model
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
!
      integer(kind=kint ), intent(inout) :: visval
      type(MHD_step_param), intent(inout) :: MHD_step
!
      type(phys_data), intent(inout) :: nod_fld
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!
      type(ucd_file_data), intent(inout) :: fem_ucd
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(MHD_IO_data), intent(inout) :: MHD_IO
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
     &      fem%mesh%node, nod_fld, SNAP_time_IO,                       &
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
     &   (FEM_model%MHD_prop, iphys, nod_fld)
!
!     ---------------------
!
      if (iflag_debug.eq.1)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(fem%mesh, nod_fld)
!
      if (iflag_debug.eq.1)  write(*,*) 'update_FEM_fields'
      call update_FEM_fields(MHD_step%time_d, FEM_model%FEM_prm,        &
     &    FEM_SGS%SGS_par, fem, FEM_model%MHD_mesh,                     &
     &    FEM_model%FEM_MHD_BCs, iphys, FEM_SGS%FEM_filters,            &
     &    SGS_MHD_wk, nod_fld, FEM_SGS%Csims)
!
!     ----- Evaluate model coefficients
!
      call cal_FEM_model_coefficients                                   &
     &   (MHD_step%time_d, FEM_model%FEM_prm, FEM_SGS%SGS_par,          &
     &    fem, FEM_model%MHD_mesh, FEM_model%MHD_prop,                  &
     &    FEM_model%FEM_MHD_BCs,iphys, FEM_SGS%FEM_filters,             &
     &    SGS_MHD_wk, nod_fld, FEM_SGS%Csims)
!
!     ========  Data output
!
      call lead_fields_by_FEM(MHD_step%flex_p%istep_max_dt, MHD_step,   &
     &    FEM_model%FEM_prm, FEM_SGS%SGS_par, fem, FEM_model%MHD_mesh,  &
     &    FEM_model%MHD_prop, FEM_model%FEM_MHD_BCs,                    &
     &    iphys, FEM_SGS%iphys_LES, ak_MHD, FEM_SGS%FEM_filters,        &
     &    SGS_MHD_wk, nod_fld, FEM_SGS%Csims)
!
!     ----Filtering
      if (iflag_debug.eq.1) write(*,*) 'filtering_all_fields'
      call filtering_all_fields(FEM_SGS%SGS_par%filter_p,               &
     &    fem%mesh%nod_comm, fem%mesh%node,                             &
     &    FEM_SGS%FEM_filters%filtering,                                &
     &    SGS_MHD_wk%FEM_SGS_wk%wk_filter, nod_fld)
!
!     -----Output monitor date
!
      call output_time_step_control                                     &
     &   (MHD_step%flex_p%istep_max_dt, MHD_step%rms_step,              &
     &    FEM_model%FEM_prm, MHD_step%time_d, fem%mesh,                 &
     &    FEM_model%MHD_mesh, FEM_model%MHD_prop,                       &
     &    iphys, nod_fld, SGS_MHD_wk%iphys_ele,                         &
     &    SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int%jcs,                   &
     &    SGS_MHD_wk%rhs_mat, SGS_MHD_wk%mhd_fem_wk, fem_sq)
!
      call output_monitor_control(MHD_step%flex_p%istep_max_dt,         &
     &    MHD_step%point_step, MHD_step%time_d, fem%mesh, nod_fld)
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
     &    MHD_step%time_d, MHD_step%ucd_step, fem_ucd)
!
!     ----
!
      call MHD_viz_routine_flag_and_step                                &
     &   (MHD_step%flex_p, MHD_step%time_d, MHD_step%viz_step, visval)
!
      end subroutine FEM_analyze_filtered
!
! ----------------------------------------------------------------------
!
      end module FEM_analyzer_filtered
