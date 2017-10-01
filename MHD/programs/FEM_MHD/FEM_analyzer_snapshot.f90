!FEM_analyzer_snapshot.f90
!      module FEM_analyzer_snapshot
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_initialize_snapshot                              &
!!     &        (MHD_files, MHD_step, femmesh, ele_mesh,                &
!!     &         iphys_nod, nod_fld, FEM_model, ak_MHD, FEM_SGS,        &
!!     &         SGS_MHD_wk, range, fem_ucd, fem_sq, label_sim)
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(mesh_data), intent(inout) :: femmesh
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(phys_address), intent(inout) :: iphys_nod
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(FEM_MHD_model_data), intent(inout) :: FEM_model
!!        type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!!        type(FEM_SGS_structure), intent(inout) :: FEM_SGS
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(maximum_informations), intent(inout) :: range
!!        type(ucd_file_data), intent(inout) :: fem_ucd
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!      subroutine FEM_analyze_snapshot(i_step, MHD_files,              &
!!     &          femmesh, ele_mesh, iphys_nod, FEM_model, ak_MHD,      &
!!     &          MHD_step, visval, FEM_SGS, SGS_MHD_wk,                &
!!     &          nod_fld, fem_ucd, fem_sq)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(mesh_data), intent(in) :: femmesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(phys_address), intent(in) :: iphys_nod
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(FEM_MHD_model_data), intent(in) :: FEM_model
!!        type(FEM_SGS_structure), intent(inout) :: FEM_SGS
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!      subroutine FEM_finalize_snapshot                                &
!!     &         (MHD_files, MHD_step, range, fem_ucd)
!
      module FEM_analyzer_snapshot
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_material_property
      use t_ucd_file
      use t_IO_step_parameter
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_cal_max_indices
      use t_FEM_MHD_model_data
      use t_FEM_SGS_structure
      use t_FEM_MHD_mean_square
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
      subroutine FEM_initialize_snapshot                                &
     &        (MHD_files, MHD_step, femmesh, ele_mesh,                  &
     &         iphys_nod, nod_fld, FEM_model, ak_MHD, FEM_SGS,          &
     &         SGS_MHD_wk, range, fem_ucd, fem_sq, label_sim)
!
      use t_boundary_field_IO
!
      use initialize_4_snapshot
      use FEM_MHD_ucd_data
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
!
      type(mesh_data), intent(inout) :: femmesh
      type(element_geometry), intent(inout) :: ele_mesh
      type(phys_address), intent(inout) :: iphys_nod
      type(phys_data), intent(inout) :: nod_fld
      type(FEM_MHD_model_data), intent(inout) :: FEM_model
      type(coefs_4_MHD_type), intent(inout) :: ak_MHD
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(maximum_informations), intent(inout) :: range
      type(ucd_file_data), intent(inout) :: fem_ucd
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      character(len=kchara), intent(inout)   :: label_sim
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'init_analyzer_snap'
      call init_analyzer_snap(MHD_files,                                &
     &   FEM_model%FEM_prm, FEM_SGS%SGS_par, FEM_model%bc_FEM_IO,       &
     &   MHD_step, femmesh%mesh, femmesh%group, ele_mesh,               &
     &   FEM_model%MHD_mesh,  FEM_SGS%FEM_filters, FEM_model%MHD_prop,  &
     &   ak_MHD, FEM_model%MHD_BC, FEM_model%FEM_MHD_BCs,               &
     &   FEM_SGS%Csims, iphys_nod, nod_fld, SNAP_time_IO,               &
     &   MHD_step%rst_step, SGS_MHD_wk, fem_sq, label_sim)
!
      call output_grd_file_w_org_connect                                &
     &   (MHD_step%ucd_step, femmesh%mesh, FEM_model%MHD_mesh, nod_fld, &
     &    MHD_files%ucd_file_IO, fem_ucd)
!
      call alloc_phys_range(nod_fld%ntot_phys_viz, range)
!
      end subroutine FEM_initialize_snapshot
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_snapshot(i_step, MHD_files,                &
     &          femmesh, ele_mesh, iphys_nod, FEM_model, ak_MHD,        &
     &          MHD_step, visval, FEM_SGS, SGS_MHD_wk,                  &
     &          nod_fld, fem_ucd, fem_sq)
!
      use m_fem_mhd_restart
!
      use nod_phys_send_recv
      use lead_physical_values
      use update_after_evolution
      use FEM_MHD_evolution
      use chenge_step_4_dynamic
      use copy_nodal_fields
      use input_control
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
      type(phys_address), intent(in) :: iphys_nod
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
!
      integer(kind=kint ), intent(inout) :: visval
      type(MHD_step_param), intent(inout) :: MHD_step
!
      type(FEM_MHD_model_data), intent(inout) :: FEM_model
      type(phys_data), intent(inout) :: nod_fld
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!
      type(ucd_file_data), intent(inout) :: fem_ucd
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
!     ---- Load field data --- 
!
      call reset_update_flag(nod_fld,                                   &
     &    FEM_SGS%Csims%sgs_coefs, FEM_SGS%Csims%diff_coefs)
      MHD_step%flex_p%istep_max_dt = i_step
      if(my_rank.eq.0) write(*,*)                                       &
     &    'step: ', MHD_step%flex_p%istep_max_dt
!
      if (MHD_step%rst_step%increment .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'input_restart_4_snapshot'
        call input_restart_4_snapshot                                   &
     &     (MHD_step%flex_p%istep_max_dt, MHD_files%fst_file_IO,        &
     &      femmesh%mesh%node, nod_fld, SNAP_time_IO,                   &
     &      MHD_step%rst_step)
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
      if (iflag_debug.eq.1)  write(*,*) 'update_FEM_fields'
      call update_FEM_fields(MHD_step%time_d,                           &
     &    FEM_model%FEM_prm, FEM_SGS%SGS_par,                           &
     &    femmesh, ele_mesh, FEM_model%MHD_mesh, FEM_model%FEM_MHD_BCs, &
     &    iphys_nod, FEM_SGS%FEM_filters, SGS_MHD_wk,                   &
     &    nod_fld, FEM_SGS%Csims)
!
!     ----- Evaluate model coefficients
!
      call cal_FEM_model_coefficients                                   &
     &   (MHD_step%time_d, FEM_model%FEM_prm, FEM_SGS%SGS_par,          &
     &    femmesh, ele_mesh, FEM_model%MHD_mesh, FEM_model%MHD_prop,    &
     &    FEM_model%FEM_MHD_BCs, iphys_nod, FEM_SGS%FEM_filters,        &
     &    SGS_MHD_wk, nod_fld, FEM_SGS%Csims)
!
!     ========  Data output
!
      call lead_fields_by_FEM(MHD_step%flex_p%istep_max_dt,             &
     &    MHD_step, FEM_model%FEM_prm, FEM_SGS%SGS_par,                 &
     &    femmesh, ele_mesh, FEM_model%MHD_mesh, FEM_model%MHD_prop,    &
     &    FEM_model%FEM_MHD_BCs, iphys_nod, ak_MHD,                     &
     &    FEM_SGS%FEM_filters, SGS_MHD_wk, nod_fld, FEM_SGS%Csims)
!
!     -----Output monitor date
!
      call start_elapsed_time(4)
!
      call output_time_step_control                                     &
     &   (MHD_step%flex_p%istep_max_dt, MHD_step%rms_step,              &
     &    FEM_model%FEM_prm, MHD_step%time_d, femmesh%mesh,             &
     &    FEM_model%MHD_mesh, FEM_model%MHD_prop,                       &
     &    iphys_nod, nod_fld, SGS_MHD_wk%iphys_ele,                     &
     &    SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int%jcs,                   &
     &    fem_sq%i_rms, fem_sq%j_ave, fem_sq%i_msq,                     &
     &    SGS_MHD_wk%rhs_mat, SGS_MHD_wk%mhd_fem_wk, fem_sq%msq)
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
     &    MHD_step%time_d, MHD_step%ucd_step, fem_ucd)
!
!     ----
!
      if(MHD_step%flex_p%iflag_flexible_step .eq. iflag_flex_step) then
        visval = viz_file_step_4_flex(MHD_step%time_d,                  &
     &                                MHD_step%viz_step)
      else
        visval = viz_file_step_4_fix(MHD_step%flex_p%istep_max_dt,      &
     &                               MHD_step%viz_step)
      end if
!
      call end_elapsed_time(4)
!
      end subroutine FEM_analyze_snapshot
!
! ----------------------------------------------------------------------
!
      subroutine FEM_finalize_snapshot                                  &
     &         (MHD_files, MHD_step, range, fem_ucd)
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
!
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
      end subroutine FEM_finalize_snapshot
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_snapshot
