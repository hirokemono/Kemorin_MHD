!FEM_analyzer_vol_average.f90
!      module FEM_analyzer_vol_average
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_initialize_vol_average                           &
!!     &         (MHD_files, MHD_step, FEM_model, ak_MHD,               &
!!     &          FEM_MHD, FEM_SGS, SGS_MHD_wk, MHD_IO, fem_sq, m_SR)
!!        type(FEM_MHD_model_data), intent(inout) :: FEM_model
!!        type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!!        type(FEM_SGS_structure), intent(inout) :: FEM_SGS
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine FEM_analyze_vol_average(i_step, MHD_files, iphys_LES,&
!!     &         FEM_model, MHD_step, SGS_MHD_wk, FEM_MHD, fem_sq, m_SR)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(mesh_SR), intent(inout) :: m_SR
!
      module FEM_analyzer_vol_average
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use t_time_data
      use t_FEM_mesh_field_data
      use t_SGS_model_addresses
      use t_material_property
      use t_FEM_MHD_model_data
      use t_MHD_file_parameter
      use t_MHD_step_parameter
      use t_ucd_file
      use t_FEM_MHD_mean_square
      use t_FEM_SGS_structure
      use t_work_FEM_SGS_MHD
      use t_MHD_IO_data
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
      subroutine FEM_initialize_vol_average                             &
     &         (MHD_files, MHD_step, FEM_model, ak_MHD,                 &
     &          FEM_MHD, FEM_SGS, SGS_MHD_wk, MHD_IO, fem_sq, m_SR)
!
      use t_boundary_field_IO
!
      use initialize_4_snapshot
!
      use open_sgs_model_coefs
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
!
      type(FEM_MHD_model_data), intent(inout) :: FEM_model
      type(coefs_4_MHD_type), intent(inout) :: ak_MHD
      type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(mesh_SR), intent(inout) :: m_SR
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'init_analyzer_snap'
      call init_analyzer_snap(MHD_files, FEM_model%FEM_prm,             &
     &   FEM_SGS%SGS_par, FEM_model%bc_FEM_IO,                          &
     &   MHD_step, FEM_MHD%geofem, FEM_model%MHD_mesh,                  &
     &   FEM_SGS%FEM_filters, FEM_model%MHD_prop, ak_MHD,               &
     &   FEM_model%MHD_BC, FEM_model%FEM_MHD_BCs, FEM_SGS%Csims,        &
     &   FEM_MHD%iref_base, FEM_MHD%iref_grad, FEM_MHD%ref_fld,         &
     &   FEM_MHD%iphys, FEM_SGS%iphys_LES, FEM_MHD%field, SNAP_time_IO, &
     &   MHD_step%rst_step, SGS_MHD_wk, fem_sq, MHD_IO%rst_IO,          &
     &   m_SR, FEM_MHD%label_sim)
!
      end subroutine FEM_initialize_vol_average
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_vol_average(i_step, MHD_files, iphys_LES,  &
     &         FEM_model, MHD_step, SGS_MHD_wk, FEM_MHD, fem_sq, m_SR)
!
      use t_FEM_MHD_mean_square
      use nod_phys_send_recv
      use lead_physical_values
      use copy_nodal_fields
      use input_control
!
      use output_parallel_ucd_file
      use FEM_MHD_evolution
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(FEM_MHD_model_data), intent(in) :: FEM_model
!
      type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(MHD_step_param), intent(inout) :: MHD_step
      type(mesh_SR), intent(inout) :: m_SR
!
!     ---- Load field data --- 
!
      if (my_rank.eq.0) write(*,*) 'step: ', i_step
!
      if (iflag_debug.eq.1)  write(*,*) 'read_udt_4_snap'
      call read_udt_4_snap(i_step, MHD_step%ucd_step,                   &
     &    MHD_files%org_ucd_file_IO, FEM_MHD%field, SNAP_time_IO)
      MHD_step%time_d%time = MHD_step%init_d%time                       &
     &                      + MHD_step%time_d%dt * dble(i_step)
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
!     -----Output monitor date
!
      call output_time_step_control                                     &
     &   (MHD_step%flex_p%istep_max_dt, MHD_step%rms_step,              &
     &    FEM_model%FEM_prm, MHD_step%time_d, FEM_MHD%geofem,           &
     &    FEM_model%MHD_mesh, FEM_model%MHD_prop, FEM_MHD%iphys,        &
     &    iphys_LES, FEM_MHD%field, SGS_MHD_wk%iphys_ele_base,          &
     &    SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int%jcs,                   &
     &    SGS_MHD_wk%rhs_mat, SGS_MHD_wk%mhd_fem_wk, fem_sq)
!
      end subroutine FEM_analyze_vol_average
!
! ----------------------------------------------------------------------
!
!      subroutine FEM_finalize_vol_average
!
!      end subroutine FEM_finalize_vol_average
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_vol_average
