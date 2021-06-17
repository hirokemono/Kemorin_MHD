!>@file   FEM_analyzer_sph_SGS_MHD.f90
!!@brief  module FEM_analyzer_sph_SGS_MHD
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2017
!
!>@brief Top routines to transfer spherical harmonics grids data
!!       to FEM data for data visualization
!!
!!@verbatim
!!      subroutine FEM_initialize_sph_SGS_MHD(MHD_files, MHD_step,      &
!!     &          iphys_LES, MHD_IO, FEM_MHD,                           &
!!     &          v_sol, SR_sig, SR_r, SR_i, SR_il)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(MHD_step_param), intent(in) :: MHD_step
!!        type(SGS_model_addresses), intent(inout) :: iphys_LES
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!      subroutine FEM_analyze_sph_SGS_MHD(MHD_files, MHD_step, MHD_IO, &
!!     &                                   FEM_MHD, v_sol, SR_sig, SR_r)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!      subroutine SPH_to_FEM_bridge_SGS_MHD                            &
!!     &        (SGS_par, sph, WK, WK_LES, geofem, nod_fld)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(sph_grids), intent(in) :: sph
!!        type(works_4_sph_trans_MHD), intent(in) :: WK
!!        type(works_4_sph_trans_SGS_MHD), intent(in) :: WK_LES
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(inout) :: nod_fld
!!      subroutine FEM_finalize_sph_SGS_MHD(MHD_files, MHD_step, MHD_IO)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(MHD_step_param), intent(in) :: MHD_step
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!@endverbatim
!!
!!@n @param  i_step       Current time step
!!@n @param  visval       Return flag to call visualization routines
!
      module FEM_analyzer_sph_SGS_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_work_time
!
      use t_time_data
      use t_MHD_step_parameter
      use t_file_IO_parameter
!
      use t_shape_functions
      use t_FEM_mesh_field_data
      use t_vector_for_solver
      use t_solver_SR
      use t_solver_SR_int
      use t_solver_SR_int8
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_sph_SGS_MHD(MHD_files, MHD_step,        &
     &          iphys_LES, MHD_IO, FEM_MHD,                             &
     &          v_sol, SR_sig, SR_r, SR_i, SR_il)
!
      use t_phys_address
      use t_SGS_model_addresses
      use t_VIZ_step_parameter
      use t_MHD_file_parameter
      use t_cal_max_indices
      use t_MHD_IO_data
      use t_ucd_file
!
      use set_table_4_RHS_assemble
      use FEM_analyzer_sph_MHD
      use int_volume_of_domain
      use set_normal_vectors
      use parallel_FEM_mesh_init
      use set_field_data_w_SGS
      use node_monitor_IO
      use nod_phys_send_recv
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
      type(SGS_model_addresses), intent(inout) :: iphys_LES
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      integer(kind = kint) :: iflag
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_local_nod_4_monitor'
      call set_local_nod_4_monitor(FEM_MHD%geofem%mesh,                 &
     &                             FEM_MHD%geofem%group)
!
      if (iflag_debug.gt.0) write(*,*) 'init_field_data_w_SGS'
      call init_field_data_w_SGS(FEM_MHD%geofem%mesh%node%numnod,       &
     &    FEM_MHD%field, FEM_MHD%iphys, iphys_LES)
!
!  -------------------------------
!      INIT communication buffer
!  -------------------------------
!
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_comm_initialization'
      call FEM_comm_initialization(FEM_MHD%geofem%mesh, v_sol,          &
     &                             SR_sig, SR_r, SR_i, SR_il)
!
!  -------------------------------
!  connect grid data to volume output
!  -------------------------------
!
      if(MHD_step%ucd_step%increment .gt. 0) then
        call alloc_phys_range(FEM_MHD%field%ntot_phys_viz,              &
     &                        MHD_IO%range)
      end if
!
      if(iflag_debug .gt. 0) write(*,*) 'output_grd_file_4_snapshot'
      call output_grd_file_4_snapshot(MHD_files%ucd_file_IO,            &
     &    MHD_step%ucd_step, FEM_MHD%geofem%mesh, FEM_MHD%field,        &
     &    MHD_IO%ucd, SR_sig, SR_i)
!
      end subroutine FEM_initialize_sph_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_sph_SGS_MHD(MHD_files, MHD_step, MHD_IO,   &
     &                                   FEM_MHD, v_sol, SR_sig, SR_r)
!
      use FEM_analyzer_sph_MHD
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call FEM_analyze_sph_MHD                                          &
     &   (MHD_files, FEM_MHD%geofem, FEM_MHD%field,                     &
     &    MHD_step, MHD_IO, v_sol, SR_sig, SR_r)
!
      end subroutine FEM_analyze_sph_SGS_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_SGS_MHD                              &
     &        (SGS_par, sph, WK, WK_LES, geofem, nod_fld)
!
      use t_spheric_parameter
      use t_sph_trans_arrays_MHD
      use t_sph_trans_arrays_SGS_MHD
      use t_SGS_control_parameter
!
      use set_address_sph_trans_snap
      use SGS_MHD_fields_to_FEM
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_grids), intent(in) :: sph
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(works_4_sph_trans_SGS_MHD), intent(in) :: WK_LES
      type(mesh_data), intent(in) :: geofem
!
      type(phys_data), intent(inout) :: nod_fld
!*
!*  -----------  data transfer to FEM array --------------
!*
      if (iflag_debug.gt.0) write(*,*) 'copy_force_from_transform MHD'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_MHD%forward, geofem%mesh, nod_fld)
!
      if(SGS_par%model_p%iflag_SGS .gt. 0) then
        call copy_SGS_MHD_fld_from_trans                                &
     &     (sph, WK_LES, geofem%mesh, nod_fld)
      end if
!
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                'copy_field_from_transform base fields'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_snap%backward, geofem%mesh, nod_fld)
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                'copy_field_from_transform diff_vector'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_difv%backward, geofem%mesh, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_field_from_transform SNAP'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_eflux%backward, geofem%mesh, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_force_from_transform SNAP'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_eflux%forward, geofem%mesh, nod_fld)
!
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                    'copy_force_from_transform filter_MHD'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK_LES%trns_fil_MHD%forward, geofem%mesh, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                'copy_field_from_transform base fields'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK_LES%trns_fil_snap%backward, geofem%mesh, nod_fld)
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                'copy_field_from_transform filtered_diff_vector'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK_LES%trns_fil_difv%backward, geofem%mesh, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                    'copy_field_from_transform SGS_SNAP'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK_LES%trns_SGS_snap%backward, geofem%mesh, nod_fld)
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                     'copy_force_from_transform SGS_SNAP'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK_LES%trns_SGS_snap%forward, geofem%mesh, nod_fld)
!
      end subroutine SPH_to_FEM_bridge_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine FEM_finalize_sph_SGS_MHD(MHD_files, MHD_step, MHD_IO)
!
      use FEM_analyzer_sph_MHD
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
!
      type(MHD_IO_data), intent(inout) :: MHD_IO
!
!
      call FEM_finalize(MHD_files, MHD_step, MHD_IO)
!
      end subroutine FEM_finalize_sph_SGS_MHD
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_SGS_MHD
