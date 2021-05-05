!analyzer_zonal_rms_sph.f90
!      module analyzer_zonal_rms_sph
!..................................................
!
!      modified by H. Matsui on June, 2012
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_zonal_rms_sph
!
      use m_precision
      use calypso_mpi
!
      use m_SPH_transforms
      use m_work_time
      use m_elapsed_labels_SEND_RECV
!
      use SPH_analyzer_sph_trans
      use SPH_analyzer_back_trans_old
      use FEM_analyzer_sph_trans
      use FEM_analyzer_back_trans
      use t_visualizer
      use t_VIZ_mesh_field
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use t_ctl_params_sph_trans
      use t_SPH_mesh_field_data
      use m_elapsed_labels_4_VIZ
      use FEM_to_VIZ_bridge
!
!     --------------------- 
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_VIZ
      call elpsed_label_field_send_recv
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans(spt_ctl1)
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans(spt_ctl1, t_STR, SPH_TRNS,        &
     &                                FEM_STR1, SPH_STR1, VIZ_D_STR1)
      call set_ctl_data_4_pick_zm(spt_ctl1, FEM_STR1)
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh                                   &
     &   (SPH_STR1%FEM_mesh_flags, SPH_STR1%sph_file_param, SPH_TRNS,   &
     &    FEM_STR1%geofem, FEM_STR1%mesh_file_IO)
!
!    Initialize FEM grid
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans(t_STR%ucd_step, FEM_STR1)
!
!    Initialization for spherical tranform
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_sph_trans'
      call SPH_initialize_sph_trans(SPH_TRNS, SPH_STR1)
!
!    Set field IOP array by spectr fields
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_sph_trans'
      call SPH_to_FEM_bridge_sph_trans(SPH_TRNS%fld, SPH_STR1%fld_IO)
!
!
!  -------------------------------------------
!  ----   Mesh setting for visualization -----
!  -------------------------------------------
      if(iflag_debug .gt. 0) write(*,*) 'init_FEM_to_VIZ_bridge'
      call init_FEM_to_VIZ_bridge(FEM_STR1%viz_step,                    &
     &                            FEM_STR1%geofem, VIZ_D_STR1)
!
!  ------  initialize visualization
      call init_visualize(FEM_STR1%viz_step, FEM_STR1%geofem,           &
     &    FEM_STR1%field, VIZ_D_STR1, spt_ctl1%viz_ctls, FEM_STR1%vizs)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use t_ctl_params_sph_trans
      use sph_rtp_zonal_rms_data
      use coordinate_convert_4_sph
      use FEM_to_VIZ_bridge
!
      integer(kind = kint) :: i_step
      logical :: visval
!
!
      do i_step = t_STR%init_d%i_time_step, t_STR%finish_d%i_end_step
!
!   Input field data
        call FEM_analyze_sph_trans(i_step, t_STR%ucd_step, FEM_STR1)
!
!   Take zonal RMS
        if (iflag_debug.gt.0) write(*,*) 'zonal_rms_all_rtp_field'
        call overwrite_nodal_xyz_2_sph                                  &
     &    (FEM_STR1%geofem%mesh%node, FEM_STR1%field)
        call zonal_rms_all_rtp_field (SPH_TRNS%sph%sph_rtp,             &
     &      FEM_STR1%geofem%mesh%node, FEM_STR1%field)
!
        visval = iflag_vizs_w_fix_step(i_step, FEM_STR1%viz_step)
        call FEM_analyze_back_trans                                     &
     &     (i_step, t_STR%ucd_step, visval, FEM_STR1)
!
        if(visval) then
          call istep_viz_w_fix_dt(i_step, FEM_STR1%viz_step)
!  ------- Transfer to reparitioned mesh
          call visualize_all(FEM_STR1%viz_step, t_STR%time_d,           &
     &                       FEM_STR1%geofem, FEM_STR1%field,           &
     &                       VIZ_D_STR1, FEM_STR1%vizs, FEM_STR1%v_sol)
        end if
      end do
!
      call FEM_finalize_sph_trans(t_STR%ucd_step, FEM_STR1)
      call output_elapsed_times
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_zonal_rms_sph
