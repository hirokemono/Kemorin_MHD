!analyzer_zm_streamfunc.f90
!      module analyzer_zm_streamfunc
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine init_zm_streamfunc
!      subroutine analyze_zm_streamfunc
!
      module analyzer_zm_streamfunc
!
      use m_precision
      use calypso_mpi
      use m_SPH_transforms
      use m_work_time
      use m_elapsed_labels_SEND_RECV
      use m_spheric_data_transform
!
      use SPH_analyzer_back_trans_old
      use FEM_analyzer_back_trans
      use SPH_analyzer_zm_streamfunc
      use t_visualizer
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_zm_streamfunc
!
      use t_ctl_params_sph_trans
      use t_check_and_make_SPH_mesh
      use m_elapsed_labels_4_VIZ
!
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_VIZ
      call elpsed_label_field_send_recv
!
!   ----  read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans(spt_ctl1)
      call set_ctl_data_4_zm_streamline(spt_ctl1%fld_ctl%field_ctl)
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans                                   &
     &   (spt_ctl1, t_STR, viz_step_STR, files_STR, SPH_TRNS%fld,       &
     &    d_gauss_trans, field_STR, trns_param,                         &
     &    WK_leg_TRNS, sph_maker_TRNS)
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh                                   &
     &   (files_STR%FEM_mesh_flags, files_STR%sph_file_param,           &
     &    SPH_TRNS%sph, SPH_TRNS%comms, SPH_TRNS%groups,                &
     &    femmesh_STR, files_STR%mesh_file_IO, sph_maker_TRNS)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans                                    &
     &   (files_STR%ucd_file_IO, viz_step_STR, ele_4_nod_SPH_TRANS,     &
     &    jacobians_STR, ucd_SPH_TRNS)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_back_trans'
      call SPH_initialize_back_trans(files_STR, trns_param,             &
     &    SPH_TRNS, ipol_LES_TRNS, time_IO_TRNS, sph_trns_IO)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_visualize'
      call init_visualize                                               &
     &   (femmesh_STR, field_STR, spt_ctl1%viz_ctls, vizs_TRNS)
!
      end subroutine init_zm_streamfunc
!
! ----------------------------------------------------------------------
!
      subroutine analyze_zm_streamfunc
!
      integer(kind=kint ) :: i_step
      logical :: visval
!
!
      do i_step = t_STR%init_d%i_time_step, t_STR%finish_d%i_end_step
        if (iflag_debug.gt.0) write(*,*) 'step ', i_step, 'start...'
!
        call SPH_analyze_zm_streamfunc(i_step, files_STR, viz_step_STR, &
     &      trns_param, SPH_TRNS, time_IO_TRNS, sph_trns_IO)
!
        visval = iflag_vizs_w_fix_step(i_step, viz_step_STR)
        call FEM_analyze_back_trans(files_STR%ucd_file_IO,              &
     &      time_IO_TRNS, ucd_SPH_TRNS, i_step, visval)
!
        if(visval) then
          call istep_viz_w_fix_dt(i_step, viz_step_STR)
          call visualize_all(viz_step_STR, t_STR%time_d,                &
     &        femmesh_STR, field_STR, ele_4_nod_SPH_TRANS,              &
     &        jacobians_STR, vizs_TRNS)
        end if
      end do
!
      call output_elapsed_times
!
      end subroutine analyze_zm_streamfunc
!
! ----------------------------------------------------------------------
!
      end module analyzer_zm_streamfunc
