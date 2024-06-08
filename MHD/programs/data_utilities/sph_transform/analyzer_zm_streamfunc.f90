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
!
      use SPH_analyzer_back_trans_old
      use FEM_analyzer_back_trans
      use SPH_analyzer_zm_streamfunc
      use t_four_visualizers
      use t_VIZ_mesh_field
      use t_mesh_SR
!
      implicit none
!
      character (len = kchara), parameter, private                      &
     &                :: fname_sph_trns_ctl = 'ctl_sph_transform'
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
      use t_SPH_mesh_field_data
      use m_elapsed_labels_4_VIZ
      use FEM_to_VIZ_bridge
      use input_controls_sph_trans
!
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_VIZ
      call elpsed_label_field_send_recv
!
!   ----  read controls
!
      if (iflag_debug.gt.0) write(*,*) 'load_control_data_sph_trans'
      call load_control_data_sph_trans(fname_sph_trns_ctl, spt_ctl1)
      call set_ctl_data_4_zm_streamline(spt_ctl1%fld_ctl%field_ctl)
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans(spt_ctl1, t_STR, SPH_TRNS,        &
     &                                FEM_STR1, SPH_STR1)
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh                                   &
     &   (SPH_STR1%FEM_mesh_flags, SPH_STR1%sph_file_param, SPH_TRNS,   &
     &    FEM_STR1%geofem, FEM_STR1%mesh_file_IO)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans(t_STR%ucd_step, FEM_STR1, m_SR5)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_back_trans'
      call SPH_initialize_back_trans                                    &
     &   (t_STR%init_d%i_time_step, SPH_TRNS, SPH_STR1,                 &
     &    FEM_STR1%time_IO, m_SR5%SR_sig, m_SR5%SR_r)
!
!
!  -------------------------------------------
!  ----   Mesh setting for visualization -----
!  -------------------------------------------
      if(iflag_debug .gt. 0) write(*,*) 'init_FEM_to_VIZ_bridge'
      call init_FEM_to_VIZ_bridge(FEM_STR1%viz_step, FEM_STR1%geofem,   &
     &                            VIZ_D_STR1, m_SR5)
!
!  ------  initialize visualization
      if (iflag_debug.gt.0) write(*,*) 'init_visualize'
      call init_four_visualize                                          &
     &   (FEM_STR1%viz_step, FEM_STR1%geofem, FEM_STR1%field,           &
     &    VIZ_D_STR1, spt_ctl1%viz4_ctls, FEM_STR1%four_vizs, m_SR5)
      call dealloc_viz4_controls(spt_ctl1%viz4_ctls)
!
      end subroutine init_zm_streamfunc
!
! ----------------------------------------------------------------------
!
      subroutine analyze_zm_streamfunc
!
      use FEM_to_VIZ_bridge
!
      integer(kind=kint ) :: i_step
      logical :: visval
!
!
      do i_step = t_STR%init_d%i_time_step, t_STR%finish_d%i_end_step
        if (iflag_debug.gt.0) write(*,*) 'step ', i_step, 'start...'
!
        if(      iflag_vizs_w_fix_step(i_step, FEM_STR1%viz_step)       &
     &    .or. output_IO_flag(i_step, t_STR%ucd_step)) then
          call SPH_analyze_zm_streamfunc(i_step, FEM_STR1%geofem,       &
     &        SPH_TRNS, SPH_STR1, FEM_STR1%time_IO, FEM_STR1%field,     &
     &        m_SR5%SR_sig, m_SR5%SR_r)
        end if
!
        visval = iflag_vizs_w_fix_step(i_step, FEM_STR1%viz_step)
        call FEM_analyze_back_trans(i_step, t_STR%ucd_step, visval,     &
     &                              FEM_STR1, m_SR5)
!
        if(visval) then
          call istep_viz_w_fix_dt(i_step, FEM_STR1%viz_step)
          call visualize_four                                           &
     &       (FEM_STR1%viz_step, t_STR%time_d, FEM_STR1%geofem,         &
     &        FEM_STR1%field, VIZ_D_STR1, FEM_STR1%four_vizs, m_SR5)
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
