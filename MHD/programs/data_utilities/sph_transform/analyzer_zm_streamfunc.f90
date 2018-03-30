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
      use m_ctl_data_4_sph_trans
      use t_ctl_params_sph_trans
      use parallel_load_data_4_sph
      use set_viz_time_labels
!
!
      num_elapsed = 80
      call allocate_elapsed_times
      call s_set_viz_time_labels
!
      elapse_labels(num_elapsed) = 'Communication time        '
!
!
!   ----  read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans
      call set_ctl_data_4_zm_streamline(fld_st_ctl%field_ctl)
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans(t_STR, viz_step_STR, files_STR,   &
     &    SPH_TRNS%fld, d_gauss_trans, field_STR, WK_sph_TRNS)
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh(files_STR%FEM_mesh_flags,         &
     &    SPH_TRNS%sph, SPH_TRNS%comms, SPH_TRNS%groups,                &
     &    femmesh_STR%mesh, femmesh_STR%group, elemesh_STR,             &
     &    files_STR%mesh_file_IO, gen_sph_TRNS)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans                                    &
     &   (files_STR%ucd_file_IO, viz_step_STR, ele_4_nod_SPH_TRANS,     &
     &    jacobians_STR, ucd_SPH_TRNS, m_ucd_SPH_TRNS)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_back_trans'
      call SPH_initialize_back_trans(files_STR, SPH_TRNS,               &
     &    time_IO_TRNS, sph_trns_IO)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_visualize'
      call init_visualize(femmesh_STR, elemesh_STR, field_STR,          &
     &    viz_st_ctls, vizs_TRNS)
!
      end subroutine init_zm_streamfunc
!
! ----------------------------------------------------------------------
!
      subroutine analyze_zm_streamfunc
!
      integer(kind=kint ) :: visval, i_step
!
!
      do i_step = t_STR%init_d%i_time_step, t_STR%finish_d%i_end_step
        if (iflag_debug.gt.0) write(*,*) 'step ', i_step, 'start...'
!
        call SPH_analyze_zm_streamfunc(i_step, files_STR, viz_step_STR, &
     &      SPH_TRNS, time_IO_TRNS, sph_trns_IO, visval)
!
        call FEM_analyze_back_trans                                     &
     &     (files_STR%ucd_file_IO, time_IO_TRNS, ucd_SPH_TRNS, i_step,  &
     &      viz_step_STR, visval)
!
        if(visval .eq. 0) then
          call visualize_all(viz_step_STR, t_STR%time_d,                &
     &        femmesh_STR, elemesh_STR, field_STR, ele_4_nod_SPH_TRANS, &
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
