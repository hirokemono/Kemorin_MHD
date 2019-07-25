!analyzer_gauss_back_trans.f90
!      module analyzer_gauss_back_trans
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
      module analyzer_gauss_back_trans
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
      use m_SPH_transforms
      use m_work_time
      use m_elapsed_labels_SEND_RECV
      use m_spheric_data_transform
      use t_ctl_params_sph_trans
!
      use FEM_analyzer_back_trans
      use SPH_analyzer_gauss_b_trans
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
      subroutine init_analyzer
!
      use parallel_load_data_4_sph
      use m_elapsed_labels_4_VIZ
!
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_VIZ
      call elpsed_label_field_send_recv
!
!   -----  read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans(spt_ctl1)
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans                                   &
     &   (spt_ctl1, t_STR, viz_step_STR, files_STR,                     &
     &    SPH_TRNS%fld, d_gauss_trans, field_STR, WK_sph_TRNS)
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh                                   &
     &   (files_STR%FEM_mesh_flags, sph_file_param0,                    &
     &    SPH_TRNS%sph, SPH_TRNS%comms, SPH_TRNS%groups,                &
     &    femmesh_STR, files_STR%mesh_file_IO, gen_sph_TRNS)
!
!  ------  initialize FEM data
!
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans                                    &
     &   (files_STR%ucd_file_IO, viz_step_STR, ele_4_nod_SPH_TRANS,     &
     &    jacobians_STR, ucd_SPH_TRNS, m_ucd_SPH_TRNS)
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'SPH_init_gauss_back_trans'
      call SPH_init_gauss_back_trans(files_STR, SPH_TRNS)
!
      call init_visualize                                               &
     &   (femmesh_STR, field_STR, spt_ctl1%viz_ctls, vizs_TRNS)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      integer(kind=kint ) :: visval, i_step
!
!
      do i_step = t_STR%init_d%i_time_step, t_STR%finish_d%i_end_step
        if (iflag_debug.gt.0) write(*,*) 'step ', i_step, 'start...'
!
        call SPH_analyze_gauss_back_trans                               &
     &     (i_step, viz_step_STR, SPH_TRNS, visval)
!
        call FEM_analyze_back_trans                                     &
     &     (files_STR%ucd_file_IO, time_IO_TRNS, ucd_SPH_TRNS, i_step,  &
     &      viz_step_STR, visval)
!
        if (visval .eq. 0) then
          call visualize_all(viz_step_STR, t_STR%time_d,                &
     &        femmesh_STR, field_STR, ele_4_nod_SPH_TRANS,              &
     &        jacobians_STR, vizs_TRNS)
        end if
      end do
!
      call output_elapsed_times
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_gauss_back_trans
