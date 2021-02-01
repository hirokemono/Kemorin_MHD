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
      use t_SPH_mesh_field_data
      use m_elapsed_labels_4_VIZ
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
      call s_set_ctl_data_4_sph_trans(spt_ctl1, t_STR, SPH_TRNS,        &
     &                                FEM_STR1, SPH_STR1)
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh                                   &
     &   (SPH_STR1%FEM_mesh_flags, SPH_STR1%sph_file_param, SPH_TRNS,   &
     &    FEM_STR1%geofem, FEM_STR1%mesh_file_IO)
!
!  ------  initialize FEM data
!
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans(t_STR%ucd_step, FEM_STR1)
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'SPH_init_gauss_back_trans'
      call SPH_init_gauss_back_trans(SPH_TRNS, SPH_STR1)
!
      call init_visualize(FEM_STR1%geofem, FEM_STR1%field,              &
     &                    spt_ctl1%viz_ctls, FEM_STR1%vizs)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      integer(kind = kint) :: i_step
      logical :: visval
!
!
      do i_step = t_STR%init_d%i_time_step, t_STR%finish_d%i_end_step
        if (iflag_debug.gt.0) write(*,*) 'step ', i_step, 'start...'
!
        if(    iflag_vizs_w_fix_step(i_step, FEM_STR1%viz_step)         &
     &    .or. output_IO_flag(i_step, t_STR%ucd_step)) then
          call SPH_analyze_gauss_back_trans                             &
     &       (i_step, FEM_STR1%geofem, SPH_TRNS,                        &
     &        SPH_STR1, FEM_STR1%field)
        end if
!
        visval = iflag_vizs_w_fix_step(i_step, FEM_STR1%viz_step)
        call FEM_analyze_back_trans                                     &
     &     (i_step, t_STR%ucd_step, visval, FEM_STR1)
!
        if(visval) then
          call istep_viz_w_fix_dt(i_step, FEM_STR1%viz_step)
          call visualize_all(FEM_STR1%viz_step, t_STR%time_d,           &
     &        FEM_STR1%geofem, FEM_STR1%field, FEM_STR1%ele_4_nod,      &
     &        FEM_STR1%jacobians, FEM_STR1%vizs)
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
