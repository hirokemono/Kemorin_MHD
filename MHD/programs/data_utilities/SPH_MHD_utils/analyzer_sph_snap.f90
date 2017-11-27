!>@file   analyzer_sph_snap.f90
!!@brief  module analyzer_sph_snap
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!
!!@verbatim
!!      subroutine initialize_sph_snap
!!      subroutine evolution_sph_snap
!!      subroutine evolution_sph_snap_badboy
!!@endverbatim
!
      module analyzer_sph_snap
!
      use m_precision
      use calypso_mpi
!
      use m_work_time
      use m_machine_parameter
      use m_MHD_step_parameter
      use m_SPH_MHD_model_data
      use m_jacobians_VIZ
      use m_SPH_SGS_structure
      use t_step_parameter
      use t_visualizer
!
      use SPH_analyzer_snap
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &                      :: snap_ctl_name = 'control_snapshot'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_snap
!
      use t_ctl_data_SGS_MHD
      use m_ctl_data_sph_SGS_MHD
!
      use init_sph_MHD_elapsed_label
      use FEM_analyzer_sph_MHD_w_viz
      use input_control_sph_SGS_MHD
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      total_start = MPI_WTIME()
      call set_sph_MHD_elapsed_label
!
!   Load parameter file
!
      call start_elapsed_time(1)
      call start_elapsed_time(4)
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_SGS_MHD'
      call read_control_4_sph_SGS_MHD(snap_ctl_name, MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_dynamo'
      call input_control_SPH_dynamo                                     &
     &  (MHD_files1, MHD_ctl1, SPH_SGS1, MHD_step1, SPH_model1,         &
     &   SPH_WK1%trns_WK, SPH_WK1%monitor, SPH_MHD1, FEM_d1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      call end_elapsed_time(4)
!
!     --------------------- 
!
      call start_elapsed_time(2)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_w_viz'
      call FEM_initialize_w_viz                                         &
     &   (MHD_files1, MHD_step1, FEM_d1%geofem, FEM_d1%ele_mesh,        &
     &    FEM_d1%iphys, FEM_d1%field, next_tbl_VIZ1, jacobians_VIZ1,    &
     &    MHD_IO1)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_snap'
      call SPH_init_sph_snap(MHD_files1, FEM_d1%iphys,                  &
     &    SPH_model1, SPH_SGS1, SPH_MHD1, SPH_WK1)
!        Initialize visualization
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize'
      call init_visualize(FEM_d1%geofem, FEM_d1%ele_mesh, FEM_d1%field, &
     &    MHD_ctl1%viz_ctls, vizs1)
!
      call calypso_MPI_barrier
      call end_elapsed_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_snap
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_snap
!
      use FEM_analyzer_sph_MHD
      use FEM_analyzer_sph_SGS_MHD
      use output_viz_file_control
!
      integer(kind = kint) :: visval
      integer(kind = kint) :: iflag
!
!*  -----------  set initial step data --------------
!*
      call start_elapsed_time(3)
      call s_initialize_time_step(MHD_step1%init_d, MHD_step1%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(MHD_step1%time_d)
!
        iflag = output_IO_flag(MHD_step1%time_d%i_time_step,            &
     &                         MHD_step1%rst_step)
        if(iflag .ne. 0) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_snap'
        call SPH_analyze_snap(MHD_step1%time_d%i_time_step,             &
     &      MHD_files1, SPH_model1, MHD_step1,                          &
     &      SPH_SGS1, SPH_MHD1, SPH_WK1)
!*
!*  -----------  output field data --------------
!*
        call start_elapsed_time(1)
        call start_elapsed_time(4)
!
        iflag = lead_field_data_flag(MHD_step1%time_d%i_time_step,      &
     &                               MHD_step1)
        if(iflag .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_SGS_MHD'
          call SPH_to_FEM_bridge_SGS_MHD                                &
     &       (SPH_SGS1%SGS_par, SPH_MHD1%sph, SPH_WK1%trns_WK,          &
     &        FEM_d1%geofem, FEM_d1%iphys, FEM_d1%field)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(MHD_files1,                            &
     &      FEM_d1%geofem, FEM_d1%field, MHD_step1, visval, MHD_IO1)
!
        call end_elapsed_time(4)
!
!*  ----------- Visualization --------------
!*
        if(visval .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_all'
          call start_elapsed_time(12)
          call visualize_all(MHD_step1%viz_step, MHD_step1%time_d,      &
     &        FEM_d1%geofem, FEM_d1%ele_mesh, FEM_d1%field,             &
     &        next_tbl_VIZ1%neib_ele, jacobians_VIZ1, vizs1)
          call end_elapsed_time(12)
        end if
        call end_elapsed_time(1)
!
!*  -----------  exit loop --------------
!*
        if(MHD_step1%time_d%i_time_step                                 &
     &        .ge. MHD_step1%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      call end_elapsed_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_files1, MHD_step1, MHD_IO1)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
!      call SPH_finalize_snap
!
      call copy_COMM_TIME_to_elaps(num_elapsed)
      call end_elapsed_time(1)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_snap
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_snap_badboy
!
      use m_ctl_data_sph_SGS_MHD
      use t_control_data_vizs
      use t_volume_rendering
      use FEM_analyzer_sph_MHD
      use FEM_analyzer_sph_SGS_MHD
      use output_viz_file_control
!
      integer(kind = kint) :: visval
      integer(kind = kint) :: iflag
!
      real(kind = kreal) :: total_max, total_prev
!
!     ---------------------
!
      MHD_step1%rms_step%increment = 0
      MHD_step1%ucd_step%increment = 0
      if(MHD_step1%finish_d%elapsed_time .gt. 1800.0) then
        if (my_rank.eq.0) write(*,*) 'This code can use up to 30 min.'
        MHD_step1%finish_d%elapsed_time = 1800.0
      else if(MHD_step1%finish_d%elapsed_time .lt. 0.0d0) then
        MHD_step1%finish_d%elapsed_time = 1800.0
      end if
!
      call start_elapsed_time(3)
!
!*  ----------- Read spectr data and get field data --------------
!*
      MHD_step1%time_d%i_time_step = MHD_step1%init_d%i_time_step
      if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_snap'
      call SPH_analyze_snap(MHD_step1%time_d%i_time_step,               &
     &    MHD_files1, SPH_model1, MHD_step1,                            &
     &    SPH_SGS1, SPH_MHD1, SPH_WK1)
!*
      iflag = lead_field_data_flag(MHD_step1%time_d%i_time_step,        &
     &                             MHD_step1)
      if(iflag .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_SGS_MHD'
        call SPH_to_FEM_bridge_SGS_MHD                                  &
     &     (SPH_SGS1%SGS_par, SPH_MHD1%sph, SPH_WK1%trns_WK,            &
     &      FEM_d1%geofem, FEM_d1%iphys, FEM_d1%field)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
      call FEM_analyze_sph_MHD(MHD_files1,                              &
     &    FEM_d1%geofem, FEM_d1%field, MHD_step1, visval, MHD_IO1)
      call end_elapsed_time(4)
!
      if(visval .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'visualize_all'
        call start_elapsed_time(12)
        call visualize_all(MHD_step1%viz_step, MHD_step1%time_d,        &
     &      FEM_d1%geofem, FEM_d1%ele_mesh, FEM_d1%field,               &
     &      next_tbl_VIZ1%neib_ele, jacobians_VIZ1, vizs1)
        call deallocate_pvr_data(vizs1%pvr)
        call end_elapsed_time(12)
      end if
!
!*  ----------- Visualization --------------
!*
      do
        visval = check_PVR_update                                       &
     &         (MHD_ctl1%viz_ctls%pvr_ctls, vizs1%pvr)
        call calypso_mpi_barrier
!
        if(visval .eq. IFLAG_TERMINATE) then
          if (my_rank.eq.0) write(*,*) 'end flag is recieved'
          exit
!
        else if(visval .eq. IFLAG_UPDATE) then
          if (my_rank.eq.0) then
            write(*,*) 'visualization start!'
            write(*,*) 'Current elapsed time: ', total_time
          end if
!
          call start_elapsed_time(12)
          call PVR_initialize                                           &
     &       (FEM_d1%geofem%mesh, FEM_d1%geofem%group, FEM_d1%ele_mesh, &
     &        FEM_d1%field, MHD_ctl1%viz_ctls%pvr_ctls, vizs1%pvr)
          call calypso_MPI_barrier
          call PVR_visualize(MHD_step1%viz_step%PVR_t%istep_file,       &
     &        FEM_d1%geofem%mesh, FEM_d1%geofem%group, FEM_d1%ele_mesh, &
     &        jacobians_VIZ1, FEM_d1%field, vizs1%pvr)
          call deallocate_pvr_data(vizs1%pvr)
          call end_elapsed_time(12)
        end if
        call end_elapsed_time(1)
!
        total_prev = total_time
        total_time = MPI_WTIME() - total_start
!
        if(my_rank .eq. 0) then
          if(int(total_time/60.0) .ne. int(total_prev/60.0)) then
            write(*,*) int(total_time/60), 'minuts passed'
          end if
        end if
!
        if(total_time .gt. MHD_step1%finish_d%elapsed_time) then
          call calypso_mpi_barrier
          call MPI_allREDUCE (total_time, total_max, ione,              &
     &       CALYPSO_REAL, MPI_MAX, CALYPSO_COMM, ierr_MPI)
          exit
        end if
      end do
      call end_elapsed_time(3)
!
  10   continue
!    Loop end
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_files1, MHD_step1, MHD_IO1)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
!      call SPH_finalize_snap
!
      call copy_COMM_TIME_to_elaps(num_elapsed)
      call end_elapsed_time(1)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_snap_badboy
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_snap
