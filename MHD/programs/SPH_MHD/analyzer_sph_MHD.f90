!>@file   analyzer_sph_MHD.f90
!!@brief  module analyzer_sph_MHD
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine initialize_sph_mhd
!!      subroutine evolution_sph_mhd
!!@endverbatim
!
      module analyzer_sph_MHD
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use m_SPH_MHD_model_data
      use m_MHD_step_parameter
      use m_SPH_SGS_structure
      use t_ctl_data_SGS_MHD
      use t_SPH_mesh_field_data
      use t_visualizer
      use t_SPH_MHD_zonal_mean_viz
      use t_VIZ_mesh_field
      use t_sph_trans_arrays_MHD
!
      use SPH_analyzer_SGS_MHD
      use FEM_analyzer_sph_SGS_MHD
      use init_sph_MHD_elapsed_label
!
      implicit none
!
!>      File name for control file
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
!>      Control struture for MHD simulation
      type(sph_sgs_mhd_control), save :: MHD_ctl1
!
      private :: MHD_ctl_name, MHD_ctl1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_mhd
!
      use input_control_sph_SGS_MHD
      use FEM_to_VIZ_bridge
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      MHD_step1%finish_d%started_time = MPI_WTIME()
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if(iflag_debug.eq.1) write(*,*) 'read_control_4_sph_SGS_MHD'
      call read_control_4_sph_SGS_MHD(MHD_ctl_name, MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_SGS_dynamo'
      call input_control_SPH_SGS_dynamo                                 &
     &    (MHD_files1, MHD_ctl1, MHD_step1, SPH_model1,                 &
     &     SPH_WK1, SPH_SGS1, SPH_MHD1, FEM_d1, VIZ_DAT1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!    IO elapsed end
!    precondition elaps start
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
!
!        Initialize FEM mesh data for field data IO
!
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_SGS_MHD'
      call FEM_initialize_sph_SGS_MHD(MHD_files1, MHD_step1,            &
     &    SPH_SGS1%iphys_LES, MHD_IO1, FEM_d1)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_SGS_MHD'
      call SPH_initialize_SGS_MHD                                       &
     &   (MHD_files1, FEM_d1%iphys, MHD_step1, MHD_IO1%rst_IO,          &
     &    SPH_model1, SPH_SGS1, SPH_MHD1, SPH_WK1)
!
!  -------------------------------------------
!  ----   Mesh setting for visualization -----
!  -------------------------------------------
      if(iflag_debug .gt. 0) write(*,*) 'init_FEM_to_VIZ_bridge'
      call init_FEM_to_VIZ_bridge(MHD_step1%viz_step,                   &
     &    FEM_d1%geofem, FEM_d1%field, VIZ_DAT1)
!
!        Initialize visualization
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize'
      call init_visualize(VIZ_DAT1%viz_fem, VIZ_DAT1%edge_comm,         &
     &    VIZ_DAT1%viz_fld, MHD_ctl1%viz_ctls, vizs1)
      call init_zonal_mean_sections(FEM_d1%geofem, VIZ_DAT1%edge_comm,  &
     &    FEM_d1%field, MHD_ctl1%zm_ctls, zmeans1)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mhd
!
      use FEM_analyzer_sph_SGS_MHD
      use SGS_MHD_zonal_mean_viz
      use output_viz_file_control
      use t_sph_trans_arrays_SGS_MHD
      use FEM_to_VIZ_bridge
!
      integer(kind = kint) :: iflag_finish
!
!     ---------------------
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
!
!*  -----------  set initial step data --------------
!*
      call copy_time_step_data(MHD_step1%init_d, MHD_step1%time_d)
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        call evolve_time_data(MHD_step1%time_d)
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if(lead_field_data_flag(MHD_step1%time_d%i_time_step,           &
     &                          MHD_step1)) then
          call alloc_sph_trans_area_snap                                &
     &       (SPH_MHD1%sph%sph_rtp, SPH_WK1%trns_WK)
          call alloc_SGS_sph_trns_area_snap                             &
     &       (SPH_MHD1%sph%sph_rtp, SPH_SGS1%trns_WK_LES)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_SGS_MHD'
        call SPH_analyze_SGS_MHD(MHD_step1%time_d%i_time_step,          &
     &      MHD_files1, iflag_finish, SPH_model1,                       &
     &      MHD_step1, MHD_IO1%rst_IO, SPH_SGS1, SPH_MHD1, SPH_WK1)
!*
!*  -----------  output field data --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
        if(lead_field_data_flag(MHD_step1%time_d%i_time_step,           &
     &                          MHD_step1)) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_SGS_MHD'
          call SPH_to_FEM_bridge_SGS_MHD                                &
     &       (SPH_SGS1%SGS_par, SPH_MHD1%sph, SPH_WK1%trns_WK,          &
     &        SPH_SGS1%trns_WK_LES, FEM_d1%geofem, FEM_d1%field)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_SGS_MHD'
        call FEM_analyze_sph_SGS_MHD                                    &
     &     (MHD_files1, MHD_step1, MHD_IO1, FEM_d1)
!
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!*  ----------- Visualization --------------
!*
        if(iflag_vizs_w_fix_step(MHD_step1%time_d%i_time_step,          &
     &                           MHD_step1%viz_step)) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_all', my_rank
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call istep_viz_w_fix_dt(MHD_step1%time_d%i_time_step,         &
     &                          MHD_step1%viz_step)
          call s_FEM_to_VIZ_bridge                                      &
     &       (FEM_d1%field, FEM_d1%v_sol, VIZ_DAT1)
          call visualize_all(MHD_step1%viz_step, MHD_step1%time_d,      &
     &        VIZ_DAT1%viz_fem, VIZ_DAT1%edge_comm, VIZ_DAT1%viz_fld,   &
     &        VIZ_DAT1%ele_4_nod, VIZ_DAT1%jacobians, vizs1)
!*
!*  ----------- Zonal means --------------
!*
          if(MHD_step1%viz_step%istep_psf .ge. 0) then
            call SGS_MHD_zmean_sections(MHD_step1%viz_step%istep_psf,   &
     &          MHD_step1%time_d, SPH_MHD1%sph, FEM_d1%geofem,          &
     &          SPH_WK1%trns_WK, SPH_SGS1, FEM_d1%field,                &
     &          zmeans1, FEM_d1%v_sol)
          end if
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
        end if
!
        if(lead_field_data_flag(MHD_step1%time_d%i_time_step,           &
     &                          MHD_step1)) then
          call dealloc_sph_trans_area_snap(SPH_WK1%trns_WK)
          call dealloc_SGS_sph_trns_area_snap(SPH_SGS1%trns_WK_LES)
        end if
!*  -----------  exit loop --------------
!*
        if(iflag_finish .gt. 0) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize_sph_SGS_MHD'
      call FEM_finalize_sph_SGS_MHD(MHD_files1, MHD_step1, MHD_IO1)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_MHD'
!      call SPH_finalize_MHD
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      if (iflag_debug.eq.1) write(*,*) 'write_resolution_data'
      call write_resolution_data(SPH_MHD1%sph)
      if (iflag_debug.eq.1) write(*,*) 'output_elapsed_times '
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_mhd
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_MHD
