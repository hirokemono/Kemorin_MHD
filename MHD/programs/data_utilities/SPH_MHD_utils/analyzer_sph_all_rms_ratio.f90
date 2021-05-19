!>@file   analyzer_sph_all_rms_ratio.f90
!!@brief  module analyzer_sph_all_rms_ratio
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!
!!@verbatim
!!      subroutine initialize_sph_all_rms_ratio
!!      subroutine evolution_sph_all_rms_ratio
!!@endverbatim
!
      module analyzer_sph_all_rms_ratio
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
      use t_step_parameter
      use t_visualizer
      use t_SPH_mesh_field_data
      use t_VIZ_mesh_field
!
      use SPH_analyzer_back_trans
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &                      :: ratio_ctl_name = 'control_sph_rms_ratio'
!>      Control struture for MHD simulation
      type(sph_sgs_mhd_control), save, private :: MHD_ctl1
!
       private :: set_ctl_4_second_spectr_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_all_rms_ratio
!
      use init_sph_MHD_elapsed_label
      use FEM_analyzer_sph_SGS_MHD
      use input_control_sph_SGS_MHD
      use SPH_analyzer_rms_ratio_all
      use FEM_to_VIZ_bridge
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_SGS_MHD'
      call read_control_4_sph_SGS_MHD(ratio_ctl_name, MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_SGS_dynamo'
      call input_control_SPH_SGS_dynamo                                 &
     &   (MHD_files1, MHD_ctl1, MHD_step1, SPH_model1,                  &
     &    SPH_WK1, SPH_SGS1, SPH_MHD1, FEM_d1)
      call set_ctl_4_second_spectr_data                                 &
     &   (MHD_ctl1%new_plt, sph_file_param2)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!     --------------------- 
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_SGS_MHD'
      call FEM_initialize_sph_SGS_MHD(MHD_files1, MHD_step1,            &
     &    SPH_SGS1%iphys_LES, MHD_IO1, FEM_d1)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_back_trans'
      call SPH_init_sph_back_trans                                      &
     &   (MHD_files1, SPH_model1, SPH_SGS1, SPH_MHD1, SPH_WK1)
!
!  -------------------------------------------
!  ----   Mesh setting for visualization -----
!  -------------------------------------------
      if(iflag_debug .gt. 0) write(*,*) 'init_FEM_to_VIZ_bridge'
      call init_FEM_to_VIZ_bridge(MHD_step1%viz_step,                   &
     &                            FEM_d1%geofem, VIZ_DAT1)
!
!        Initialize visualization
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize'
      call init_visualize(MHD_step1%viz_step, FEM_d1%geofem,            &
     &    FEM_d1%field, VIZ_DAT1, MHD_ctl1%viz_ctls, vizs1)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_all_rms_ratio
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_all_rms_ratio
!
      use copy_all_fields_4_sph_trans
!
      use FEM_analyzer_sph_SGS_MHD
      use SPH_analyzer_rms_ratio_all
      use set_time_step_params
      use FEM_to_VIZ_bridge
!
!*  -----------  set initial step data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
      call set_from_initial_step(MHD_step1%init_d, MHD_step1%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(MHD_step1%time_d)
        if(output_IO_flag(MHD_step1%time_d%i_time_step,                 &
     &                    MHD_step1%rst_step) .eqv. .FALSE.) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_rms_ratio_all'
        call SPH_analyze_rms_ratio_all(MHD_step1%time_d, MHD_files1,    &
     &      SPH_SGS1, MHD_step1, SPH_MHD1, SPH_WK1)
!*
!*  -----------  output field data --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
!
        if (iflag_debug.gt.0) write(*,*) 'copy_all_field_from_trans'
        call copy_all_field_from_trans                                  &
     &     (SPH_MHD1%sph%sph_params%m_folding, SPH_MHD1%sph%sph_rtp,    &
     &      SPH_WK1%trns_WK%trns_MHD%backward, FEM_d1%geofem%mesh,      &
     &      FEM_d1%field)
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
          if (iflag_debug.eq.1) write(*,*) 'visualize_all'
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call istep_viz_w_fix_dt(MHD_step1%time_d%i_time_step,         &
     &                          MHD_step1%viz_step)
          call visualize_all(MHD_step1%viz_step, MHD_step1%time_d,      &
     &                       FEM_d1%geofem, FEM_d1%field,               &
     &                       VIZ_DAT1, vizs1, FEM_d1%v_sol)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
        end if
!
!*  -----------  exit loop --------------
!*
        if(MHD_step1%time_d%i_time_step                                 &
     &        .ge. MHD_step1%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize_sph_SGS_MHD'
      call FEM_finalize_sph_SGS_MHD(MHD_files1, MHD_step1, MHD_IO1)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
!      call SPH_finalize_snap
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_all_rms_ratio
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_4_second_spectr_data(new_plt, sph_file_param2)
!
      use t_file_IO_parameter
      use t_ctl_data_4_platforms
      use m_file_format_switch
!
      type(platform_data_control), intent(in) :: new_plt
      type(field_IO_params), intent(inout) :: sph_file_param2
!
!
      sph_file_param2%iflag_format                                      &
     &   = choose_para_file_format(new_plt%spectr_field_fmt_ctl)
!
      sph_file_param2%iflag_IO = new_plt%spectr_field_file_prefix%iflag
      if(sph_file_param2%iflag_IO .gt. 0) then
        sph_file_param2%file_prefix                                     &
     &           = new_plt%spectr_field_file_prefix%charavalue
      end if
!
      end subroutine set_ctl_4_second_spectr_data
!
!  --------------------------------------------------------------------
!
      end module analyzer_sph_all_rms_ratio
