!>@file   analyzer_sph_all_correlate.f90
!!@brief  module analyzer_sph_all_correlate
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!
!!@verbatim
!!      subroutine initialize_sph_all_correlate
!!      subroutine evolution_sph_all_correlate
!!@endverbatim
!
      module analyzer_sph_all_correlate
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_SPH_MHD_model_data
      use m_MHD_step_parameter
      use m_SPH_SGS_structure
      use m_jacobians_VIZ
      use t_step_parameter
      use t_visualizer
      use t_SPH_MHD_zonal_mean_viz
!
      use SPH_analyzer_back_trans
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &                      :: corr_ctl_name = 'control_sph_correlate'
!
      private :: set_ctl_4_second_spectr_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_all_correlate
!
      use t_ctl_data_SGS_MHD
      use m_ctl_data_sph_SGS_MHD
!
      use init_sph_MHD_elapsed_label
      use FEM_analyzer_sph_MHD_w_viz
      use input_control_sph_SGS_MHD
      use SPH_analyzer_correle_all
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
      call read_control_4_sph_SGS_MHD(corr_ctl_name, MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_dynamo'
      call input_control_SPH_dynamo                                     &
     &   (MHD_files1, MHD_ctl1, SPH_SGS1, MHD_step1, SPH_model1,        &
     &    SPH_WK1%trns_WK, SPH_WK1%monitor, SPH_MHD1, FEM_d1)
      call set_ctl_4_second_spectr_data                                 &
     &   (MHD_ctl1%new_plt, sph_file_param2)
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
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_back_trans'
      call SPH_init_sph_back_trans                                      &
     &   (MHD_files1, FEM_d1%iphys, SPH_model1, SPH_MHD1, SPH_WK1)
!        Initialize visualization
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize'
      call init_visualize(FEM_d1%geofem, FEM_d1%ele_mesh, FEM_d1%field, &
     &    MHD_ctl1%viz_ctls, vizs1)
      call init_zonal_mean_sections                                     &
     &   (FEM_d1%geofem, FEM_d1%ele_mesh, FEM_d1%field,                 &
     &    MHD_ctl1%zm_ctls, zmeans1)
!
!
      call calypso_MPI_barrier
      call end_elapsed_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_all_correlate
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_all_correlate
!
      use copy_all_fields_4_sph_trans
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_correle_all
      use SGS_MHD_zonal_mean_viz
!
      integer(kind = kint) :: visval, iflag
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
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_correlate_all'
        call SPH_analyze_correlate_all                                  &
     &     (MHD_step1%time_d, mhd_files1, MHD_step1, SPH_MHD1, SPH_WK1)
!*
!*  -----------  output field data --------------
!*
        call start_elapsed_time(1)
        call start_elapsed_time(4)
!
        if (iflag_debug.gt.0) write(*,*) 'copy_all_field_from_trans'
        call copy_all_field_from_trans                                  &
     &     (SPH_MHD1%sph%sph_params%m_folding, SPH_MHD1%sph%sph_rtp,    &
     &      SPH_WK1%trns_WK%trns_MHD, FEM_d1%geofem%mesh, FEM_d1%field)
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
!*
!*  ----------- Zonal means --------------
!*
          call SGS_MHD_zmean_sections                                   &
     &       (MHD_step1%viz_step, MHD_step1%time_d, SPH_SGS1%SGS_par,   &
     &        SPH_MHD1%sph, FEM_d1%geofem, FEM_d1%ele_mesh,             &
     &        SPH_WK1%trns_WK, FEM_d1%field, zmeans1)
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
      end subroutine evolution_sph_all_correlate
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
      call choose_para_file_format                                      &
     &   (new_plt%spectr_field_fmt_ctl, sph_file_param2%iflag_format)
!
      sph_file_param2%iflag_IO = new_plt%spectr_field_file_prefix%iflag
      if(sph_file_param2%iflag_IO .gt. 0) then
        sph_file_param2%file_prefix                                     &
     &         = new_plt%spectr_field_file_prefix%charavalue
      end if
!
      end subroutine set_ctl_4_second_spectr_data
!
!  --------------------------------------------------------------------
!
      end module analyzer_sph_all_correlate
