!>@file   analyzer_noviz_sph_snap.f90
!!@brief  module analyzer_noviz_sph_snap
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!        without visualization routines
!!
!!@verbatim
!!      subroutine initialize_noviz_sph_snap
!!      subroutine evolution_noviz_sph_snap
!!@endverbatim
!
      module analyzer_noviz_sph_snap
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use m_ctl_data_sph_SGS_MHD
      use m_MHD_step_parameter
      use m_SPH_MHD_model_data
      use m_SPH_SGS_structure
      use t_SPH_mesh_field_data
      use t_step_parameter
      use t_sph_trans_arrays_MHD
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_snap
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_noviz_sph_snap
!
      use t_ctl_data_SGS_MHD
      use m_ctl_data_sph_MHD
      use init_sph_MHD_elapsed_label
      use input_control_sph_SGS_MHD
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
      call read_control_4_sph_SGS_MHD(snap_ctl_name, MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_SGS_dynamo'
      call input_control_SPH_SGS_dynamo                                 &
     &   (MHD_files1, MHD_ctl1, MHD_step1, SPH_model1,                  &
     &    SPH_WK1%trns_WK, SPH_WK1%monitor, SPH_SGS1, SPH_MHD1, FEM_d1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!     --------------------- 
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD(MHD_files1, MHD_step1, FEM_d1%geofem, &
     &    FEM_d1%field, FEM_d1%iphys, MHD_IO1)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_snap'
      call SPH_init_sph_snap(MHD_files1, FEM_d1%iphys, SPH_model1,      &
     &    SPH_SGS1, SPH_MHD1, SPH_WK1)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_noviz_sph_snap
!
! ----------------------------------------------------------------------
!
      subroutine evolution_noviz_sph_snap
!
      use FEM_analyzer_sph_SGS_MHD
      use output_viz_file_control
      use set_time_step_params
!
      integer(kind = kint) :: iflag
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
!
        iflag = output_IO_flag(MHD_step1%time_d%i_time_step,            &
     &                         MHD_step1%rst_step)
        if(iflag .ne. 0) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if(lead_field_data_flag(MHD_step1%time_d%i_time_step,MHD_step1) &
     &      .eq. 0) then
          call alloc_sph_trans_area_snap                                &
     &       (SPH_MHD1%sph%sph_rtp, SPH_WK1%trns_WK)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_snap'
        call SPH_analyze_snap(MHD_step1%time_d%i_time_step, MHD_files1, &
     &      SPH_model1, MHD_step1, SPH_SGS1, SPH_MHD1, SPH_WK1)
!*
!*  -----------  output field data --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
        if(lead_field_data_flag(MHD_step1%time_d%i_time_step,MHD_step1) &
     &      .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_SGS_MHD'
          call SPH_to_FEM_bridge_SGS_MHD                                &
     &       (SPH_SGS1%SGS_par, SPH_MHD1%sph, SPH_WK1%trns_WK,          &
     &        SPH_SGS1%trns_WK_LES, FEM_d1%geofem, FEM_d1%field)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(MHD_files1,                            &
     &      FEM_d1%geofem, FEM_d1%field, MHD_step1, MHD_IO1)
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
        if(lead_field_data_flag(MHD_step1%time_d%i_time_step,MHD_step1) &
     &      .eq. 0) then
          call dealloc_sph_trans_area_snap(SPH_WK1%trns_WK)
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
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_files1, MHD_step1, MHD_IO1)
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
      end subroutine evolution_noviz_sph_snap
!
! ----------------------------------------------------------------------
!
      end module analyzer_noviz_sph_snap
