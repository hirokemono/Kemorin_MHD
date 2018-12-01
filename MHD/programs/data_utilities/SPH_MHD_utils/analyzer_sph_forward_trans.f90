!>@file   analyzer_sph_forward_trans.f90
!!@brief  module analyzer_sph_forward_trans
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!        without visualization routines
!!
!!@verbatim
!!      subroutine initialize_sph_forward_trans
!!      subroutine evolution_sph_forward_trans
!!@endverbatim
!
      module analyzer_sph_forward_trans
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_SPH_MHD_model_data
      use m_MHD_step_parameter
      use m_SPH_SGS_structure
      use m_jacobians_VIZ
      use t_step_parameter
!
      use FEM_analyzer_sph_MHD_w_viz
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
      subroutine initialize_sph_forward_trans
!
      use t_ctl_data_SGS_MHD
      use m_ctl_data_sph_SGS_MHD
      use init_sph_MHD_elapsed_label
      use input_control_sph_SGS_MHD
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      total_start = MPI_WTIME()
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call append_COMM_TIME_to_elapsed
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_SGS_MHD'
      call read_control_4_sph_SGS_MHD(snap_ctl_name, MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_dynamo'
      call input_control_SPH_dynamo                                     &
     &  (MHD_files1, MHD_ctl1, SPH_SGS1, MHD_step1, SPH_model1,         &
     &   SPH_WK1%trns_WK, SPH_WK1%monitor, SPH_MHD1, FEM_d1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!     --------------------- 
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_w_viz'
      call FEM_initialize_w_viz                                         &
     &   (MHD_files1, MHD_step1, FEM_d1%geofem, FEM_d1%ele_mesh,        &
     &    FEM_d1%iphys, FEM_d1%field, next_tbl_VIZ1, jacobians_VIZ1,    &
     &    MHD_IO1)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_snap'
      call SPH_init_sph_snap(MHD_files1, FEM_d1%iphys, SPH_model1,      &
     &    SPH_SGS1, SPH_MHD1, SPH_WK1)
!
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_forward_trans
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_forward_trans
!
      use FEM_analyzer_sph_MHD
!
      integer(kind = kint) :: visval, iflag
!
!*  -----------  set initial step data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
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
      call copy_COMM_TIME_to_elaps
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_forward_trans
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_forward_trans
