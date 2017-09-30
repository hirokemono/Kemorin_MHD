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
      use m_mesh_data
      use m_SPH_MHD_model_data
      use m_MHD_step_parameter
      use m_SPH_SGS_structure
      use t_step_parameter
!
      use FEM_analyzer_sph_MHD
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
      use m_mesh_data
      use init_sph_MHD_elapsed_label
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
     &  (MHD_files1, SPH_model1%bc_IO, MHD_ctl1, SPH_MHD1%sph,          &
     &   SPH_MHD1%comms, SPH_MHD1%groups, SPH_MHD1%fld, FEM_d1%field,   &
     &   SPH_SGS1, MHD_step1, SPH_model1%MHD_prop, SPH_model1%MHD_BC,   &
     &   SPH_WK1%trns_WK, SPH_WK1%monitor, femmesh1, ele_mesh1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      call end_elapsed_time(4)
!
!     --------------------- 
!
      call start_elapsed_time(2)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD(MHD_files1, MHD_step1,                &
     &    femmesh1%mesh, femmesh1%group, ele_mesh1,                     &
     &    FEM_d1%iphys, FEM_d1%field, range1, fem_ucd1)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_snap'
      call SPH_init_sph_snap(MHD_files1, FEM_d1%iphys, SPH_model1,      &
     &    SPH_SGS1, SPH_MHD1, SPH_WK1)
!
      call calypso_MPI_barrier
!
      call end_elapsed_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_forward_trans
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_forward_trans
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
      call end_elapsed_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_files1, MHD_step1, range1, fem_ucd1)
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
      end subroutine evolution_sph_forward_trans
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_forward_trans
