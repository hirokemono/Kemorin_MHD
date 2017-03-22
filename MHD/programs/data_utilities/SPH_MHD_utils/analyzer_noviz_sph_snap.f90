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
      use calypso_mpi
      use m_work_time
      use m_t_step_parameter
      use m_MHD_step_parameter
      use m_mesh_data
      use m_sph_trans_arrays_MHD
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
      subroutine initialize_noviz_sph_snap
!
      use t_ctl_data_sph_MHD_psf
      use m_ctl_data_sph_MHD
      use m_spheric_parameter
      use m_SGS_control_parameter
      use m_mesh_data
      use m_node_phys_data
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_cal_max_indices
      use init_sph_MHD_elapsed_label
      use input_control_sph_MHD
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      total_start = MPI_WTIME()
      call set_sph_MHD_elapsed_label
!
!   Load parameter file
!
      call start_eleps_time(1)
      call start_eleps_time(4)
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_MHD_noviz'
      call read_control_4_sph_MHD_noviz(snap_ctl_name, MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_mesh'
      call input_control_SPH_mesh                                       &
     &   (MHD_ctl1, sph1, comms_sph1, sph_grps1, rj_fld1, nod_fld1,     &
     &    pwr1, SGS_par1, trns_WK1%dynamic_SPH,                         &
     &    mesh1, group1, ele_mesh1)
      call copy_delta_t(init_d1, time_d1)
      call end_eleps_time(4)
!
!     --------------------- 
!
      call start_eleps_time(2)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD(MHD_step1, mesh1, group1, ele_mesh1,  &
     &    iphys, nod_fld1, range)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_snap'
      call SPH_init_sph_snap(iphys)
!
      call calypso_MPI_barrier
!
      call end_eleps_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_noviz_sph_snap
!
! ----------------------------------------------------------------------
!
      subroutine evolution_noviz_sph_snap
!
      use m_spheric_parameter
      use m_node_phys_data
      use output_viz_file_control
!
      integer(kind = kint) :: visval
      integer(kind = kint) :: iflag
!
!     ---------------------
!
      call start_eleps_time(3)
!
!*  -----------  set initial step data --------------
!*
      time_d1%i_time_step = init_d1%i_time_step - 1
!*
!*  -------  time evelution loop start -----------
!*
      do
        time_d1%i_time_step = time_d1%i_time_step + 1
!
        iflag = output_IO_flag(time_d1%i_time_step,MHD_step1%rst_step)
        if(iflag .ne. 0) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_snap'
        call SPH_analyze_snap(time_d1%i_time_step, MHD_step1)
!*
!*  -----------  output field data --------------
!*
        call start_eleps_time(1)
        call start_eleps_time(4)
!
        iflag = lead_field_data_flag(time_d1%i_time_step, MHD_step1,    &
     &                               SGS_par1%sgs_step)
        if(iflag .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
          call SPH_to_FEM_bridge_MHD                                    &
     &       (sph1%sph_params, sph1%sph_rtp, trns_WK1,                  &
     &        mesh1, iphys, nod_fld1)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(SGS_par1, time_d1, mesh1,              &
     &      nod_fld1, MHD_step1, visval)
!
        call end_eleps_time(4)
        call end_eleps_time(1)
!
!*  -----------  exit loop --------------
!*
        if(time_d1%i_time_step .ge. finish_d1%i_end_step) exit
      end do
!
!  time evolution end
!
      call end_eleps_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_step1)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
!      call SPH_finalize_snap
!
      call copy_COMM_TIME_to_eleps(num_elapsed)
      call end_eleps_time(1)
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
