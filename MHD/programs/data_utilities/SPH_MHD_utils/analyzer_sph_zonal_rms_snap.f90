!>@file   analyzer_sph_zonal_rms_snap.f90
!!@brief  module analyzer_sph_zonal_rms_snap
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate zonal root mean square field
!!        including visualization module
!!
!!@verbatim
!!      subroutine initialize_sph_zonal_rms_snap
!!      subroutine evolution_sph_zonal_rms_snap
!!@endverbatim
!
      module analyzer_sph_zonal_rms_snap
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_t_step_parameter
      use m_SGS_control_parameter
      use m_mesh_data
      use m_node_phys_data
      use m_sph_trans_arrays_MHD
!
      use FEM_analyzer_sph_MHD
      use sections_for_1st
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
      subroutine initialize_sph_zonal_rms_snap
!
      use t_ctl_data_sph_MHD_psf
      use m_ctl_data_sph_MHD
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_cal_max_indices
      use m_rms_4_sph_spectr
      use init_sph_MHD_elapsed_label
      use SPH_analyzer_snap
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
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_MHD_w_psf'
      call read_control_4_sph_MHD_w_psf(snap_ctl_name, MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_mesh'
      call input_control_SPH_mesh                                       &
     &   (MHD_ctl1, sph1, comms_sph1, sph_grps1, rj_fld1, nod_fld1,     &
     &    pwr1, SGS_par1, trns_WK1%dynamic_SPH,                         &
     &    mesh1, group1, ele_mesh1)
      call end_eleps_time(4)
!
!     --------------------- 
!
      call start_eleps_time(2)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD(mesh1, group1, ele_mesh1,             &
     &    iphys, nod_fld1, range)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_snap'
      call SPH_init_sph_snap(iphys)
!
!        Initialize visualization
!
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize_surface'
      call init_visualize_surface(mesh1, group1, ele_mesh1, nod_fld1)
!
      call calypso_MPI_barrier
      call end_eleps_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_zonal_rms_snap
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_zonal_rms_snap
!
      use SPH_analyzer_zrms_snap
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
      i_step_MHD = i_step_init - 1
!*
!*  -------  time evelution loop start -----------
!*
      do
        i_step_MHD = i_step_MHD + 1
!
        if(output_flag(i_step_MHD,rst_step1%increment) .ne. 0) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_zRMS_snap'
        call SPH_analyze_zRMS_snap(i_step_MHD)
!*
!*  -----------  output field data --------------
!*
        call start_eleps_time(1)
        call start_eleps_time(4)
        iflag = lead_field_data_flag(i_step_MHD,                        &
     &                               viz_step1,SGS_par1%sgs_step)
        if(iflag .eq. 0) then
          if(iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_zRMS_snap'
          call SPH_to_FEM_bridge_zRMS_snap
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD                                        &
     &     (i_step_MHD, SGS_par1, mesh1, nod_fld1, viz_step1, visval)
!
        call end_eleps_time(4)
!
!*  ----------- Visualization --------------
!*
        if(visval .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_surface'
          call start_eleps_time(8)
          call visualize_surface(viz_step1, mesh1, ele_mesh1, nod_fld1)
          call end_eleps_time(8)
        end if
        call end_eleps_time(1)
!
!*  -----------  exit loop --------------
!*
        if(i_step_MHD .ge. i_step_number) exit
      end do
!
!  time evolution end
!
      call end_eleps_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize
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
      end subroutine evolution_sph_zonal_rms_snap
!
!-----------------------------------------------------------------------
!
      end module analyzer_sph_zonal_rms_snap
