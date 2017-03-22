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
      use m_machine_parameter
      use m_work_time
      use m_MHD_step_parameter
      use m_t_step_parameter
      use m_mesh_data
      use m_node_phys_data
      use m_element_id_4_node
      use m_jacobians
      use m_sph_trans_arrays_MHD
!
      use SPH_analyzer_snap
      use visualizer_all
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
      use t_ctl_data_sph_MHD
      use m_ctl_data_sph_MHD
      use m_SGS_control_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
!
      use init_sph_MHD_elapsed_label
      use FEM_analyzer_sph_MHD_w_viz
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
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_MHD'
      call read_control_4_sph_MHD(snap_ctl_name, MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_mesh'
      call input_control_SPH_mesh                                       &
     &   (MHD_ctl1, sph1, comms_sph1, sph_grps1, rj_fld1, nod_fld1,     &
     &    pwr1, SGS_par1, trns_WK1%dynamic_SPH,                         &
     &    mesh1, group1, ele_mesh1)
      call copy_delta_t(MHD_step1%init_d, time_d1)
      call end_eleps_time(4)
!
!     --------------------- 
!
      call start_eleps_time(2)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_w_viz'
      call FEM_initialize_w_viz                                         &
     &   (MHD_step1, mesh1, group1, ele_mesh1,                          &
     &    iphys, nod_fld1, next_tbl1, jac1_3d_q, jac1_3d_l)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_snap'
      call SPH_init_sph_snap(iphys)
!        Initialize visualization
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize'
      call init_visualize(mesh1, group1, ele_mesh1, nod_fld1)
!
      call calypso_MPI_barrier
      call end_eleps_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_snap
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_snap
!
      use m_spheric_parameter
      use m_node_phys_data
!
      use FEM_analyzer_sph_MHD
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
      time_d1%i_time_step = MHD_step1%init_d%i_time_step - 1
!*
!*  -------  time evelution loop start -----------
!*
      do
        time_d1%i_time_step = time_d1%i_time_step + 1
!
        iflag = output_IO_flag(time_d1%i_time_step, MHD_step1%rst_step)
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
!
!*  ----------- Visualization --------------
!*
        if(visval .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_all'
          call start_eleps_time(12)
          call visualize_all(MHD_step1%viz_step, time_d1,               &
     &        mesh1, group1, ele_mesh1, nod_fld1,                       &
     &        next_tbl1%neib_ele, jac1_3d_q)
          call end_eleps_time(12)
        end if
        call end_eleps_time(1)
!
!*  -----------  exit loop --------------
!*
        if(time_d1%i_time_step .ge. MHD_step1%finish_d%i_end_step) exit
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
      end subroutine evolution_sph_snap
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_snap_badboy
!
      use m_spheric_parameter
      use m_node_phys_data
!
      use volume_rendering
      use FEM_analyzer_sph_MHD
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
      call start_eleps_time(3)
!
!*  ----------- Read spectr data and get field data --------------
!*
      time_d1%i_time_step = MHD_step1%init_d%i_time_step
      if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_snap'
      call SPH_analyze_snap(time_d1%i_time_step, MHD_step1)
!*
      iflag = lead_field_data_flag(time_d1%i_time_step, MHD_step1,      &
     &                             SGS_par1%sgs_step)
      if(iflag .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
        call SPH_to_FEM_bridge_MHD                                      &
     &     (sph1%sph_params, sph1%sph_rtp, trns_WK1,                    &
     &      mesh1, iphys, nod_fld1)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
      call FEM_analyze_sph_MHD(SGS_par1, time_d1, mesh1,                &
     &    nod_fld1, MHD_step1, visval)
      call end_eleps_time(4)
!
      if(visval .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'visualize_all'
        call start_eleps_time(12)
        call visualize_all(MHD_step1%viz_step, time_d1,                 &
     &      mesh1, group1, ele_mesh1, nod_fld1,                         &
     &      next_tbl1%neib_ele, jac1_3d_q)
        call deallocate_pvr_data
        call end_eleps_time(12)
      end if
!
!*  ----------- Visualization --------------
!*
      do
        visval = check_PVR_update()
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
          call start_eleps_time(12)
          call PVR_initialize                                           &
     &       (mesh1%node, mesh1%ele, ele_mesh1%surf, group1, nod_fld1)
          call PVR_visualize(MHD_step1%viz_step%PVR_t%istep_file,       &
     &        mesh1%node, mesh1%ele, ele_mesh1%surf, group1,            &
     &        jac1_3d_q, nod_fld1)
          call deallocate_pvr_data
          call end_eleps_time(12)
        end if
        call end_eleps_time(1)
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
      call end_eleps_time(3)
!
  10   continue
!    Loop end
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
      end subroutine evolution_sph_snap_badboy
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_snap
