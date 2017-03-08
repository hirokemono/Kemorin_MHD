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
      call end_eleps_time(4)
!
!     --------------------- 
!
      call start_eleps_time(2)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_w_viz'
      call FEM_initialize_w_viz(mesh1, group1, ele_mesh1,               &
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
      i_step_MHD = i_step_init - 1
!*
!*  -------  time evelution loop start -----------
!*
      do
        i_step_MHD = i_step_MHD + 1
!
        if(output_IO_flag(i_step_MHD,rst_step1) .ne. 0) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_snap'
        call SPH_analyze_snap(i_step_MHD)
!*
!*  -----------  output field data --------------
!*
        call start_eleps_time(1)
        call start_eleps_time(4)
!
        iflag = lead_field_data_flag(i_step_MHD,                        &
     &                               viz_step1, SGS_par1%sgs_step)
        if(iflag .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
          call SPH_to_FEM_bridge_MHD                                    &
     &       (sph1%sph_params, sph1%sph_rtp, trns_WK1,                  &
     &        mesh1, iphys, nod_fld1)
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
          if (iflag_debug.eq.1) write(*,*) 'visualize_all'
          call start_eleps_time(12)
          call visualize_all                                            &
     &       (viz_step1, mesh1, group1, ele_mesh1, nod_fld1,            &
     &        next_tbl1%neib_ele, jac1_3d_q)
          call end_eleps_time(12)
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
      rms_step1%increment = 0
      ucd_step1%increment = 0
      if(elapsed_time .gt. 1800.0) then
        if (my_rank.eq.0) write(*,*) 'This code can use up to 30 min.'
        elapsed_time = 1800.0
      end if
      if(elapsed_time .lt. 1800.0) elapsed_time = 1800.0
      call start_eleps_time(3)
!
!*  ----------- Read spectr data and get field data --------------
!*
      i_step_MHD = i_step_init
      if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_snap'
      call SPH_analyze_snap(i_step_MHD)
!*
      iflag = lead_field_data_flag(i_step_MHD,                          &
     &                             viz_step1, SGS_par1%sgs_step)
      if(iflag .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
        call SPH_to_FEM_bridge_MHD                                      &
     &     (sph1%sph_params, sph1%sph_rtp, trns_WK1,                    &
     &      mesh1, iphys, nod_fld1)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
      call FEM_analyze_sph_MHD                                          &
     &   (i_step_MHD, SGS_par1, mesh1, nod_fld1, viz_step1, visval)
      call end_eleps_time(4)
!
      if(visval .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'visualize_all'
        call start_eleps_time(12)
        call visualize_all                                              &
     &       (viz_step1, mesh1, group1, ele_mesh1, nod_fld1,            &
     &        next_tbl1%neib_ele, jac1_3d_q)
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
          call PVR_visualize                                            &
     &       (viz_step1%PVR_t%istep_file, mesh1%node, mesh1%ele,        &
     &        ele_mesh1%surf, group1, jac1_3d_q, nod_fld1)
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
        if(total_time .gt. elapsed_time) then
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
      end subroutine evolution_sph_snap_badboy
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_snap
