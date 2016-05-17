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
!!@endverbatim
!
      module analyzer_sph_snap
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_control_parameter
      use m_t_int_parameter
      use m_t_step_parameter
      use m_mesh_data
      use m_node_phys_data
      use m_element_id_4_node
      use m_jacobians
!
      use SPH_analyzer_snap
      use visualizer_all
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_snap
!
      use m_ctl_data_sph_MHD
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_group_data_sph_specr
      use m_mesh_data
      use m_sph_spectr_data
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
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_snap'
      call read_control_4_sph_snap
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_mesh'
      call input_control_SPH_mesh                                       &
     &   (sph_param1, sph1%sph_rtp, sph_rtm1, sph_rlm1, sph1%sph_rj,            &
     &    comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1, bc_rtp_grp1,       &
     &    radial_rtp_grp1, theta_rtp_grp1, zonal_rtp_grp,               &
     &    radial_rj_grp1, sphere_rj_grp1, rj_fld1,                      &
     &    mesh1, group1, ele_mesh1)
      call end_eleps_time(4)
!
!     --------------------- 
!
      call start_eleps_time(2)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_w_viz'
      call FEM_initialize_w_viz                                         &
     &   (mesh1, group1, ele_mesh1, next_tbl1, jac1_3d_q, jac1_3d_l)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_snap'
      call SPH_init_sph_snap
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
      use FEM_analyzer_sph_MHD
!
      integer(kind = kint) :: visval
      integer(kind = kint) :: istep_psf, istep_iso
      integer(kind = kint) :: istep_pvr, istep_fline
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
        istep_max_dt = i_step_MHD
!
        if( mod(i_step_MHD,i_step_output_rst) .ne. 0) cycle
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
        if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
        call SPH_to_FEM_bridge_MHD                                      &
     &     (sph_param1, sph1%sph_rtp, mesh1, iphys, nod_fld1)
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(i_step_MHD, istep_psf, istep_iso,      &
     &      istep_pvr, istep_fline, visval)
!
        call end_eleps_time(4)
!
!*  ----------- Visualization --------------
!*
        if(visval .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_all'
          call start_eleps_time(12)
          call visualize_all                                            &
     &       (istep_psf, istep_iso, istep_pvr, istep_fline,             &
     &        mesh1, group1, ele_mesh1, nod_fld1,                       &
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
      end module analyzer_sph_snap
