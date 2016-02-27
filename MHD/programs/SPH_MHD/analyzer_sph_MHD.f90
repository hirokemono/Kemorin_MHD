!>@file   analyzer_sph_MHD.f90
!!@brief  module analyzer_sph_MHD
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine initialize_sph_mhd
!!      subroutine evolution_sph_mhd
!!@endverbatim
!
      module analyzer_sph_MHD
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
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_node_phys_data
!
      use SPH_analyzer_MHD
      use visualizer_all
      use init_sph_MHD_elapsed_label
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_mhd
!
      use m_ctl_data_sph_MHD
      use input_control_sph_MHD
      use FEM_analyzer_sph_MHD_w_viz
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
      call read_control_4_sph_MHD
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_mesh'
      call input_control_SPH_mesh
      call end_eleps_time(4)
!
!    IO elapsed end
!    precondition elaps start
!
      call start_eleps_time(2)
!
!        Initialize FEM mesh data for field data IO
!
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_w_viz'
      call FEM_initialize_w_viz
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_MHD'
      call SPH_initialize_MHD
!
!        Initialize visualization
!
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize'
      call init_visualize                                               &
     &   (mesh1%node, ele1, surf1, edge1, mesh1%nod_comm, edge_comm,    &
     &    ele_grp1, sf_grp1, sf_grp_nod1, nod_fld1)
!
      call calypso_MPI_barrier
!
      call end_eleps_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mhd
!
      use m_element_id_4_node
      use m_jacobians
      use FEM_analyzer_sph_MHD
!
      integer(kind = kint) :: visval, iflag_finish
      integer(kind = kint) :: istep_psf, istep_iso
      integer(kind = kint) :: istep_pvr, istep_fline
!
!     ---------------------
!
      call start_eleps_time(3)
!
!*  -----------  set initial step data --------------
!*
      time =       time_init
      i_step_MHD = i_step_init
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        time = time + dt
        i_step_MHD = i_step_MHD + 1
        istep_max_dt = i_step_MHD
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_MHD'
        call SPH_analyze_MHD(i_step_MHD, iflag_finish)
!*
!*  -----------  output field data --------------
!*
        call start_eleps_time(4)
!
        if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
        call SPH_to_FEM_bridge_MHD
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(i_step_MHD, istep_psf, istep_iso,      &
     &      istep_pvr, istep_fline, visval)
!
        call end_eleps_time(4)
!
!*  ----------- Visualization --------------
!*
        if(visval .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_all', my_rank
          call start_eleps_time(12)
          call visualize_all                                            &
     &       (istep_psf, istep_iso, istep_pvr, istep_fline,             &
     &        mesh1%node, ele1, surf1, edge1, mesh1%nod_comm,           &
     &        edge_comm, ele_grp1, nod_fld1, next_tbl1%neib_ele,        &
     &        jac1_3d_q)
          call end_eleps_time(12)
        end if
!
!*  -----------  exit loop --------------
!*
        if(iflag_finish .gt. 0) exit
      end do
!
!  time evolution end
!
      call end_eleps_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_MHD'
!      call SPH_finalize_MHD
!
      call copy_COMM_TIME_to_eleps(num_elapsed)
      call end_eleps_time(1)
!
      if (iflag_debug.eq.1) write(*,*) 'write_resolution_data'
      call write_resolution_data
      if (iflag_debug.eq.1) write(*,*) 'output_elapsed_times '
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_mhd
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_MHD
