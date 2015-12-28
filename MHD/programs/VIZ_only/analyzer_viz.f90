!analyzer_viz.f90
!      module analyzer_viz
!
!     Written by H. Matsui on July, 2006
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_viz
!
      use m_precision
      use m_machine_parameter
!
      use m_visualization
      use m_nod_comm_table
      use m_ele_sf_eg_comm_tables
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_node_phys_data
!
      use FEM_analyzer_viz
      use visualizer_all
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use calypso_mpi
      use m_control_data_vizs
      use set_control_visualizer
!
      integer(kind = kint) :: ierr
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_params_4_viz'
      call read_control_data_vizs
      call set_control_params_4_viz(my_rank, ierr, ucd_VIZ)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!
!  FEM Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'FEM_initialize_vizs'
      call FEM_initialize_vizs                                          &
     &   (ele_4_nod_VIZ, jac_VIZ_l, jac_VIZ_q, ucd_VIZ)
!
!  VIZ Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'init_visualize'
      call init_visualize                                               &
     &   (node1, ele1, surf1, edge1, nod_comm, edge_comm,               &
     &    ele_grp1, sf_grp1, sf_grp_nod1, nod_fld1)
!
      end subroutine init_analyzer
!
!  ---------------------------------------------------------------------
!
      subroutine analyze
!
      integer(kind=kint ) :: i_step, visval
      integer(kind=kint ) :: istep_psf, istep_iso
      integer(kind=kint ) :: istep_pvr, istep_fline
!
!
      do i_step = i_step_init, i_step_number
!  Load field data
        if(iflag_debug .gt. 0)  write(*,*) 'FEM_analyze_vizs', i_step
        call FEM_analyze_vizs(i_step,                                   &
     &      istep_psf, istep_iso, istep_pvr, istep_fline, visval,       &
     &      ucd_VIZ)
!
!  Rendering
        if(visval .eq. 0) then
          if(iflag_debug .gt. 0)  write(*,*) 'visualize_all', i_step
          call visualize_all                                            &
     &       (istep_psf, istep_iso, istep_pvr, istep_fline,             &
     &        node1, ele1, surf1, edge1, nod_comm, edge_comm, ele_grp1, &
     &        nod_fld1, ele_4_nod_VIZ, jac_VIZ_q)
        end if
      end do
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_viz
