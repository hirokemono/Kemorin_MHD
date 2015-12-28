!analyzer_fline.f90
!      module analyzer_fline
!
      module analyzer_fline
!
!     Written by H. Matsui on July, 2006
!
      use m_precision
!
      use m_visualization
!
      use m_node_phys_data
      use m_geometry_data
      use m_group_data
      use FEM_analyzer_viz_fline
      use fieldline
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine initialize_fline
!
      use calypso_mpi
      use m_control_data_vizs
      use set_control_visualizer
!
      integer(kind = kint) :: ierr
!
!     read controls
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_params_4_viz'
      call read_control_data_vizs
      call set_control_params_4_viz(my_rank, ierr, ucd_VIZ)
!
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_fline                                         &
     &   (ele_4_nod_VIZ, jac_VIZ_l, jac_VIZ_q, ucd_VIZ)
!
!  VIZ Initialization
      call FLINE_initialize(node1, ele1, ele_grp1, sf_grp1, nod_fld1)
!
      end subroutine initialize_fline
!
!  ---------------------------------------------------------------------
!
      subroutine analyze
!
      use m_t_step_parameter
!
      integer(kind=kint ) :: i_step, istep_fline
!
!
      do i_step = i_step_init, i_step_number
!
!  Load field data
        call FEM_analyze_fline(i_step, istep_fline, ucd_VIZ)
!
!  Generate field lines
        call FLINE_visualize(istep_fline, node1, ele1, surf1, ele_grp1, &
     &                       ele_4_nod_VIZ, nod_fld1, nod_comm)
      end do
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_fline
