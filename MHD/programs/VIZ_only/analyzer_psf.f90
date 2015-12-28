!
!      module analyzer_psf
!
!     Written by H. Matsui on July, 2006
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_psf
!
      use m_precision
!
      use m_visualization
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
!
      use FEM_analyzer_viz_surf
      use sections_for_1st
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
      use m_control_data_section_only
      use set_control_visualizer
!
      integer(kind = kint) :: ierr
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_params_4_viz'
      call read_control_data_section_only
      call set_control_params_4_viz(my_rank, ierr, ucd_VIZ)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_surface(ucd_VIZ)
!
!  VIZ Initialization
      call init_visualize_surface                                       &
     &   (node1, ele1, surf1, edge1, nod_comm, edge_comm,               &
     &    ele_grp1, sf_grp1, sf_grp_nod1, nod_fld1)
!
      end subroutine init_analyzer
!
!  ---------------------------------------------------------------------
!
      subroutine analyze
!
      use m_t_step_parameter
!
      integer(kind=kint ) :: i_step, istep_psf, istep_iso
!
!
      do i_step = i_step_init, i_step_number
!
!  Load field data
        call FEM_analyze_surface                                        &
     &     (i_step, istep_psf, istep_iso, ucd_VIZ)
!
!  Generate field lines
        call visualize_surface(istep_psf, istep_iso,                    &
     &                         node1, ele1, edge1, edge_comm, nod_fld1)
      end do
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_psf
