!visualizer_all.f90
!      module visualizer_all
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine init_visualize                                       &
!!     &         (node, ele, surf, edge, nod_comm, edge_comm,           &
!!     &          ele_grp, sf_grp, sf_grp_nod, nod_fld)
!!      subroutine visualize_all                                        &
!!     &         (istep_psf, istep_iso, istep_pvr, istep_fline,         &
!!     &          node, ele, surf, edge, nod_comm, edge_comm,           &
!!     &          ele_grp, sf_grp, sf_grp_nod, nod_fld,                 &
!!     &          ele_4_nod, jac_3d)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(in) :: edge_comm
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(phys_data), intent(in) :: nod_fld
!!        type(element_around_node), intent(in) :: ele_4_nod
!!        type(jacobians_3d), intent(in) :: jac_3d
!
      module visualizer_all
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_surface_group_connect
      use t_phys_data
      use t_next_node_ele_4_node
      use t_jacobian_3d
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize                                         &
     &         (node, ele, surf, edge, nod_comm, edge_comm,             &
     &          ele_grp, sf_grp, sf_grp_nod, nod_fld)
!
      use m_cross_section
      use m_isosurface
      use set_psf_case_table
      use volume_rendering
      use fieldline
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: edge_comm
!
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
      type(phys_data), intent(in) :: nod_fld
!
!
      if (iflag_debug.eq.1)  write(*,*) 'set_sectioning_case_table'
      call set_sectioning_case_table
!
      call SECTIONING_initialize                                        &
     &   (node, ele, surf, edge, nod_comm, edge_comm,               &
     &    ele_grp, sf_grp, sf_grp_nod, nod_fld)
!
      call ISOSURF_initialize                                           &
     &   (node, ele, surf, edge, ele_grp, nod_fld)
!
      call PVR_initialize(node, ele, surf, ele_grp, nod_fld)
      call calypso_MPI_barrier
!
      call FLINE_initialize(node, ele, ele_grp, sf_grp, nod_fld)
!
      end subroutine init_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_all                                          &
     &         (istep_psf, istep_iso, istep_pvr, istep_fline,           &
     &          node, ele, surf, edge, nod_comm, edge_comm,             &
     &          ele_grp, nod_fld, ele_4_nod, jac_3d)
!
      use m_cross_section
      use m_isosurface
      use volume_rendering
      use fieldline
!
      integer(kind = kint), intent(in) :: istep_psf, istep_iso
      integer(kind = kint), intent(in) :: istep_pvr, istep_fline
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: edge_comm
!
      type(group_data), intent(in) :: ele_grp
!
      type(phys_data), intent(in) :: nod_fld
      type(element_around_node), intent(in) :: ele_4_nod
      type(jacobians_3d), intent(in) :: jac_3d
!
!
      call SECTIONING_visualize(istep_psf, edge, nod_fld)
!
      call ISOSURF_visualize(istep_iso, node, ele, edge,                &
      &                      edge_comm, nod_fld)
!
      call PVR_visualize(istep_pvr, node, ele, surf, jac_3d, nod_fld)
!
      call FLINE_visualize(istep_fline, node, ele, surf, ele_grp,       &
     &                     ele_4_nod, nod_fld, nod_comm)
!
      end subroutine visualize_all
!
!  ---------------------------------------------------------------------
!
      end module visualizer_all
