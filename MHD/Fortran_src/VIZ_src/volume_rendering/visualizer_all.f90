!visualizer_all.f90
!      module visualizer_all
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine init_visualize(mesh, group, ele_mesh, nod_fld)
!!      subroutine visualize_all(viz_step, time_d,                     &
!!     &          mesh, group, ele_mesh, nod_fld, ele_4_nod, jacobians)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(phys_data), intent(in) :: nod_fld
!!        type(element_around_node), intent(in) :: ele_4_nod
!!        type(jacobians_type), intent(in) :: jacobians
!
      module visualizer_all
!
      use m_precision
!
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_VIZ_step_parameter
      use t_time_data
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_surface_group_connect
      use t_phys_data
      use t_next_node_ele_4_node
      use t_jacobians
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize(mesh, group, ele_mesh, nod_fld)
!
      use m_cross_section
      use m_isosurface
      use set_psf_case_table
      use volume_rendering
      use fieldline
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
!
      type(phys_data), intent(in) :: nod_fld
!
!
      call start_eleps_time(60)
      if (iflag_debug.eq.1)  write(*,*) 'set_sectioning_case_table'
      call set_sectioning_case_table
!
      call SECTIONING_initialize                                        &
     &   (mesh%node, mesh%ele, ele_mesh%surf, ele_mesh%edge,            &
     &    mesh%nod_comm, ele_mesh%edge_comm,                            &
     &    group%ele_grp, group%surf_grp, group%surf_nod_grp, nod_fld)
      call end_eleps_time(60)
!
      call start_eleps_time(61)
      call ISOSURF_initialize(mesh%node, mesh%ele,                      &
     &    ele_mesh%surf, ele_mesh%edge, group%ele_grp, nod_fld)
      call end_eleps_time(61)
!
      call start_eleps_time(62)
      call PVR_initialize                                               &
     &   (mesh%node, mesh%ele, ele_mesh%surf, group, nod_fld)
      call calypso_MPI_barrier
      call end_eleps_time(62)
!
      call start_eleps_time(63)
      call FLINE_initialize                                             &
     &   (mesh%node, mesh%ele, group%ele_grp, group%surf_grp, nod_fld)
      call end_eleps_time(63)
!
      end subroutine init_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_all(viz_step, time_d,                       &
     &          mesh, group, ele_mesh, nod_fld, ele_4_nod, jacobians)
!
      use m_cross_section
      use m_isosurface
      use volume_rendering
      use fieldline
!
      type(time_data), intent(in) :: time_d
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
!
      type(phys_data), intent(in) :: nod_fld
      type(element_around_node), intent(in) :: ele_4_nod
      type(jacobians_type), intent(in) :: jacobians
!
!
      call start_eleps_time(65)
      call SECTIONING_visualize                                         &
     &   (viz_step%PSF_t%istep_file, time_d, ele_mesh%edge, nod_fld)
      call calypso_MPI_barrier
      call end_eleps_time(65)
!
      call start_eleps_time(66)
      call ISOSURF_visualize                                            &
     &   (viz_step%ISO_t%istep_file, time_d, mesh%node, mesh%ele,       &
     &    ele_mesh%edge, ele_mesh%edge_comm, nod_fld)
      call calypso_MPI_barrier
      call end_eleps_time(66)
!
      call start_eleps_time(67)
      call PVR_visualize                                                &
     &   (viz_step%PVR_t%istep_file, mesh%node, mesh%ele,               &
     &    ele_mesh%surf, group, jacobians%jac_3d, nod_fld)
      call calypso_MPI_barrier
      call end_eleps_time(67)
!
      call start_eleps_time(68)
      call FLINE_visualize                                              &
     &   (viz_step%FLINE_t%istep_file, mesh%node, mesh%ele,             &
     &    ele_mesh%surf, group%ele_grp, ele_4_nod, nod_fld,             &
     &    mesh%nod_comm)
      call calypso_MPI_barrier
      call end_eleps_time(68)
!
      end subroutine visualize_all
!
!  ---------------------------------------------------------------------
!
      end module visualizer_all
