!visualizer_all.f90
!      module visualizer_all
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine init_visualize(femmesh, ele_mesh, nod_fld)
!!      subroutine visualize_all(viz_step, time_d,                     &
!!     &          femmesh, ele_mesh, nod_fld, ele_4_nod, jacs)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: femmesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(phys_data), intent(in) :: nod_fld
!!        type(element_around_node), intent(in) :: ele_4_nod
!!        type(jacobians_type), intent(in) :: jacs
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
      use t_phys_data
      use t_next_node_ele_4_node
      use t_jacobians
      use t_volume_rendering
!
      implicit  none
!
      type(volume_rendering_module), save :: pvr1
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize(femmesh, ele_mesh, nod_fld)
!
      use m_cross_section
      use m_control_data_sections
      use m_control_data_pvrs
      use m_control_data_flines
      use fieldline
!
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
!
      type(phys_data), intent(in) :: nod_fld
!
!
      call start_elapsed_time(60)
      call SECTIONING_initialize                                        &
     &   (femmesh%mesh, femmesh%group, ele_mesh, nod_fld,               &
     &    psf_ctls1, psf1)
      call end_elapsed_time(60)
!
      call start_elapsed_time(61)
      call ISOSURF_initialize                                           &
     &   (femmesh%mesh, femmesh%group, ele_mesh, nod_fld,               &
     &    iso_ctls1, iso1)
      call end_elapsed_time(61)
!
      call start_elapsed_time(62)
      call PVR_initialize(femmesh%mesh, femmesh%group, ele_mesh,        &
     &    nod_fld, pvr_ctls1, pvr1)
      call calypso_MPI_barrier
      call end_elapsed_time(62)
!
      call start_elapsed_time(63)
      call FLINE_initialize                                             &
     &   (femmesh%mesh, femmesh%group, nod_fld, fline_ctls1)
      call end_elapsed_time(63)
!
      end subroutine init_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_all(viz_step, time_d,                       &
     &          femmesh, ele_mesh, nod_fld, ele_4_nod, jacs)
!
      use m_cross_section
      use fieldline
!
      type(time_data), intent(in) :: time_d
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
!
      type(phys_data), intent(in) :: nod_fld
      type(element_around_node), intent(in) :: ele_4_nod
      type(jacobians_type), intent(in) :: jacs
!
!
      call start_elapsed_time(65)
      call SECTIONING_visualize                                         &
     &   (viz_step%PSF_t%istep_file, time_d, ele_mesh, nod_fld, psf1)
      call calypso_MPI_barrier
      call end_elapsed_time(65)
!
      call start_elapsed_time(66)
      call ISOSURF_visualize(viz_step%ISO_t%istep_file, time_d,         &
     &    femmesh%mesh, ele_mesh, nod_fld, iso1)
      call calypso_MPI_barrier
      call end_elapsed_time(66)
!
      call start_elapsed_time(67)
      call PVR_visualize(viz_step%PVR_t%istep_file,                    &
     &    femmesh%mesh, femmesh%group, ele_mesh, jacs, nod_fld, pvr1)
      call calypso_MPI_barrier
      call end_elapsed_time(67)
!
      call start_elapsed_time(68)
      call FLINE_visualize(viz_step%FLINE_t%istep_file,                &
     &   femmesh%mesh, femmesh%group, ele_mesh, ele_4_nod, nod_fld)
      call calypso_MPI_barrier
      call end_elapsed_time(68)
!
      end subroutine visualize_all
!
!  ---------------------------------------------------------------------
!
      end module visualizer_all
