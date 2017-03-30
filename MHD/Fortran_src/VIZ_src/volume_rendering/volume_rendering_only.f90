!volume_rendering_only.f90
!      module volume_rendering_only
!
!      Written by H. Matsui on Apr., 2012
!
!!      subroutine init_visualize_pvr_only                              &
!!     &         (mesh, group, ele_mesh, nod_fld)
!!      subroutine visualize_pvr_only(istep_pvr,                        &
!!     &          mesh, group, ele_mesh, jacobians, nod_fld)
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        integer(kind = kint), intent(inout) :: ierror
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacobians
!
      module volume_rendering_only
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_surface_data
      use t_phys_data
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
      subroutine init_visualize_pvr_only                                &
     &         (mesh, group, ele_mesh, nod_fld)
!
      use volume_rendering
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
!
!
      call PVR_initialize(mesh%node, mesh%ele, ele_mesh%surf,           &
     &    group, nod_fld)
      call calypso_MPI_barrier
!
      end subroutine init_visualize_pvr_only
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_pvr_only(istep_pvr,                          &
     &          mesh, group, ele_mesh, jacobians, nod_fld)
!
      use volume_rendering
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacobians
!
!
      call PVR_visualize(istep_pvr, mesh%node, mesh%ele,                &
     &    ele_mesh%surf, group, jacobians%jac_3d, nod_fld)
!
      end subroutine visualize_pvr_only
!
!  ---------------------------------------------------------------------
!
      end module volume_rendering_only
