!volume_rendering_only.f90
!      module volume_rendering_only
!
!      Written by H. Matsui on Apr., 2012
!
!!      subroutine init_visualize_pvr_only                              &
!!     &         (femmesh, ele_mesh, nod_fld)
!!      subroutine visualize_pvr_only                                   &
!!     &         (istep_pvr, femmesh, ele_mesh, jacobians, nod_fld)
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        integer(kind = kint), intent(inout) :: ierror
!!        type(mesh_data), intent(in) :: femmesh
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
     &         (femmesh, ele_mesh, nod_fld)
!
      use volume_rendering
!
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
!
!
      call PVR_initialize                                               &
     &   (femmesh%mesh, femmesh%group, ele_mesh, nod_fld)
      call calypso_MPI_barrier
!
      end subroutine init_visualize_pvr_only
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_pvr_only                                     &
     &         (istep_pvr, femmesh, ele_mesh, jacobians, nod_fld)
!
      use volume_rendering
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacobians
!
!
      call PVR_visualize                                                &
     &   (istep_pvr, femmesh%mesh, femmesh%group, ele_mesh,             &
     &    jacobians%jac_3d, nod_fld)
!
      end subroutine visualize_pvr_only
!
!  ---------------------------------------------------------------------
!
      end module volume_rendering_only
