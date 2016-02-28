!volume_rendering_type.f90
!      module volume_rendering_type
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine init_visualize_pvr_type(fem, ele_mesh_psf, fld_nod)
!      subroutine visualize_pvr_type(istep_pvr,                         &
!     &          fem, ele_mesh_psf, jac_3d, fld_nod)
!        integer(kind = kint), intent(in) :: istep_pvr
!        integer(kind = kint), intent(inout) :: ierror
!        type(mesh_data), intent(in) :: fem
!        type(element_geometry), intent(in) :: ele_mesh_psf
!        type(phys_data), intent(in) :: fld_nod
!        type(jacobians_3d), intent(in) :: jac_3d
!
      module volume_rendering_type
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_surface_data
      use t_phys_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize_pvr_type(fem, ele_mesh_psf, fld_nod)
!
      use volume_rendering
!
      type(mesh_data), intent(in) :: fem
      type(element_geometry), intent(in) :: ele_mesh_psf
      type(phys_data), intent(in) :: fld_nod
!
!
      call PVR_initialize(fem%mesh%node, fem%mesh%ele,                  &
     &    ele_mesh_psf%surf, fem%group%ele_grp, fld_nod)
      call calypso_MPI_barrier
!
      end subroutine init_visualize_pvr_type
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_pvr_type(istep_pvr,                          &
     &          fem, ele_mesh_psf, jac_3d, fld_nod)
!
      use t_jacobian_3d
      use volume_rendering
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(mesh_data), intent(in) :: fem
      type(element_geometry), intent(in) :: ele_mesh_psf
      type(phys_data), intent(in) :: fld_nod
      type(jacobians_3d), intent(in) :: jac_3d
!
!
      call PVR_visualize(istep_pvr, fem%mesh%node, fem%mesh%ele,        &
     &    ele_mesh_psf%surf, jac_3d, fld_nod)
!
      end subroutine visualize_pvr_type
!
!  ---------------------------------------------------------------------
!
      end module volume_rendering_type
