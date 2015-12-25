!volume_rendering_type.f90
!      module volume_rendering_type
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine init_visualize_pvr_type(fem, sf_mesh_psf, fld_nod)
!      subroutine visualize_pvr_type(istep_pvr,                         &
!     &          fem, sf_mesh_psf, jac_3d, fld_nod)
!        integer(kind = kint), intent(in) :: istep_pvr
!        integer(kind = kint), intent(inout) :: ierror
!        type(mesh_data), intent(in) :: fem
!        type(surface_geometry), intent(in) :: sf_mesh_psf
!        type(phys_data), intent(in) :: fld_nod
!        type(jacobians_3d), intent(in) :: jac_3d
!
!      subroutine pvr_init_type(fem, surf, fld_nod)
!        type(mesh_data), intent(in) :: fem
!        type(surface_data), intent(in) :: surf
!        type(phys_data), intent(in) :: fld_nod
!
!      subroutine pvr_main_type(istep_pvr, fem, surf, jac_3d, fld_nod)
!        type(mesh_data), intent(in) :: fem
!        type(surface_data), intent(in) :: surf
!        type(phys_data), intent(in) :: fld_nod
!        type(jacobians_3d), intent(in) :: jac_3d
!        integer(kind = kint), intent(in) :: istep_pvr
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
      subroutine init_visualize_pvr_type(fem, sf_mesh_psf, fld_nod)
!
      use m_control_data_pvrs
      use volume_rendering
!
      type(mesh_data), intent(in) :: fem
      type(surface_geometry), intent(in) :: sf_mesh_psf
      type(phys_data), intent(in) :: fld_nod
!
!
      num_pvr = num_pvr_ctl
      if (num_pvr .gt. 0) then
        call pvr_init_type(fem, sf_mesh_psf%surf, fld_nod)
      end if
      call calypso_MPI_barrier
!
      end subroutine init_visualize_pvr_type
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_pvr_type(istep_pvr,                          &
     &          fem, sf_mesh_psf, jac_3d, fld_nod)
!
      use t_jacobian_3d
      use volume_rendering
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(mesh_data), intent(in) :: fem
      type(surface_geometry), intent(in) :: sf_mesh_psf
      type(phys_data), intent(in) :: fld_nod
      type(jacobians_3d), intent(in) :: jac_3d
!
!
      if (num_pvr.gt.0 .and. istep_pvr.gt.0) then
        call pvr_main_type(istep_pvr, fem, sf_mesh_psf%surf,            &
     &     jac_3d, fld_nod)
      end if
!
      end subroutine visualize_pvr_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine pvr_init_type(fem, surf, fld_nod)
!
      use volume_rendering
!
      type(mesh_data), intent(in) :: fem
      type(surface_data), intent(in) :: surf
      type(phys_data), intent(in) :: fld_nod
!
!
      call pvr_init(fem%mesh%node, fem%mesh%ele,                        &
     &    surf, fem%group%ele_grp, fld_nod)
!
      end subroutine pvr_init_type
!
!  ---------------------------------------------------------------------
!
      subroutine pvr_main_type(istep_pvr, fem, surf, jac_3d, fld_nod)
!
      use t_jacobian_3d
!
      use volume_rendering
!
      integer(kind = kint), intent(in) :: istep_pvr
      type(mesh_data), intent(in) :: fem
      type(surface_data), intent(in) :: surf
      type(phys_data), intent(in) :: fld_nod
      type(jacobians_3d), intent(in) :: jac_3d
!
!
      call pvr_main(istep_pvr, fem%mesh%node, fem%mesh%ele,             &
     &    surf, jac_3d, fld_nod)
!
      end subroutine pvr_main_type
!
!  ---------------------------------------------------------------------
!
      end module volume_rendering_type
