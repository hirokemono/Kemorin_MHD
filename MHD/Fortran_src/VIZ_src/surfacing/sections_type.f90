!
!      module sections_type
!
!      Written by H. Matsui on Apr., 2012
!
!!      subroutine init_sections_type(fem, ele_mesh_psf, nod_fld)
!!      subroutine sectioning_type(istep_psf, istep_iso,                &
!!     &          fem, ele_mesh_psf, nod_fld)
!!        type(mesh_data), intent(in) :: fem
!!        type(element_geometry), intent(in) :: ele_mesh_psf
!!        type(phys_data), intent(in) :: nod_fld
!
      module sections_type
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
      subroutine init_sections_type(fem, ele_mesh_psf, nod_fld)
!
      use sections_for_1st
!
      type(mesh_data), intent(inout) :: fem
      type(element_geometry), intent(inout) :: ele_mesh_psf
      type(phys_data), intent(inout) :: nod_fld
!
!
      call init_visualize_surface(fem%mesh, fem%group,                  &
     &    ele_mesh_psf%surf, ele_mesh_psf%edge, ele_mesh_psf%edge_comm, &
     &    nod_fld)
!
      end subroutine init_sections_type
!
!  ---------------------------------------------------------------------
!
      subroutine sectioning_type(istep_psf, istep_iso,                  &
     &          fem, ele_mesh_psf, nod_fld)
!
      use sections_for_1st
!
      type(mesh_data), intent(in) :: fem
      type(element_geometry), intent(in) :: ele_mesh_psf
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: istep_psf, istep_iso
!
!
      call visualize_surface(istep_psf, istep_iso, fem%mesh,            &
     &    ele_mesh_psf%edge, ele_mesh_psf%edge_comm, nod_fld)
!
      end subroutine sectioning_type
!
!  ---------------------------------------------------------------------
!
      end module sections_type
