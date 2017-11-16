!>@file   m_cross_section.f90
!!@brief  module m_cross_section
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!      subroutine SECTIONING_initialize                                &
!!     &         (mesh, group, ele_mesh, nod_fld)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(phys_data), intent(in) :: nod_fld
!!      subroutine SECTIONING_visualize                                 &
!!     &         (istep_psf, time_d, ele_mesh, nod_fld)
!!        type(time_data), intent(in) :: time_d
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(phys_data), intent(in) :: nod_fld
!!      subroutine dealloc_psf_field_type
!!@endverbatim
!
!
      module m_cross_section
!
      use calypso_mpi
      use m_precision
!
      use t_cross_section
      use t_isosurface
!
      implicit  none
!
      type(sectioning_module), save :: psf1
      type(isosurface_module), save :: iso1
!
      end module m_cross_section
