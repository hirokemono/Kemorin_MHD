!>@file   m_isosurface.f90
!!@brief  module m_isosurface
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for isosurfacing
!!
!!@verbatim
!!      subroutine ISOSURF_initialize(mesh, group, ele_mesh, nod_fld)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(element_geometry), intent(in) :: ele_mesh
!!
!!        type(phys_data), intent(in) :: nod_fld
!!      subroutine ISOSURF_visualize                                    &
!!     &         (istep_iso, time_d, mesh, ele_mesh, nod_fld)
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(phys_data), intent(in) :: nod_fld
!!
!!      subroutine dealloc_iso_field_type
!!@endverbatim
!
      module m_isosurface
!
      use m_precision
      use t_isosurface
!
      implicit  none
!
      type(isosurface_module), save :: iso1
!
      end module m_isosurface
