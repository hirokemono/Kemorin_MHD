!>@file   set_control_surface_mesh.f90
!!@brief  module set_control_surface_mesh
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2009
!
!>@brief  Set parameters for surface and edge mesh
!!
!!@verbatim
!!      subroutine set_control_surf_mesh_def
!!@endverbatim
!
      module set_control_surface_mesh
!
      use m_precision
!
      use m_constants
      use m_ctl_data_4_platforms
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_surf_mesh_def
!
      use surface_file_IO
      use edge_file_IO
!
!
      iflag_ele_file_name = elem_file_prefix%iflag
      if (iflag_ele_file_name .gt. 0) then
        mesh_file_head = elem_file_prefix%charavalue
      end if
!
      iflag_surf_file_name = surf_file_prefix%iflag
      if (iflag_surf_file_name .gt. 0) then
        mesh_surf_file_head = surf_file_prefix%charavalue
      end if
!
      iflag_edge_file_name = edge_file_prefix%iflag
      if (iflag_edge_file_name .gt. 0) then
        mesh_edge_file_head = edge_file_prefix%charavalue
      end if
!
      end subroutine set_control_surf_mesh_def
!
! ----------------------------------------------------------------------
!
      end module set_control_surface_mesh
