!set_control_surface_mesh.f90
!      module set_control_surface_mesh
!
!        programmed by H.Matsui on Nov., 2009
!
!      subroutine set_control_surf_mesh_def
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
      if (i_elem_header .gt. 0) then
        mesh_file_head = elem_file_prefix
        iflag_ele_file_name = i_elem_header
      end if
!
      if (i_surf_header .gt. 0) then
        mesh_surf_file_head = surf_file_prefix
        iflag_surf_file_name = i_surf_header
      end if
!
      if (i_edge_header .gt. 0) then
        mesh_edge_file_head = edge_file_prefix
        iflag_edge_file_name = i_edge_header
      end if
!
      end subroutine set_control_surf_mesh_def
!
! ----------------------------------------------------------------------
!
      end module set_control_surface_mesh
