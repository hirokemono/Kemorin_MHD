!int_surface_param_type.f90
!     module int_surface_param_type
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Dec., 2009
!
!>@file  int_surface_param_type.f90
!!       module int_surface_param_type
!!
!!@author H. Matsui
!!@date   Programmed on Dec., 2009
!
!> @brief  Surface integeation for boundary conditions
!!
!!@verbatim
!!      subroutine s_int_surface_param_type(mesh, surf, group, surf_wk)
!!      subroutine empty_surface_param_type(mesh, surf, group, surf_wk)
!!        type(mesh_geometry),      intent(in) :: mesh
!!        type(surface_data),       intent(in) :: surf
!!        type(mesh_groups), intent(inout) :: group
!!@endverbatim
!
      module int_surface_param_type
!
      use m_precision
!
      use t_mesh_data
      use t_surface_group_geometry
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_int_surface_param_type(mesh, surf, group, surf_wk)
!
      use int_surface_params_MHD
!
      type(mesh_geometry),      intent(in) :: mesh
      type(surface_data),       intent(in) :: surf
!
      type(mesh_groups), intent(inout) :: group
      type(work_surface_element_mat), intent(inout) :: surf_wk
!
!
      call int_surface_parameters(mesh%node, mesh%ele, surf,            &
     &    group%surf_grp, group%tbls_surf_grp, group%surf_grp_geom,     &
     &    group%surf_nod_grp, surf_wk)
!
      end subroutine s_int_surface_param_type
!
!-----------------------------------------------------------------------
!
      subroutine empty_surface_param_type(mesh, surf, group, surf_wk)
!
      use int_surface_params_MHD
!
      type(mesh_geometry),      intent(in) :: mesh
      type(surface_data),       intent(in) :: surf
      type(work_surface_element_mat), intent(inout) :: surf_wk
!
      type(mesh_groups), intent(inout) :: group
!
!
      call empty_surface_parameters(mesh%ele, surf,                     &
     &    group%surf_grp, group%surf_grp_geom, group%surf_nod_grp,      &
     &    surf_wk)
!
      end subroutine empty_surface_param_type
!
!-----------------------------------------------------------------------
!
      end module int_surface_param_type
