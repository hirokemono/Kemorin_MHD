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
!!      subroutine s_int_surface_param_type(mesh, surf, group)
!!      subroutine empty_surface_param_type(mesh, surf, group)
!!        type(mesh_geometry),      intent(in) :: mesh
!!        type(surface_data),       intent(in) :: surf
!!        type(mesh_groups), intent(inout) :: group
!!@endverbatim
!
      module int_surface_param_type
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_int_surface_param_type(mesh, surf, group)
!
      use t_mesh_data
!
      use m_machine_parameter
!
      use int_normal_4_surf_type
      use position_2_each_surf_type
      use set_connects_4_surf_group
      use set_surf_grp_vectors_type
!
      type(mesh_geometry),      intent(in) :: mesh
      type(surface_data),       intent(in) :: surf
!
      type(mesh_groups), intent(inout) :: group
!
!
      call alloc_type_int_surf_data(surf%nnod_4_surf,                   &
     &     group%surf_grp, group%surf_grp_int)
      call alloc_vectors_surf_grp_type(group%surf_grp%num_grp,          &
     &    group%surf_grp%num_item, group%surf_grp_geom)
!
      if (iflag_debug.eq.1) write(*,*) 'pick_normal_of_surf_grp_type'
      call pick_normal_of_surf_grp_type(surf, group%surf_grp,           &
     &    group%tbls_surf_grp, group%surf_grp_geom)
!
      if (iflag_debug.eq.1) write(*,*) 'sum_normal_4_surf_grp_type'
      call sum_normal_4_surf_grp_type(mesh%ele, group%surf_grp,         &
     &    group%surf_grp_geom)
!
!
      if ( group%surf_grp%num_grp .ne. 0 ) then
       if (iflag_debug.eq.1) write(*,*) 'cal_surf_normal_at_nod'
       call cal_surf_normal_at_nod(mesh, surf, group%surf_grp,          &
     &     group%surf_grp_geom, group%surf_nod_grp)
!
!
       if (iflag_debug.eq.1) write(*,*) 's_position_2_each_surf_type'
       call s_position_2_each_surf_type(mesh, surf, group%surf_grp,     &
     &     group%surf_grp_int)
!
       if (iflag_debug.eq.1) write(*,*) 'delta_x_2_each_surf_type'
       call delta_x_2_each_surf_type(mesh, surf, group%surf_grp,        &
     &     group%surf_grp_int)
      end if
!
      end subroutine s_int_surface_param_type
!
!-----------------------------------------------------------------------
!
      subroutine empty_surface_param_type(mesh, surf, group)
!
      use t_mesh_data
!
      use m_machine_parameter
!
      use int_normal_4_surf_type
      use position_2_each_surf_type
      use set_surf_grp_vectors_type
!
      type(mesh_geometry),      intent(in) :: mesh
      type(surface_data),       intent(in) :: surf
!
      type(mesh_groups), intent(inout) :: group
!
!
      call alloc_type_int_surf_data(surf%nnod_4_surf,                   &
     &     group%surf_grp, group%surf_grp_int)
      call alloc_vectors_surf_grp_type(group%surf_grp%num_grp,          &
     &    group%surf_grp%num_item, group%surf_grp_geom)
!
      if (iflag_debug.eq.1) write(*,*) 'sum_normal_4_surf_grp_type'
      call sum_normal_4_surf_grp_type(mesh%ele, group%surf_grp,         &
     &     group%surf_grp_geom)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_vect_surf_grp_nod'
      call alloc_vect_surf_grp_nod(group%surf_nod_grp)
!
      end subroutine empty_surface_param_type
!
!-----------------------------------------------------------------------
!
      end module int_surface_param_type
