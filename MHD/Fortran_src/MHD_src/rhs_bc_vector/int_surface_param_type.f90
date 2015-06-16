!int_surface_param_type.f90
!     module int_surface_param_type
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Dec., 2009
!
!      subroutine s_int_surface_param_type(mesh, surf, jac_sf_grp,      &
!     &          group)
!        type(mesh_geometry),      intent(in) :: mesh
!        type(surface_data),       intent(in) :: surf
!        type(jacobians_surf_grp), intent(in) :: jac_sf_grp
!        type(mesh_groups), intent(inout) :: group
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
      subroutine s_int_surface_param_type(mesh, surf, jac_sf_grp,       &
     &          group)
!
      use t_mesh_data
      use t_jacobians
!
      use m_machine_parameter
!
      use int_normal_4_surf_type
      use position_2_each_surf_type
      use set_norm_nod_surf_grp_type
      use set_surf_grp_vectors_type
!
      type(mesh_geometry),      intent(in) :: mesh
      type(surface_data),       intent(in) :: surf
      type(jacobians_surf_grp), intent(in) :: jac_sf_grp
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
      if (iflag_debug.eq.1) write(*,*) 'alloc_vect_surf_grp_nod'
      call alloc_vect_surf_grp_nod(group%surf_nod_grp)
!
      if ( group%surf_grp%num_grp .ne. 0 ) then
       if (iflag_debug.eq.1) write(*,*) 'cal_surf_norm_nod_type'
       call allocate_work_norm_nod(mesh%node%numnod)
       call cal_surf_norm_nod_type(mesh, surf, group%surf_grp,          &
     &     group%surf_grp_geom, group%surf_nod_grp)
!
       call deallocate_work_norm_nod
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
      subroutine empty_surface_param_type(mesh, surf, jac_sf_grp,       &
     &          group)
!
      use t_mesh_data
      use t_jacobians
!
      use m_machine_parameter
!
      use int_normal_4_surf_type
      use position_2_each_surf_type
      use set_norm_nod_surf_grp_type
      use set_surf_grp_vectors_type
!
      type(mesh_geometry),      intent(in) :: mesh
      type(surface_data),       intent(in) :: surf
      type(jacobians_surf_grp), intent(in) :: jac_sf_grp
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
