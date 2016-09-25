!int_surface_params_MHD.f90
!     module int_surface_params_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!
!!      subroutine int_surface_parameters(mesh, surf, group, surf_wk)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(surface_data), intent(in) :: surf
!!        type(mesh_groups), intent(inout) :: group
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!
      module int_surface_params_MHD
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_surface_group_geometry
      use t_group_connects
      use t_int_surface_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_surface_parameters(mesh, surf, group, surf_wk)
!
!
      use position_of_each_surface
      use set_surf_grp_vectors
      use set_connects_4_surf_group
      use sum_normal_4_surf_group
!
      type(mesh_geometry), intent(inout) :: mesh
      type(surface_data), intent(in) :: surf
!
      type(mesh_groups), intent(inout) :: group
      type(work_surface_element_mat), intent(inout) :: surf_wk
!
!
      if (iflag_debug.eq.1)  write(*,*) 'pick_normal_of_surf_group'
      call pick_normal_of_surf_group (surf, group%surf_grp,             &
     &   group%tbls_surf_grp, group%surf_grp_geom)
!
      if (group%surf_grp%num_grp .le. 0) return
!
      if (iflag_debug.eq.1)  write(*,*) 's_sum_normal_4_surf_group'
      call s_sum_normal_4_surf_group                                    &
     &   (mesh%ele, group%surf_grp, group%surf_grp_geom)
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_surf_norm_node'
      call cal_surf_normal_at_nod(mesh%node, mesh%ele, surf,            &
     &   group%surf_grp, group%surf_grp_geom, group%surf_nod_grp)
!
!
      call alloc_int_surf_data                                          &
     &   (group%surf_grp%num_item, surf%nnod_4_surf, surf_wk)
!
      if (iflag_debug.eq.1)  write(*,*) 'position_2_each_surface'
      call position_2_each_surface                                      &
      &  (mesh%node, mesh%ele, surf, group%surf_grp, surf_wk%xe_sf)
!
      if (iflag_debug.eq.1)  write(*,*) 'delta_x_2_each_surface'
      call delta_x_2_each_surface                                       &
     &   (mesh%node, mesh%ele, surf, group%surf_grp, surf_wk%dxe_sf)
!
!      call check_surface_param_smp('int_surface_parameters end',       &
!     &    my_rank, group%surf_grp, group%surf_nod_grp)
!
      end subroutine int_surface_parameters
!
!-----------------------------------------------------------------------
!
      end module int_surface_params_MHD
