!int_surface_params_MHD.f90
!     module int_surface_params_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!
!!      subroutine int_surface_parameters (node, ele, surf,             &
!!     &          sf_grp, sf_grp_tbl, sf_grp_v, sf_grp_nod, surf_wk)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_group_table), intent(in) :: sf_grp_tbl
!!        type(surface_group_geometry), intent(inout) :: sf_grp_v
!!        type(surface_node_grp_data), intent(inout) :: sf_grp_nod
!!      type(work_surface_element_mat), intent(inout) :: surf_wk
!
      module int_surface_params_MHD
!
      use m_precision
      use m_machine_parameter
!
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
      subroutine int_surface_parameters(node, ele, surf,                &
     &          sf_grp, sf_grp_tbl, sf_grp_v, sf_grp_nod, surf_wk)
!
!
      use position_of_each_surface
      use set_surf_grp_vectors
      use set_connects_4_surf_group
      use sum_normal_4_surf_group
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_table), intent(in) :: sf_grp_tbl
!
      type(surface_group_geometry), intent(inout) :: sf_grp_v
      type(surface_node_grp_data), intent(inout) :: sf_grp_nod
      type(work_surface_element_mat), intent(inout) :: surf_wk
!
!
      call alloc_int_surf_data                                          &
     &   (sf_grp%num_item, surf%nnod_4_surf, surf_wk)
!
!
      if (iflag_debug.eq.1)  write(*,*) 'pick_normal_of_surf_group'
      call pick_normal_of_surf_group                                    &
     &   (surf, sf_grp, sf_grp_tbl, sf_grp_v)
!
      if (sf_grp%num_grp .le. 0) return
!
      if (iflag_debug.eq.1)  write(*,*) 's_sum_normal_4_surf_group'
      call s_sum_normal_4_surf_group(ele, sf_grp, sf_grp_v)
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_surf_norm_node'
      call cal_surf_normal_at_nod                                       &
     &   (node, ele, surf, sf_grp, sf_grp_v, sf_grp_nod)
!
!
      if (iflag_debug.eq.1)  write(*,*) 'position_2_each_surface'
      call position_2_each_surface                                      &
      &  (node, ele, surf, sf_grp, surf_wk%xe_sf)
!
      if (iflag_debug.eq.1)  write(*,*) 'delta_x_2_each_surface'
      call delta_x_2_each_surface                                       &
     &   (node, ele, surf, sf_grp, surf_wk%dxe_sf)
!
!      call check_surface_param_smp('int_surface_parameters end',       &
!     &    my_rank, sf_grp, sf_grp_nod)
!
      end subroutine int_surface_parameters
!
!-----------------------------------------------------------------------
!
      subroutine empty_surface_parameters                               &
     &         (ele, surf, sf_grp, sf_grp_v, sf_grp_nod, surf_wk)
!
      use m_machine_parameter
!
      use sum_normal_4_surf_group
      use position_of_each_surface
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
!
      type(surface_group_geometry), intent(inout) :: sf_grp_v
      type(surface_node_grp_data), intent(inout) :: sf_grp_nod
      type(work_surface_element_mat), intent(inout) :: surf_wk
!
!
      call alloc_int_surf_data                                          &
     &   (sf_grp%num_item, surf%nnod_4_surf, surf_wk)
!
      call alloc_vectors_surf_group                                     &
     &   (sf_grp%num_grp, sf_grp%num_item, sf_grp_v)
!
      if (iflag_debug.eq.1) write(*,*) 's_sum_normal_4_surf_group'
      call s_sum_normal_4_surf_group(ele, sf_grp,                       &
     &     sf_grp_v)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_vect_surf_grp_nod'
      call alloc_vect_surf_grp_nod(sf_grp_nod)
!
      end subroutine empty_surface_parameters
!
!-----------------------------------------------------------------------
!
      end module int_surface_params_MHD
