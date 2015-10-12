!int_surface_params_MHD.f90
!     module int_surface_params_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!
!      subroutine int_surface_parameters
!
      module int_surface_params_MHD
!
      use m_precision
!
      implicit none
!
      private :: cal_surf_norm_node
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_surface_parameters(num_surf)
!
      use m_machine_parameter
      use m_control_parameter
      use m_geometry_data
      use m_group_data
      use m_finite_element_matrix
      use m_int_surface_data
!
      use position_of_each_surface
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
!
      use check_finite_element_mat
!
      integer(kind = kint), intent(in) :: num_surf
!
!
      if (num_surf .le. 0) return
!
      call allocate_int_surf_data(sf_grp1%num_item, surf1%nnod_4_surf)
!
      if (iflag_debug.eq.1)  write(*,*) 'pick_normal_of_surf_group'
      call pick_normal_of_surf_group                                    &
     &   (surf1, sf_grp1, sf_grp_tbl1, sf_grp_v1)
!
      if (iflag_debug.eq.1)  write(*,*) 's_sum_normal_4_surf_group'
      call s_sum_normal_4_surf_group(sf_grp1, sf_grp_v1)
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_surf_norm_node'
      call cal_surf_norm_node(sf_grp1, sf_grp_v1, sf_grp_nod1)
!
!
      if (iflag_debug.eq.1)  write(*,*) 'position_2_each_surface'
      call position_2_each_surface(sf_grp1)
!
      if (iflag_debug.eq.1)  write(*,*) 'delta_x_2_each_surface'
      call delta_x_2_each_surface(sf_grp1)
!
!      call check_surface_param_smp('int_surface_parameters end',       &
!     &    my_rank, sf_grp1, sf_grp_nod1)
!
      end subroutine int_surface_parameters
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_surf_norm_node(sf_grp, sf_grp_v, sf_grp_nod)
!
      use m_geometry_data
      use t_group_data
      use t_surface_group_connect
      use t_surface_group_geometry
      use set_norm_nod_4_surf_grp
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_geometry), intent(in) :: sf_grp_v
      type(surface_node_grp_data), intent(inout) :: sf_grp_nod
!
!
      call allocate_work_norm_nod(node1%numnod)
      call alloc_vect_surf_grp_nod(sf_grp_nod)
!
      call cal_surf_grp_norm_node(ele1%numele, ele1%nnod_4_ele,         &
     &    surf1%nnod_4_surf, surf1%node_on_sf, ele1%ie,                 &
     &    sf_grp%num_grp, sf_grp%num_item,                              &
     &    sf_grp%istack_grp, sf_grp%item_sf_grp,                        &
     &    sf_grp_v%vnorm_sf_grp, sf_grp_v%a_area_sf_grp,                &
     &    sf_grp_nod%ntot_node_sf_grp, sf_grp_nod%inod_stack_sf_grp,    &
     &    sf_grp_nod%inod_surf_grp, sf_grp_nod%surf_norm_nod,           &
     &    sf_grp_nod%coef_sf_nod)
!
      call deallocate_work_norm_nod
!
      end subroutine cal_surf_norm_node
!
!-----------------------------------------------------------------------
!
      end module int_surface_params_MHD
