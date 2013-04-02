!set_surface_node_grp_type.f90
!     module set_surface_node_grp_type
!
!
!        programmed by H. Matsui on Dec., 2010
!
!      subroutine s_set_surface_node_grp_type(nod, ele, surf, sf_grp,   &
!     &          sf_nod)
!        type(node_data),    intent(in) ::       nod
!        type(element_data), intent(in) ::       ele
!        type(surface_data), intent(in) ::       surf
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surface_node_grp_data), intent(inout) :: sf_nod
!
!      subroutine empty_surface_node_grp_type(sf_grp, sf_nod)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surface_node_grp_data), intent(inout) :: sf_nod
!
      module set_surface_node_grp_type
!
      use m_precision
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_surface_node_grp_type(nod, ele, surf, sf_grp,    &
     &          sf_nod)
!
      use m_machine_parameter
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_surface_group_connect
      use set_surface_node
      use set_smp_4_group_types
!
      type(node_data),    intent(in) ::       nod
      type(element_data), intent(in) ::       ele
      type(surface_data), intent(in) ::       surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      call allocate_make_4_surf_nod_grp(nod%numnod)
!
      call alloc_num_surf_grp_nod(sf_grp, sf_nod)
!
      call count_surf_nod_grp_stack(np_smp, nod%istack_nod_smp,         &
     &    ele%numele, ele%nnod_4_ele, ele%ie, surf%nnod_4_surf,         &
     &    surf%node_on_sf, sf_grp%num_grp, sf_grp%num_item,             &
     &    sf_grp%istack_grp, sf_grp%item_sf_grp,                        &
     &    sf_nod%ntot_node_sf_grp, sf_nod%nnod_sf_grp,                  &
     &    sf_nod%inod_stack_sf_grp)
!
      call count_surf_nod_grp_type_smp(sf_grp, sf_nod)
!
      call alloc_item_surf_grp_nod(sf_nod)
      if (sf_nod%ntot_node_sf_grp .gt. 0) then
        call set_surf_nod_grp_item(nod%numnod, ele%numele,              &
     &      ele%nnod_4_ele, ele%ie, surf%nnod_4_surf,                   &
     &      surf%node_on_sf, surf%node_on_sf_n,                         &
     &      sf_grp%num_grp, sf_grp%num_item,                            &
     &      sf_grp%istack_grp, sf_grp%item_sf_grp,                      &
     &      sf_nod%ntot_node_sf_grp, sf_nod%inod_stack_sf_grp,          &
     &      sf_nod%inod_surf_grp, sf_nod%surf_node_n,                   &
     &      sf_nod%num_sf_4_nod)
      end if
!
      call deallocate_make_4_surf_nod_grp
!
      end subroutine s_set_surface_node_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine empty_surface_node_grp_type(sf_grp, sf_nod)
!
      use t_group_data
      use t_surface_group_connect
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      sf_nod%ntot_node_sf_grp = 0
      call alloc_num_surf_grp_nod(sf_grp, sf_nod)
!
      sf_nod%ntot_node_sf_grp = 0
      call alloc_item_surf_grp_nod(sf_nod)
!
      end subroutine empty_surface_node_grp_type
!
!-----------------------------------------------------------------------
!
      end module set_surface_node_grp_type
