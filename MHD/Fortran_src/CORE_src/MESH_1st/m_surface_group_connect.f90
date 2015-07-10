!m_surface_group_connect.f90
!     module m_surface_group_connect
!
!> @brief connectivity data for surface group items
!
!     Writteg by H.Matsui on Aug., 2006
!
!      subroutine const_surf_group_connect
!      subroutine deallocate_surf_group_connect
!      subroutine set_surface_node_grp
!      subroutine cal_surf_norm_node
!
      module m_surface_group_connect
!
      use m_precision
      use t_group_connects
      use t_surface_group_connect
!
      implicit  none
!
!
!>   Structure of connectivities for surface group
      type(surface_group_table), save :: sf_grp_data1
!> Structure of connectivity data for surface group items
      type(surface_node_grp_data), save :: sf_grp_nod1
!
      private :: set_surf_id_4_surf_group, set_edge_4_surf_group
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_surf_group_connect
!
      use m_machine_parameter
!
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_id_4_surf_group'
      call set_surf_id_4_surf_group
!
       if (iflag_debug.eq.1) write(*,*) 'set_edge_4_surf_group'
      call set_edge_4_surf_group
!
      end subroutine const_surf_group_connect
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_group_connect
!
!
      call dealloc_grp_connect(sf_grp_data1%edge)
      call dealloc_surf_item_sf_grp_type(sf_grp_data1)
      call dealloc_num_surf_grp_nod_smp(sf_grp_nod1)
      call dealloc_surf_grp_nod(sf_grp_nod1)
!
      end subroutine deallocate_surf_group_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_surf_id_4_surf_group
!
      use m_geometry_parameter
      use m_geometry_data
      use m_surface_group
      use set_surface_id_4_surf_grp
!
!
      call alloc_surf_item_sf_grp_type(sf_grp1%num_item, sf_grp_data1)
!
      call set_surface_id_4_surf_group(numele, isf_4_ele,               &
     &    sf_grp1%num_grp, sf_grp1%num_item, sf_grp1%istack_grp,        &
     &    surf_item, sf_grp_data1%isurf_grp, sf_grp_data1%isurf_grp_n)
!
      end subroutine set_surf_id_4_surf_group
!
!-----------------------------------------------------------------------
!
      subroutine set_edge_4_surf_group
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_surface_group
!
      use set_node_4_group
!
      integer(kind=kint), allocatable :: imark_4_grp(:)
!
!
      allocate(imark_4_grp(numedge))
      if(numedge .gt. 0) imark_4_grp = 0
!
      call alloc_num_other_grp(sf_grp1%num_grp, sf_grp_data1%edge)
!
      call count_nod_4_ele_grp(numedge, numsurf, nedge_4_surf,          &
     &    iedge_4_sf, sf_grp1%num_grp, sf_grp1%num_item,                &
     &    sf_grp1%istack_grp, sf_grp_data1%isurf_grp,                   &
     &    sf_grp_data1%edge%ntot_e_grp, sf_grp_data1%edge%nitem_e_grp,  &
     &    sf_grp_data1%edge%istack_e_grp, imark_4_grp)
!
      call alloc_item_other_grp(sf_grp_data1%edge)
!
      call set_nod_4_ele_grp(numedge, numsurf, nedge_4_surf,            &
     &    iedge_4_sf, sf_grp1%num_grp, sf_grp1%num_item,                &
     &    sf_grp1%istack_grp, sf_grp_data1%isurf_grp,                   &
     &    sf_grp_data1%edge%ntot_e_grp, sf_grp_data1%edge%nitem_e_grp,  &
     &    sf_grp_data1%edge%istack_e_grp, sf_grp_data1%edge%item_e_grp, &
     &    imark_4_grp)
!
      deallocate(imark_4_grp)
!
      end subroutine set_edge_4_surf_group
!
!-----------------------------------------------------------------------
!
      subroutine set_surface_node_grp
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_surface_group
      use set_surface_node
      use set_smp_4_groups
      use cal_minmax_and_stacks
!
!
      call allocate_make_4_surf_nod_grp(numnod)
!
      call alloc_num_surf_grp_nod(sf_grp1%num_grp, sf_grp_nod1)
!
      call count_surf_nod_grp_stack(np_smp, inod_smp_stack,             &
     &    numele, nnod_4_ele, ie, nnod_4_surf, node_on_sf,              &
     &    sf_grp1%num_grp, sf_grp1%num_item,                            &
     &    sf_grp1%istack_grp, surf_item,    &
     &    sf_grp_nod1%ntot_node_sf_grp, sf_grp_nod1%nnod_sf_grp,        &
     &    sf_grp_nod1%inod_stack_sf_grp)
!
!
      call alloc_num_surf_grp_nod_smp(num_surf_smp, sf_grp_nod1)
!
      call set_group_size_4_smp                                         &
     &   (np_smp, sf_grp1%num_grp, sf_grp_nod1%inod_stack_sf_grp,       &
     &    sf_grp_nod1%istack_surf_nod_smp,                              &
     &    sf_grp_nod1%max_sf_nod_4_smp)
!
!
!
      if (sf_grp_nod1%ntot_node_sf_grp .gt. 0) then
        call alloc_item_surf_grp_nod(sf_grp_nod1)
!
        call set_surf_nod_grp_item(numnod, numele, nnod_4_ele, ie,      &
     &      nnod_4_surf, node_on_sf, node_on_sf_n, sf_grp1%num_grp,     &
     &      sf_grp1%num_item, sf_grp1%istack_grp, surf_item,            &
     &      sf_grp_nod1%ntot_node_sf_grp,                               &
     &      sf_grp_nod1%inod_stack_sf_grp, sf_grp_nod1%inod_surf_grp,   &
     &      sf_grp_nod1%surf_node_n, sf_grp_nod1%num_sf_4_nod)
      end if
!
      call deallocate_make_4_surf_nod_grp
!
      end subroutine set_surface_node_grp
!
! -----------------------------------------------------------------------
!
      subroutine cal_surf_norm_node
!
      use m_geometry_parameter
      use m_geometry_data
      use m_surface_group
      use m_surface_group_geometry
      use set_norm_nod_4_surf_grp
!
!
      call allocate_work_norm_nod(numnod)
      call alloc_vect_surf_grp_nod(sf_grp_nod1)
!
      call cal_surf_grp_norm_node(numele, nnod_4_ele,                   &
     &    nnod_4_surf, node_on_sf, ie,                                  &
     &    sf_grp1%num_grp, sf_grp1%num_item,                            &
     &    sf_grp1%istack_grp, surf_item,    &
     &    sf_grp_v1%vnorm_sf_grp, sf_grp_v1%a_area_sf_grp,              &
     &    sf_grp_nod1%ntot_node_sf_grp, sf_grp_nod1%inod_stack_sf_grp,  &
     &    sf_grp_nod1%inod_surf_grp, sf_grp_nod1%surf_norm_nod,         &
     &    sf_grp_nod1%coef_sf_nod)
!
      call deallocate_work_norm_nod
!
      end subroutine cal_surf_norm_node
!
!-----------------------------------------------------------------------
!
      end module m_surface_group_connect
