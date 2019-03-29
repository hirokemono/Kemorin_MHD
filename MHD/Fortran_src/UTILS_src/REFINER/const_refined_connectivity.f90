!
!   module   const_refined_connectivity
!
!   Written by H. Matsui on Oct., 2007
!
!!      subroutine s_const_refined_connectivity                         &
!!     &         (ele, surf, edge, refine_p, refine_tbl, refine_ele,    &
!!     &          refine_surf, refine_edge)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(ctl_param_4_refiner), intent(in) :: refine_p
!!        type(element_refine_table), intent(inout) :: refine_tbl
!!        type(table_4_refine), intent(inout) :: refine_ele
!!        type(table_4_refine), intent(inout) :: refine_surf, refine_edge
!
      module   const_refined_connectivity
!
      use m_precision
      use m_constants
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_control_param_4_refiner
      use t_refined_node_id
      use t_refined_element_data
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_refined_connectivity                           &
     &         (ele, surf, edge, refine_p, refine_tbl, refine_ele,      &
     &          refine_surf, refine_edge)
!
      use m_refine_flag_parameters
!
      use set_refined_connection
      use set_local_refined_node
      use cal_minmax_and_stacks
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(ctl_param_4_refiner), intent(in) :: refine_p
!
      type(element_refine_table), intent(inout) :: refine_tbl
      type(table_4_refine), intent(inout) :: refine_ele
      type(table_4_refine), intent(inout) :: refine_surf, refine_edge
!
      integer(kind = kint) :: iele
!
!
      call alloc_refined_num_element(ele, refine_tbl)
!
      do iele = 1, ele%numele
        call count_refined_connection                                   &
     &     (iele, refine_tbl%iflag_refine_ele(iele),                    &
     &      ele%numele, refine_tbl%num_ele_refined)
      end do
!
      call s_cal_total_and_stacks                                       &
     &   (ele%numele, refine_tbl%num_ele_refined, izero,                &
     &    refine_tbl%istack_ele_refined, refine_tbl%ntot_ele_refined)
!
!      call check_refine_stack(ele%numele)
!
      if (refine_p%iflag_refine_type(1) .eq. iflag_8_to_20) then
        refine_tbl%nnod_4_ele_refined = num_t_quad
      else if (refine_p%iflag_refine_type(1) .eq. iflag_8_to_27) then
        refine_tbl%nnod_4_ele_refined = num_t_lag
      else
        refine_tbl%nnod_4_ele_refined = num_t_linear
      end if
!
      call alloc_refined_ele_connect(refine_tbl)
!
      do iele = 1, ele%numele
!
!         write(*,*) 'iflag_refine_ele', iele, iflag_refine_ele(iele)
!
        call refined_node_on_ele_2_local                                &
     &     (iele, ele, refine_tbl%iflag_refine_ele,                     &
     &      refine_ele%ntot_nod_refine, refine_ele%num_nod_refine,      &
     &      refine_ele%istack_nod_refine, refine_ele%inod_refine,       &
     &      refine_tbl%inod_refine_nod_local,                           &
     &      refine_tbl%inod_refine_ele_local)
        call refined_node_on_surf_2_local                               &
     &     (iele, surf, refine_tbl%iflag_refine_surf,                   &
     &      refine_surf%ntot_nod_refine, refine_surf%num_nod_refine,    &
     &      refine_surf%istack_nod_refine, refine_surf%inod_refine,     &
     &      refine_tbl%inod_refine_surf_local)
        call refined_node_on_edge_2_local(iele, edge,                   &
     &      refine_edge%ntot_nod_refine, refine_edge%num_nod_refine,    &
     &      refine_edge%istack_nod_refine, refine_edge%inod_refine,     &
     &      refine_tbl%inod_refine_edge_local)
!
        call s_set_refined_connection                                   &
     &     (iele, refine_tbl%iflag_refine_ele(iele), ele%numele,        &
     &      refine_tbl%num_ele_refined, refine_tbl%istack_ele_refined,  &
     &      refine_tbl%ntot_ele_refined, refine_tbl%nnod_4_ele_refined, &
     &      refine_tbl%inod_refine_ele_local,                           &
     &      refine_tbl%inod_refine_surf_local,                          &
     &      refine_tbl%inod_refine_edge_local,                          &
     &      refine_tbl%inod_refine_nod_local,                           &
     &      refine_tbl%inod_refine_local, refine_tbl%ie_refined)
!
!
        if(refine_tbl%iflag_refine_ele(iele) .gt. 0) then
          refine_tbl%ilevel_refine(iele) = 1
        end if
!        if(iele .eq. ele%numele) write(*,*) 'hehehe ',                 &
!     &                   edge%iedge_4_ele(ele%numele,1:nedge_4_ele)
!
      end do
      refine_tbl%max_refine_level = refine_tbl%max_refine_level + 1
!
      end subroutine s_const_refined_connectivity
!
! ----------------------------------------------------------------------
!
      end module const_refined_connectivity
