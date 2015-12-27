!
!   module   const_refined_connectivity
!
!   Written by H. Matsui on Oct., 2007
!
!      subroutine s_const_refined_connectivity(ele, surf, edge)
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
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_refined_connectivity(ele, surf, edge)
!
      use m_refine_flag_parameters
      use m_control_param_4_refiner
      use m_refined_element_data
!
      use set_refined_connection
      use set_local_refined_node
      use cal_minmax_and_stacks
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      integer(kind = kint) :: iele
!
!
      call allocate_refined_num_element(ele%numele)
!
      do iele = 1, ele%numele
        call count_refined_connection(iele, iflag_refine_ele(iele) )
      end do
!
      call s_cal_total_and_stacks(ele%numele, num_ele_refined, izero,   &
     &    istack_ele_refined, ntot_ele_refined)
!
!      call check_refine_stack(ele%numele)
!
      if (iflag_refine_type(1) .eq. iflag_8_to_20) then
        nnod_4_ele_refined = num_t_quad
      else if (iflag_refine_type(1) .eq. iflag_8_to_27) then
        nnod_4_ele_refined = num_t_lag
      else
        nnod_4_ele_refined = num_t_linear
      end if
!
      call allocate_refined_ele_connect
!
      do iele = 1, ele%numele
!
!         write(*,*) 'iflag_refine_ele', iele, iflag_refine_ele(iele)
!
        call refined_node_on_ele_2_local(iele, ele)
        call refined_node_on_surf_2_local(iele, surf)
        call refined_node_on_edge_2_local(iele, edge)
!
        call s_set_refined_connection(iele, iflag_refine_ele(iele) )
!
!
        if(iflag_refine_ele(iele) .gt. 0) then
          ilevel_refine(iele) = 1
        end if
!        if(iele.eq.ele1%numele) write(*,*) 'hehehe ',                  &
!     &                   edge%iedge_4_ele(ele1%numele,1:nedge_4_ele)
!
      end do
      max_refine_level = max_refine_level + 1
!
      end subroutine s_const_refined_connectivity
!
! ----------------------------------------------------------------------
!
      end module const_refined_connectivity
