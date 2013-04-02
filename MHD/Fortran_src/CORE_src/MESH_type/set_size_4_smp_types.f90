!
!      module set_size_4_smp_types
!
!     Written by H. Matsui on Dec., 2008
!
!> @brief set numbers for SMP parallelization
!
!      subroutine count_size_4_smp_mesh_type(nod, ele)
!        type(node_data),    intent(inout) :: nod
!        type(element_data), intent(inout) :: ele
!      subroutine count_surf_size_smp_type(surf)
!        type(surface_data), intent(inout) :: surf
!      subroutine count_edge_size_smp_type(edge)
!        type(edge_data),    intent(inout) :: edge
!
!      subroutine count_overlap_ele_type(nod, ele)
!        type(node_data),    intent(in) :: nod
!        type(element_data), intent(inout) :: ele
!      subroutine count_overlap_surf_type(nod, surf)
!        type(node_data),    intent(in) :: nod
!        type(surface_data), intent(inout) :: surf
!      subroutine count_overlap_edge_type(nod, edge)
!        type(node_data),    intent(in) :: nod
!        type(edge_data),    intent(inout) :: edge
!
!      subroutine set_gl_ele_type_by_local(ele)
!        type(element_data), intent(inout) :: ele
!      subroutine set_gl_surf_type_by_local(surf)
!        type(surface_data), intent(inout) :: surf
!      subroutine set_gl_edge_type_by_local(edge)
!        type(edge_data), intent(inout) :: edge
!
      module set_size_4_smp_types
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_size_4_smp_mesh_type(nod, ele)
!
      use t_geometry_data
      use cal_minmax_and_stacks
!
      type(node_data),    intent(inout) :: nod
      type(element_data), intent(inout) :: ele
!
!
      call allocate_node_param_smp_type(nod)
      call allocate_ele_param_smp_type(ele)
!
      call count_number_4_smp( np_smp, ione, nod%numnod,                &
     &    nod%istack_nod_smp, nod%max_nod_smp )
!
      call count_number_4_smp( np_smp, ione, nod%internal_node,         &
     &    nod%istack_internal_smp, nod%max_internal_nod_smp )
!
      call count_number_4_smp( np_smp, ione, ele%numele,                &
     &    ele%istack_ele_smp, ele%max_ele_smp )
!
      end subroutine count_size_4_smp_mesh_type
!
!-----------------------------------------------------------------------
!
      subroutine count_surf_size_smp_type(surf)
!
      use t_surface_data
      use cal_minmax_and_stacks
!
      type(surface_data), intent(inout) :: surf
!
!
      call allocate_surf_param_smp_type(surf)
!
      call count_number_4_smp( np_smp, ione, surf%numsurf,              &
     &    surf%istack_surf_smp, surf%max_surf_smp )
!
      end subroutine count_surf_size_smp_type
!
!-----------------------------------------------------------------------
!
      subroutine count_edge_size_smp_type(edge)
!
      use t_edge_data
      use cal_minmax_and_stacks
!
      type(edge_data),    intent(inout) :: edge
!
!
      call allocate_edge_param_smp_type(edge)
!
      call count_number_4_smp( np_smp, ione, edge%numedge,              &
     &    edge%istack_edge_smp, edge%max_edge_smp )
!
      end subroutine count_edge_size_smp_type
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_overlap_ele_type(nod, ele)
!
      use t_geometry_data
      use count_overlap
!
      type(node_data),    intent(in) :: nod
      type(element_data), intent(inout) :: ele
!
      integer (kind = kint) :: ip, iele, ist, ied
!
!
      call set_overlap_flag(np_smp, ele%istack_ele_smp,                 &
     &    nod%internal_node, ele%numele, ele%ie(1:ele%numele,1),        &
     &    ele%internal_ele, ele%interior_ele)
!
      call copy_real_overlap_flag(np_smp, ele%istack_ele_smp,           &
     &    ele%numele, ele%interior_ele, ele%e_multi)
!
      end subroutine count_overlap_ele_type
!
! ----------------------------------------------------------------------
!
      subroutine count_overlap_surf_type(nod, surf)
!
      use t_geometry_data
      use t_surface_data
      use count_overlap
!
      type(node_data),    intent(in) :: nod
      type(surface_data), intent(inout) :: surf
!
!
      call set_overlap_flag(np_smp, surf%istack_surf_smp,               &
     &    nod%internal_node, surf%numsurf,                              &
     &    surf%ie_surf(1:surf%numsurf,1), surf%internal_surf,           &
     &    surf%interior_surf)
!
      end subroutine count_overlap_surf_type
!
! ----------------------------------------------------------------------
!
      subroutine count_overlap_edge_type(nod, edge)
!
      use t_geometry_data
      use t_edge_data
      use count_overlap
!
      type(node_data),    intent(in) :: nod
      type(edge_data),    intent(inout) :: edge
!
!
      call set_overlap_flag(np_smp, edge%istack_edge_smp,               &
     &    nod%internal_node, edge%numedge,                              &
     &    edge%ie_edge(1:edge%numedge,1), edge%internal_edge,           &
     &    edge%interior_edge)
!
      end subroutine count_overlap_edge_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_gl_ele_type_by_local(ele)
!
      use  t_geometry_data
      use set_global_id_by_local
!
      type(element_data), intent(inout) :: ele
!
!
      call s_set_global_id_by_local(ele%numele, ele%internal_ele,       &
     &    ele%interior_ele, ele%iele_global)
!
      end subroutine set_gl_ele_type_by_local
!
! ----------------------------------------------------------------------
!
      subroutine set_gl_surf_type_by_local(surf)
!
      use t_surface_data
      use set_global_id_by_local
!
      type(surface_data), intent(inout) :: surf
!
!
      call s_set_global_id_by_local(surf%numsurf, surf%internal_surf,   &
     &    surf%interior_surf, surf%isurf_global)
!
      end subroutine set_gl_surf_type_by_local
!
! ----------------------------------------------------------------------
!
      subroutine set_gl_edge_type_by_local(edge)
!
      use t_edge_data
      use set_global_id_by_local
!
      type(edge_data), intent(inout) :: edge
!
!
      call s_set_global_id_by_local(edge%numedge, edge%internal_edge,   &
     &    edge%interior_edge, edge%iedge_global)
!
      end subroutine set_gl_edge_type_by_local
!
! ----------------------------------------------------------------------
!
      end module set_size_4_smp_types
