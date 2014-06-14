!refined_nod_2_mesh_data.f90
!      module refined_nod_2_mesh_data
!
      module refined_nod_2_mesh_data
!
!     Written by H. Matsui on Oct., 2007
!
      use m_precision
!
      implicit none
!
!      subroutine s_refined_nod_2_mesh_data
!      subroutine s_refined_ele_2_mesh_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_refined_nod_2_mesh_data
!
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_data
      use m_2nd_nod_comm_table
      use m_refined_node_id
!
      integer(kind = kint) :: inod, icou
!
!
      num_neib_2 = 0
!
      node_2nd%numnod = ntot_nod_refine_nod + ntot_nod_refine_edge      &
     &          + ntot_nod_refine_surf + ntot_nod_refine_ele
      node_2nd%internal_node = node_2nd%numnod
!
      call allocate_node_geometry_type(node_2nd)
!
!
      do inod = 1, node_2nd%numnod
        node_2nd%inod_global(inod) = inod
      end do
!
      node_2nd%xx(1:numnod,1) = xx(1:numnod,1)
      node_2nd%xx(1:numnod,2) = xx(1:numnod,2)
      node_2nd%xx(1:numnod,3) = xx(1:numnod,3)
!
      icou = ntot_nod_refine_nod
      do inod = 1, ntot_nod_refine_edge
        icou = icou + 1
        node_2nd%xx(icou,1) =  x_refine_edge(inod,1)
        node_2nd%xx(icou,2) =  x_refine_edge(inod,2)
        node_2nd%xx(icou,3) =  x_refine_edge(inod,3)
      end do
!
      icou = ntot_nod_refine_nod + ntot_nod_refine_edge
      do inod = 1, ntot_nod_refine_surf
        icou = icou + 1
        node_2nd%xx(icou,1) =  x_refine_surf(inod,1)
        node_2nd%xx(icou,2) =  x_refine_surf(inod,2)
        node_2nd%xx(icou,3) =  x_refine_surf(inod,3)
      end do
!
      icou = ntot_nod_refine_nod + ntot_nod_refine_edge                 &
     &      + ntot_nod_refine_surf
      do inod = 1, ntot_nod_refine_ele
        icou = icou + 1
        node_2nd%xx(icou,1) =  x_refine_ele(inod,1)
        node_2nd%xx(icou,2) =  x_refine_ele(inod,2)
        node_2nd%xx(icou,3) =  x_refine_ele(inod,3)
      end do
!
      call deallocate_refined_xyz
!
      end subroutine s_refined_nod_2_mesh_data
!
!  ---------------------------------------------------------------------
!
      subroutine s_refined_ele_2_mesh_data
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_2nd_geometry_data
      use m_refined_element_data
!
      integer(kind = kint) :: elmtyp_2
      integer(kind = kint) :: iele, k1
!
!
      ele_2nd%numele =       ntot_ele_refined
      ele_2nd%internal_ele = ele_2nd%numele
      ele_2nd%nnod_4_ele =   nnod_4_ele_refined
      if (ele_2nd%nnod_4_ele .eq. num_t_lag) then
        elmtyp_2 = 333
        surf_2nd%nnod_4_surf = num_lag_sf
        edge_2nd%nnod_4_edge = num_quad_edge
      else if (ele_2nd%nnod_4_ele .eq. num_t_quad) then
        elmtyp_2 = 332
        surf_2nd%nnod_4_surf = num_quad_sf
        edge_2nd%nnod_4_edge = num_quad_edge
      else
        elmtyp_2 = 331
        surf_2nd%nnod_4_surf = num_linear_sf
        edge_2nd%nnod_4_edge = num_linear_edge
      end if
!
      call allocate_ele_connect_type(ele_2nd)
!
      do iele = 1, ntot_ele_refined
        ele_2nd%iele_global(iele) = iele
        ele_2nd%nodelm(iele) = ele_2nd%nnod_4_ele
        ele_2nd%elmtyp(iele) = elmtyp_2
      end do
!
      do k1 = 1, ele_2nd%nnod_4_ele
        ele_2nd%ie(1:ntot_ele_refined,k1)                               &
     &          = ie_refined(1:ntot_ele_refined,k1)
      end do
!
      end subroutine s_refined_ele_2_mesh_data
!
! ----------------------------------------------------------------------
!
      end module refined_nod_2_mesh_data
