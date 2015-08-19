!refined_nod_2_mesh_data.f90
!      module refined_nod_2_mesh_data
!
!     Written by H. Matsui on Oct., 2007
!
!      subroutine s_refined_nod_2_mesh_data(org_node, new_node)
!      subroutine s_refined_ele_2_mesh_data
!
      module refined_nod_2_mesh_data
!
      use m_precision
!
      use t_geometry_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_refined_nod_2_mesh_data(org_node, new_node)
!
      use m_refined_node_id
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
!
      integer(kind = kint) :: inod, icou
!
!
      new_node%numnod = ntot_nod_refine_nod + ntot_nod_refine_edge      &
     &          + ntot_nod_refine_surf + ntot_nod_refine_ele
      new_node%internal_node = new_node%numnod
!
      call allocate_node_geometry_type(new_node)
!
!
      do inod = 1, new_node%numnod
        new_node%inod_global(inod) = inod
      end do
!
      do inod = 1, new_node%numnod
        new_node%xx(inod,1) = org_node%xx(inod,1)
        new_node%xx(inod,2) = org_node%xx(inod,2)
        new_node%xx(inod,3) = org_node%xx(inod,3)
      end do
!
      icou = ntot_nod_refine_nod
      do inod = 1, ntot_nod_refine_edge
        icou = icou + 1
        new_node%xx(icou,1) =  x_refine_edge(inod,1)
        new_node%xx(icou,2) =  x_refine_edge(inod,2)
        new_node%xx(icou,3) =  x_refine_edge(inod,3)
      end do
!
      icou = ntot_nod_refine_nod + ntot_nod_refine_edge
      do inod = 1, ntot_nod_refine_surf
        icou = icou + 1
        new_node%xx(icou,1) =  x_refine_surf(inod,1)
        new_node%xx(icou,2) =  x_refine_surf(inod,2)
        new_node%xx(icou,3) =  x_refine_surf(inod,3)
      end do
!
      icou = ntot_nod_refine_nod + ntot_nod_refine_edge                 &
     &      + ntot_nod_refine_surf
      do inod = 1, ntot_nod_refine_ele
        icou = icou + 1
        new_node%xx(icou,1) =  x_refine_ele(inod,1)
        new_node%xx(icou,2) =  x_refine_ele(inod,2)
        new_node%xx(icou,3) =  x_refine_ele(inod,3)
      end do
!
      call deallocate_refined_xyz
!
      end subroutine s_refined_nod_2_mesh_data
!
!  ---------------------------------------------------------------------
!
      subroutine s_refined_ele_2_mesh_data(new_ele)
!
      use m_geometry_constants
      use m_refined_element_data
!
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint) :: elmtyp_2
      integer(kind = kint) :: iele, k1
!
!
      new_ele%numele =       ntot_ele_refined
      new_ele%internal_ele = new_ele%numele
      new_ele%nnod_4_ele =   nnod_4_ele_refined
      if (new_ele%nnod_4_ele .eq. num_t_lag) then
        elmtyp_2 = 333
      else if (new_ele%nnod_4_ele .eq. num_t_quad) then
        elmtyp_2 = 332
      else
        elmtyp_2 = 331
      end if
!
      call allocate_ele_connect_type(new_ele)
!
      do iele = 1, ntot_ele_refined
        new_ele%iele_global(iele) = iele
        new_ele%nodelm(iele) = new_ele%nnod_4_ele
        new_ele%elmtyp(iele) = elmtyp_2
      end do
!
      do k1 = 1, new_ele%nnod_4_ele
        new_ele%ie(1:ntot_ele_refined,k1)                               &
     &          = ie_refined(1:ntot_ele_refined,k1)
      end do
!
      end subroutine s_refined_ele_2_mesh_data
!
! ----------------------------------------------------------------------
!
      end module refined_nod_2_mesh_data
