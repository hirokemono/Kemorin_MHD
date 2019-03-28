!refined_nod_2_mesh_data.f90
!      module refined_nod_2_mesh_data
!
!     Written by H. Matsui on Oct., 2007
!
!!      subroutine s_refined_nod_2_mesh_data(org_node,                  &
!!     &          refine_nod, refine_ele, refine_surf, refine_edge,     &
!!     &          new_node)
!!        type(node_data), intent(in) :: org_node
!!        type(table_4_refine), intent(in) :: refine_nod, refine_ele
!!        type(table_4_refine), intent(in) :: refine_surf, refine_edge
!!        type(node_data), intent(inout) :: new_node
!!      subroutine s_refined_ele_2_mesh_data(refine_tbl, new_ele)
!!        type(element_refine_table), intent(in) :: refine_tbl
!!        type(element_data), intent(inout) :: new_ele
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
      subroutine s_refined_nod_2_mesh_data(org_node,                    &
     &          refine_nod, refine_ele, refine_surf, refine_edge,       &
     &          new_node)
!
      use t_refined_node_id
!
      type(node_data), intent(in) :: org_node
      type(table_4_refine), intent(in) :: refine_nod, refine_ele
      type(table_4_refine), intent(in) :: refine_surf, refine_edge
!
      type(node_data), intent(inout) :: new_node
!
      integer(kind = kint) :: inod, icou
!
!
      new_node%numnod =  refine_nod%ntot_nod_refine                     &
     &                 + refine_edge%ntot_nod_refine                    &
     &                 + refine_surf%ntot_nod_refine                    &
     &                 + refine_ele%ntot_nod_refine
      new_node%internal_node = new_node%numnod
!
      call alloc_node_geometry_w_sph(new_node)
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
      icou = refine_nod%ntot_nod_refine
      do inod = 1, refine_edge%ntot_nod_refine
        icou = icou + 1
        new_node%xx(icou,1) =  refine_edge%x_refine(inod,1)
        new_node%xx(icou,2) =  refine_edge%x_refine(inod,2)
        new_node%xx(icou,3) =  refine_edge%x_refine(inod,3)
      end do
!
      icou = refine_nod%ntot_nod_refine + refine_edge%ntot_nod_refine
      do inod = 1, refine_surf%ntot_nod_refine
        icou = icou + 1
        new_node%xx(icou,1) =  refine_surf%x_refine(inod,1)
        new_node%xx(icou,2) =  refine_surf%x_refine(inod,2)
        new_node%xx(icou,3) =  refine_surf%x_refine(inod,3)
      end do
!
      icou = refine_nod%ntot_nod_refine + refine_edge%ntot_nod_refine   &
     &      + refine_surf%ntot_nod_refine
      do inod = 1, refine_ele%ntot_nod_refine
        icou = icou + 1
        new_node%xx(icou,1) =  refine_ele%x_refine(inod,1)
        new_node%xx(icou,2) =  refine_ele%x_refine(inod,2)
        new_node%xx(icou,3) =  refine_ele%x_refine(inod,3)
      end do
!
      end subroutine s_refined_nod_2_mesh_data
!
!  ---------------------------------------------------------------------
!
      subroutine s_refined_ele_2_mesh_data(refine_tbl, new_ele)
!
      use m_geometry_constants
      use t_refined_element_data
!
      type(element_refine_table), intent(in) :: refine_tbl
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint) :: elmtyp_2
      integer(kind = kint) :: iele, k1
!
!
      new_ele%numele =       refine_tbl%ntot_ele_refined
      new_ele%internal_ele = new_ele%numele
      new_ele%nnod_4_ele =   refine_tbl%nnod_4_ele_refined
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
      do iele = 1, new_ele%numele
        new_ele%iele_global(iele) = iele
        new_ele%nodelm(iele) = new_ele%nnod_4_ele
        new_ele%elmtyp(iele) = elmtyp_2
      end do
!
      do k1 = 1, new_ele%nnod_4_ele
        new_ele%ie(1:new_ele%numele,k1)                                 &
     &          = refine_tbl%ie_refined(1:new_ele%numele,k1)
      end do
!
      end subroutine s_refined_ele_2_mesh_data
!
! ----------------------------------------------------------------------
!
      end module refined_nod_2_mesh_data
