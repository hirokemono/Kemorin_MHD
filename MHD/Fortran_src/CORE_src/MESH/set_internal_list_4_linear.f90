!set_internal_list_4_linear.f90
!      module set_internal_list_4_linear
!
!     Written by H. Matsui on June, 2012
!
!      subroutine set_internal_list_4_linear_20(numnod, internal_node,  &
!     &          numele, numsurf, interior_ele, interior_surf,          &
!     &          nnod_l, nele_l, nsurf_l, nedge_l, ie_l, ie_surf_l,     &
!     &          ie_edge_l, interior_ele_l, interior_surf_l,            &
!     &          interior_edge_l)
!      subroutine set_internal_list_4_linear_27(numnod, internal_node,  &
!     &          numele, numsurf, interior_ele, interior_surf,          &
!     &          nnod_l, nele_l, nsurf_l, nedge_l, ie_l, ie_surf_l,     &
!     &          ie_edge_l, interior_ele_l, interior_surf_l,            &
!     &          interior_edge_l)
!
!
      module set_internal_list_4_linear
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable :: interior_node_l(:)
      private :: interior_node_l
!   flag for interior node
!
      private :: allocate_internal_node_list
      private :: deallocate_internal_node_list
      private :: set_interior_node_list, set_internal_ele_by_nod
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_internal_list_4_linear_20(numnod, internal_node,   &
     &          numele, numsurf, interior_ele, interior_surf,           &
     &          nnod_l, nele_l, nsurf_l, nedge_l, ie_l, ie_surf_l,      &
     &          ie_edge_l, interior_ele_l, interior_surf_l,             &
     &          interior_edge_l)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: numele, numsurf
!
      integer(kind = kint), intent(in) :: interior_ele(numele)
      integer(kind = kint), intent(in) :: interior_surf(numsurf)
!
      integer(kind = kint), intent(in) :: nnod_l, nele_l
      integer(kind = kint), intent(in) :: nsurf_l, nedge_l
      integer(kind = kint), intent(in) :: ie_l(nele_l,num_t_linear)
      integer(kind = kint), intent(in)                                  &
     &                     :: ie_surf_l(nsurf_l,num_linear_sf)
      integer(kind = kint), intent(in)                                  &
     &                     :: ie_edge_l(nedge_l,num_linear_edge)
!
      integer(kind = kint), intent(inout) :: interior_ele_l(nele_l)
      integer(kind = kint), intent(inout) :: interior_surf_l(nsurf_l)
      integer(kind = kint), intent(inout) :: interior_edge_l(nedge_l)
!
!
      call allocate_internal_node_list(nnod_l)
!
      call set_interior_node_list(numnod, internal_node,                &
     &    numsurf, numele, interior_ele, interior_surf)
!
      call set_internal_ele_by_nod(nele_l, num_t_linear, ie_l,          &
     &    interior_ele_l)
      call set_internal_ele_by_nod(nsurf_l, num_linear_sf, ie_surf_l,   &
     &    interior_surf_l)
      call set_internal_ele_by_nod(nedge_l, num_linear_edge, ie_edge_l, &
     &    interior_edge_l)
!
      call deallocate_internal_node_list
!
      end subroutine set_internal_list_4_linear_20
!
!  ---------------------------------------------------------------------
!
      subroutine set_internal_list_4_linear_27(numnod, internal_node,   &
     &          numele, numsurf, interior_ele, interior_surf,           &
     &          nnod_l, nele_l, nsurf_l, nedge_l, ie_l, ie_surf_l,      &
     &          ie_edge_l, interior_ele_l, interior_surf_l,             &
     &          interior_edge_l)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: numele, numsurf
!
      integer(kind = kint), intent(in) :: interior_ele(numele)
      integer(kind = kint), intent(in) :: interior_surf(numsurf)
!
      integer(kind = kint), intent(in) :: nnod_l, nele_l
      integer(kind = kint), intent(in) :: nsurf_l, nedge_l
      integer(kind = kint), intent(in) :: ie_l(nele_l,num_t_linear)
      integer(kind = kint), intent(in)                                  &
     &                     :: ie_surf_l(nsurf_l,num_linear_sf)
      integer(kind = kint), intent(in)                                  &
     &                     :: ie_edge_l(nedge_l,num_linear_edge)
!
      integer(kind = kint), intent(inout) :: interior_ele_l(nele_l)
      integer(kind = kint), intent(inout) :: interior_surf_l(nsurf_l)
      integer(kind = kint), intent(inout) :: interior_edge_l(nedge_l)
!
!
      call allocate_internal_node_list(nnod_l)
      interior_node_l(1:internal_node) = 1
!
      call set_internal_ele_by_nod(nele_l, num_t_linear, ie_l,          &
     &    interior_ele_l)
      call set_internal_ele_by_nod(nsurf_l, num_linear_sf, ie_surf_l,   &
     &    interior_surf_l)
      call set_internal_ele_by_nod(nedge_l, num_linear_edge, ie_edge_l, &
     &    interior_edge_l)
!
      call deallocate_internal_node_list
!
      end subroutine set_internal_list_4_linear_27
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_internal_node_list(nnod_l)
!
      integer(kind = kint),  intent(in) :: nnod_l
!
!
      allocate( interior_node_l(nnod_l) )
      interior_node_l = 0
!
      end subroutine allocate_internal_node_list
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_internal_node_list
!
      deallocate( interior_node_l )
!
      end subroutine deallocate_internal_node_list
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_interior_node_list(numnod, internal_node,          &
     &          numsurf, numele, interior_ele, interior_surf)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: numsurf, numele
      integer(kind = kint), intent(in) :: interior_surf(numsurf)
      integer(kind = kint), intent(in) :: interior_ele(numele)
!
      integer(kind = kint) :: inod, iele, isurf
!
!
!$omp parallel do
      do inod = 1, internal_node
        interior_node_l(inod) = 1
      end do
!$omp end parallel do
!
!$omp parallel do
      do inod = internal_node+1, numnod
        interior_node_l(inod) = 0
      end do
!$omp end parallel do
!
!$omp parallel do
      do isurf = 1, numsurf
        inod = numnod + isurf
        interior_node_l(inod) = interior_surf(isurf)
      end do
!$omp end parallel do
!
!$omp parallel do
      do iele = 1, numele
        inod = numnod + numsurf + iele
        interior_node_l(inod) = interior_ele(iele)
      end do
!$omp end parallel do
!
      end subroutine set_interior_node_list
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_internal_ele_by_nod(numele, nnod_4_ele,            &
     &          ie, interior_ele)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(inout) :: interior_ele(numele)
!
      integer(kind = kint) :: iele, inod1
!
!
!$omp parallel do private(iele,inod1)
      do iele = 1, numele
        inod1 = ie(iele,1)
        interior_ele(iele) = interior_node_l(inod1)
      end do
!$omp end parallel do
!
      end subroutine set_internal_ele_by_nod
!
!  ---------------------------------------------------------------------
!
      end module set_internal_list_4_linear
