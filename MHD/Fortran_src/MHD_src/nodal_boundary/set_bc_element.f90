!
!     module set_bc_element
!
!      Written by H. Matsui and H.Okuda  on July 2001
!      Modified by H. Matsui on June, 2005
!
!!      subroutine count_bc_element_whole(node, ele, num_index_ibc, ibc)
!!      subroutine count_bc_element_layer(node, ele, iele_st, iele_ed,  &
!!     &          num_index_ibc, ibc)
!!
!!      subroutine set_bc_element_layer(node, ele, iele_st, iele_ed,    &
!!     &           num_idx_ibc, ibc, ele_bc_id, nod_bc_id, num_t)
!!      subroutine set_bc_element_whole(node, ele,                      &
!!     &          num_idx_ibc, ibc, ele_bc_id, nod_bc_id, num_t)
!!        type(node_data), intent(in) :: node
!!        type(node_data), intent(in) :: ele
!
      module set_bc_element
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: count_bc_ele, set_bc_ele
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_whole(node, ele, num_index_ibc, ibc)
!
      use t_geometry_data
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      integer (kind= kint), intent(inout) :: num_index_ibc
      integer (kind= kint), intent(in)    :: ibc(node%numnod)
!
!   conunt node in elements for boundary
      call count_bc_element_layer                                       &
     &   (node, ele, ione, ele%numele, num_index_ibc, ibc)
!
      end subroutine count_bc_element_whole
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_layer(node, ele, iele_st, iele_ed,    &
     &          num_index_ibc, ibc)
!
      use t_geometry_data
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      integer (kind= kint), intent(in) :: iele_st, iele_ed
      integer (kind= kint), intent(in) :: ibc(node%numnod)
!
      integer (kind= kint), intent(inout) :: num_index_ibc
!
      call count_bc_ele                                                 &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    iele_st, iele_ed, num_index_ibc, ibc)
!
      end subroutine count_bc_element_layer
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_bc_element_layer(node, ele, iele_st, iele_ed,      &
     &           num_idx_ibc, ibc, ele_bc_id, nod_bc_id, num_t)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      integer (kind= kint), intent(in)    :: iele_st, iele_ed
      integer (kind= kint), intent(in)    :: num_idx_ibc
      integer (kind= kint), intent(in)    :: num_t
      integer (kind= kint), intent(in)    :: ibc(node%numnod)
      integer (kind= kint), intent(inout) :: ele_bc_id(num_idx_ibc)
      integer (kind= kint), intent(inout) :: nod_bc_id(num_idx_ibc)
!
!
      if(num_idx_ibc .le. 0) return
      call set_bc_ele(node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,  &
     &    num_idx_ibc, iele_st, iele_ed, ibc, ele_bc_id, nod_bc_id,     &
     &    num_t)
!
      end subroutine set_bc_element_layer
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_element_whole(node, ele,                        &
     &          num_idx_ibc, ibc, ele_bc_id, nod_bc_id, num_t)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      integer (kind= kint), intent(in)    :: num_idx_ibc
      integer (kind= kint), intent(in)    :: num_t
      integer (kind= kint), intent(in)    :: ibc(node%numnod)
      integer (kind= kint), intent(inout) :: ele_bc_id(num_idx_ibc)
      integer (kind= kint), intent(inout) :: nod_bc_id(num_idx_ibc)
!
!
      call set_bc_element_layer(node, ele, ione, ele%numele,            &
     &    num_idx_ibc, ibc, ele_bc_id, nod_bc_id, num_t)
!
      end subroutine set_bc_element_whole
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_bc_ele(numnod, numele, nnod_4_ele, ie,           &
     &           iele_st, iele_ed, num_index_ibc, ibc)
!
      integer (kind= kint), intent(in)    :: numnod
      integer (kind= kint), intent(in)    :: numele, nnod_4_ele
      integer (kind= kint), intent(in)    :: ie(numele,nnod_4_ele)
      integer (kind= kint), intent(in)    :: iele_st, iele_ed
      integer (kind= kint), intent(in)    :: ibc(numnod)
      integer (kind= kint), intent(inout) :: num_index_ibc
!
      integer (kind= kint) :: iele, kk
!
!   conunt node in elements for boundary
!
      num_index_ibc = 0
      do iele = iele_st, iele_ed
        do kk = 1, nnod_4_ele
          if ( ibc(ie(iele,kk)) == 1 ) then
            num_index_ibc = num_index_ibc + 1
          end if
        end do
      end do
!
      end subroutine count_bc_ele
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_ele(numnod, numele, nnod_4_ele, ie,             &
     &          num_idx_ibc, iele_st, iele_ed, ibc,                     &
     &          ele_bc_id, nod_bc_id, num_t)
!
      integer (kind= kint), intent(in)    :: numnod
      integer (kind= kint), intent(in)    :: numele, nnod_4_ele
      integer (kind= kint), intent(in)    :: ie(numele,nnod_4_ele)
      integer (kind= kint), intent(in)    :: num_idx_ibc
      integer (kind= kint), intent(in)    :: iele_st, iele_ed
      integer (kind= kint), intent(in)    :: num_t
      integer (kind= kint), intent(in)    :: ibc(numnod)
      integer (kind= kint), intent(inout) :: ele_bc_id(num_idx_ibc)
      integer (kind= kint), intent(inout) :: nod_bc_id(num_idx_ibc)
!
      integer (kind= kint) :: iele, inum, kk
!
!
!   set node id in elements for the temperature boundary 
!
      inum = 0
      do iele = iele_st, iele_ed
        do kk = 1, num_t
          if ( ibc(ie(iele,kk)) == 1 ) then
            inum = inum + 1
            ele_bc_id(inum) = iele
            nod_bc_id(inum) = kk
          end if
!
        end do
      end do
!
      end subroutine set_bc_ele
!
!-----------------------------------------------------------------------
!
      end module set_bc_element
