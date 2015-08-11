!
!     module set_bc_element
!
!      Written by H. Matsui and H.Okuda  on July 2001
!      Modified by H. Matsui on June, 2005
!
!      subroutine set_bc_ele(numnod, num_idx_ibc, iele_st, iele_ed,     &
!     &          ibc, ele_bc_id, nod_bc_id, num_t)
!
!      subroutine set_bc_element_whole(num_idx_ibc, ibc, ele_bc_id,     &
!     &       nod_bc_id, num_t)
!      subroutine set_bc_element_cd(num_idx_ibc, ibc, ele_bc_id,        &
!     &      nod_bc_id, num_t)
!      subroutine set_bc_element_ins(num_idx_ibc, ibc,                  &
!     &      ele_bc_id, nod_bc_id, num_t)
!
      module set_bc_element
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
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
!-----------------------------------------------------------------------
!
      subroutine set_bc_element_whole(num_idx_ibc, ibc, ele_bc_id,      &
     &       nod_bc_id, num_t)
!
      use m_geometry_data
!
      integer (kind= kint), intent(in)    :: num_idx_ibc
      integer (kind= kint), intent(in)    :: num_t
      integer (kind= kint), intent(in)    :: ibc(node1%numnod)
      integer (kind= kint), intent(inout) :: ele_bc_id(num_idx_ibc)
      integer (kind= kint), intent(inout) :: nod_bc_id(num_idx_ibc)
!
      integer (kind= kint), parameter :: ione = 1
!
!
      if ( num_idx_ibc.gt. 0 ) then
        call set_bc_ele(node1%numnod, numele, nnod_4_ele, ie,           &
     &      num_idx_ibc, ione, numele, ibc,                             &
     &      ele_bc_id, nod_bc_id, num_t)
      end if
!
      end subroutine set_bc_element_whole
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_element_fl(num_idx_ibc, ibc, ele_bc_id,         &
     &      nod_bc_id, num_t)
!
      use m_geometry_data
      use m_geometry_data_MHD
!
      integer (kind= kint), intent(in)    :: num_idx_ibc
      integer (kind= kint), intent(in)    :: num_t
      integer (kind= kint), intent(in)    :: ibc(node1%numnod)
      integer (kind= kint), intent(inout) :: ele_bc_id(num_idx_ibc)
      integer (kind= kint), intent(inout) :: nod_bc_id(num_idx_ibc)
!
!
      if ( num_idx_ibc .gt. 0 ) then
        call set_bc_ele(node1%numnod, numele, nnod_4_ele, ie,           &
     &      num_idx_ibc, iele_fl_start, iele_fl_end, ibc,               &
     &      ele_bc_id, nod_bc_id, num_t)
      end if
!
      end subroutine set_bc_element_fl
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_element_cd(num_idx_ibc, ibc, ele_bc_id,         &
     &      nod_bc_id, num_t)
!
      use m_geometry_data
      use m_geometry_data_MHD
!
      integer (kind= kint), intent(in)    :: num_idx_ibc
      integer (kind= kint), intent(in)    :: num_t
      integer (kind= kint), intent(in)    :: ibc(node1%numnod)
      integer (kind= kint), intent(inout) :: ele_bc_id(num_idx_ibc)
      integer (kind= kint), intent(inout) :: nod_bc_id(num_idx_ibc)
!
      if ( num_idx_ibc.gt. 0 ) then
        call set_bc_ele(node1%numnod, numele, nnod_4_ele, ie,           &
     &      num_idx_ibc, iele_cd_start, iele_cd_end, ibc,               &
     &      ele_bc_id, nod_bc_id, num_t)
      end if
!
      end subroutine set_bc_element_cd
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_element_ins(num_idx_ibc, ibc,                   &
     &      ele_bc_id, nod_bc_id, num_t)
!
      use m_geometry_data
      use m_geometry_data_MHD
!
      integer (kind= kint), intent(in)    :: num_idx_ibc
      integer (kind= kint), intent(in)    :: num_t
      integer (kind= kint), intent(in)    :: ibc(node1%numnod)
      integer (kind= kint), intent(inout) :: ele_bc_id(num_idx_ibc)
      integer (kind= kint), intent(inout) :: nod_bc_id(num_idx_ibc)
!
!
      if ( num_idx_ibc.gt. 0 ) then
        call set_bc_ele(node1%numnod, numele, nnod_4_ele, ie,           &
     &      num_idx_ibc, iele_ins_start, iele_ins_end, ibc,             &
     &      ele_bc_id, nod_bc_id, num_t)
      end if
!
      end subroutine set_bc_element_ins
!
!-----------------------------------------------------------------------
!
      end module set_bc_element
