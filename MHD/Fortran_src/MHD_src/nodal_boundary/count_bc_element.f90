!
!     module count_bc_element
!
!      Written by H. Matsui and H.Okuda  on July 2001
!      Modified by H. Matsui on June, 2005
!      Modified by H. Matsui on Nov., 2006
!      Modified by H. Matsui on Jan., 2009
!
!      subroutine count_bc_element_4_vect(num_index_ibc, ibc)
!      subroutine count_bc_element_4_vect_fl(num_index_ibc, ibc)
!      subroutine count_bc_element_4_vect_cd(num_index_ibc, ibc)
!      subroutine count_bc_element_4_vect_ins(num_index_ibc, ibc)
!
!      subroutine count_bc_element_whole(num_index_ibc, ibc)
!      subroutine count_bc_element_fl(num_index_ibc, ibc)
!      subroutine count_bc_element_cd(num_index_ibc, ibc)
!      subroutine count_bc_element_ins(num_index_ibc, ibc)
!
!      subroutine count_bc_ele(numnod, numele, nnod_4_ele, ie,          &
!     &           iele_st, iele_ed, num_index_ibc, ibc)
!
!
      module count_bc_element
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
      subroutine count_bc_element_4_vect(num_index_ibc, ibc)
!
      use m_geometry_data
!
      integer (kind= kint), intent(inout) :: num_index_ibc(3)
      integer (kind= kint), intent(in)    :: ibc(node1%numnod,3)
!
      integer (kind= kint) :: nd
!
      do nd = 1, 3
        call count_bc_element_whole(num_index_ibc(nd), ibc(1,nd) )
      end do
!
      end subroutine count_bc_element_4_vect
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_4_vect_fl(num_index_ibc, ibc)
!
      use m_geometry_data
!
      integer (kind= kint), intent(inout) :: num_index_ibc(3)
      integer (kind= kint), intent(in)    :: ibc(node1%numnod,3)
!
      integer (kind= kint) :: nd
!
      do nd = 1, 3
        call count_bc_element_fl(num_index_ibc(nd), ibc(1,nd) )
      end do
!
      end subroutine count_bc_element_4_vect_fl
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_4_vect_cd(num_index_ibc, ibc)
!
      use m_geometry_data
!
      integer (kind= kint), intent(inout) :: num_index_ibc(3)
      integer (kind= kint), intent(in)    :: ibc(node1%numnod,3)
!
      integer (kind= kint) :: nd
!
      do nd = 1, 3
        call count_bc_element_cd(num_index_ibc(nd), ibc(1,nd) )
      end do
!
      end subroutine count_bc_element_4_vect_cd
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_4_vect_ins(num_index_ibc, ibc)
!
      use m_geometry_data
!
      integer (kind= kint), intent(inout) :: num_index_ibc(3)
      integer (kind= kint), intent(in)    :: ibc(node1%numnod,3)
!
      integer (kind= kint) :: nd
!
      do nd = 1, 3
        call count_bc_element_ins(num_index_ibc(nd), ibc(1,nd) )
      end do
!
      end subroutine count_bc_element_4_vect_ins
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_whole(num_index_ibc, ibc)
!
      use m_geometry_data
!
      integer (kind= kint), intent(inout) :: num_index_ibc
      integer (kind= kint), intent(in)    :: ibc(node1%numnod)
!
      integer (kind= kint), parameter :: ione = 1
!
!   conunt node in elements for boundary
      call count_bc_ele(node1%numnod, ele1%numele, nnod_4_ele, ie,      &
     &    ione, ele1%numele, num_index_ibc, ibc)
!
      end subroutine count_bc_element_whole
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_fl(num_index_ibc, ibc)
!
      use m_geometry_data
      use m_geometry_data_MHD
!
      integer (kind= kint), intent(inout) :: num_index_ibc
      integer (kind= kint), intent(in)    :: ibc(node1%numnod)
!
      call count_bc_ele(node1%numnod, ele1%numele, nnod_4_ele, ie,      &
     &           iele_fl_start, iele_fl_end, num_index_ibc, ibc)
!
      end subroutine count_bc_element_fl
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_cd(num_index_ibc, ibc)
!
      use m_geometry_data
      use m_geometry_data_MHD
!
      integer (kind= kint), intent(inout) :: num_index_ibc
      integer (kind= kint), intent(in)    :: ibc(node1%numnod)
!
      call count_bc_ele(node1%numnod, ele1%numele, nnod_4_ele, ie,      &
     &           iele_cd_start, iele_cd_end, num_index_ibc, ibc)
!
      end subroutine count_bc_element_cd
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_ins(num_index_ibc, ibc)
!
      use m_geometry_data
      use m_geometry_data_MHD
!
      integer (kind= kint), intent(inout) :: num_index_ibc
      integer (kind= kint), intent(in)    :: ibc(node1%numnod)
!
      call count_bc_ele(node1%numnod, ele1%numele, nnod_4_ele, ie,      &
     &           iele_ins_start, iele_ins_end,  num_index_ibc, ibc)
!
      end subroutine count_bc_element_ins
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
      end module count_bc_element
