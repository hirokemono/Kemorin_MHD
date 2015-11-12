!
!     module count_bc_element
!
!      Written by H. Matsui and H.Okuda  on July 2001
!      Modified by H. Matsui on June, 2005
!      Modified by H. Matsui on Nov., 2006
!      Modified by H. Matsui on Jan., 2009
!
!      subroutine count_bc_element_4_vect(node, ele, num_index_ibc, ibc)
!      subroutine count_bc_element_4_vect_fl                            &
!     &         (node, ele, num_index_ibc, ibc)
!      subroutine count_bc_element_4_vect_layer                         &
!     &         (node, ele, iele_st, iele_ed, num_index_ibc, ibc)
!
!      subroutine count_bc_element_whole(node, ele, num_index_ibc, ibc)
!      subroutine count_bc_element_fl(node, ele, num_index_ibc, ibc)
!      subroutine count_bc_element_cd(node, ele, num_index_ibc, ibc)
!      subroutine count_bc_element_ins(node, ele, num_index_ibc, ibc)
!!        type(node_data),    intent(in) :: node
!!        type(element_data), intent(in) :: ele
!
!      subroutine count_bc_ele(numnod, numele, nnod_4_ele, ie,          &
!     &           iele_st, iele_ed, num_index_ibc, ibc)
!
!
      module count_bc_element
!
      use m_precision
      use m_constants
      use t_geometry_data
!
      implicit none
!
      private :: count_bc_ele
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_4_vect(node, ele, num_index_ibc, ibc)
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      integer (kind= kint), intent(inout) :: num_index_ibc(3)
      integer (kind= kint), intent(in)    :: ibc(node%numnod,3)
!
!
      call count_bc_element_4_vect_layer                                &
     &   (node, ele, ione, ele%numele, num_index_ibc, ibc)
!
      end subroutine count_bc_element_4_vect
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_4_vect_fl                             &
     &         (node, ele, num_index_ibc, ibc)
!
      use m_geometry_data_MHD
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      integer (kind= kint), intent(inout) :: num_index_ibc(3)
      integer (kind= kint), intent(in)    :: ibc(node%numnod,3)
!
!
      call count_bc_element_4_vect_layer                                &
     &   (node, ele, iele_fl_start, iele_fl_end, num_index_ibc, ibc)
!
      end subroutine count_bc_element_4_vect_fl
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_4_vect_layer                          &
     &         (node, ele, iele_st, iele_ed, num_index_ibc, ibc)
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      integer (kind= kint), intent(in) :: iele_st, iele_ed
      integer (kind= kint), intent(inout) :: num_index_ibc(3)
      integer (kind= kint), intent(in)    :: ibc(node%numnod,3)
!
      integer (kind= kint) :: nd
!
      do nd = 1, 3
        call count_bc_element_layer                                     &
    &      (node, ele, iele_st, iele_ed, num_index_ibc(nd), ibc(1,nd))
      end do
!
      end subroutine count_bc_element_4_vect_layer
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_whole(node, ele, num_index_ibc, ibc)
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
      subroutine count_bc_element_fl(node, ele, num_index_ibc, ibc)
!
      use m_geometry_data_MHD
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      integer (kind= kint), intent(inout) :: num_index_ibc
      integer (kind= kint), intent(in)    :: ibc(node%numnod)
!
      call count_bc_element_layer                                       &
     &   (node, ele, iele_fl_start, iele_fl_end, num_index_ibc, ibc)
!
      end subroutine count_bc_element_fl
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_cd(node, ele, num_index_ibc, ibc)
!
      use m_geometry_data_MHD
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      integer (kind= kint), intent(inout) :: num_index_ibc
      integer (kind= kint), intent(in)    :: ibc(node%numnod)
!
      call count_bc_element_layer                                       &
     &   (node, ele, iele_cd_start, iele_cd_end, num_index_ibc, ibc)
!
      end subroutine count_bc_element_cd
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_ins(node, ele, num_index_ibc, ibc)
!
      use m_geometry_data_MHD
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      integer (kind= kint), intent(inout) :: num_index_ibc
      integer (kind= kint), intent(in)    :: ibc(node%numnod)
!
      call count_bc_element_layer                                       &
     &   (node, ele, iele_ins_start, iele_ins_end, num_index_ibc, ibc)
!
      end subroutine count_bc_element_ins
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_element_layer(node, ele, iele_st, iele_ed,    &
     &          num_index_ibc, ibc)
!
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
