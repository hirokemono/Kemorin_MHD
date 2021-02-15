!>@file   find_element_id_4_node.f90
!!@brief  module find_element_id_4_node
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!> @brief Search belonged element list for each node
!!
!!@verbatim
!!      subroutine count_iele_4_node(numnod, numele, nnod_4_ele, ie,    &
!!     &          iele_st, iele_ed, nele_4_node)
!!      subroutine set_iele_4_node(numnod, numele, nnod_4_ele, ie,      &
!!     &          iele_st, iele_ed, ntot_ele_4_node, iele_stack_4_node, &
!!     &          nele_4_node, iele_4_node, iconn_4_node)
!!
!!      subroutine count_belonged_ele_4_node(numnod, numele, nnod_4_ele,&
!!     &          ie, iele_st, iele_ed, nele_4_node)
!!      subroutine set_belonged_ele_4_node(numnod, numele, nnod_4_ele,  &
!!     &          ie, iele_st, iele_ed, ntot_ele_4_node,                &
!!     &          iele_stack_4_node, nele_4_node, iele_4_node,          &
!!     &          iconn_4_node)
!!@endverbatim
!
      module find_element_id_4_node
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: find_degraded_node
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine count_iele_4_node(numnod, numele, nnod_4_ele, ie,      &
     &          iele_st, iele_ed, nele_4_node)
!
      integer (kind=kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind=kint), intent(in) :: iele_st, iele_ed
!
      integer (kind=kint), intent(inout) :: nele_4_node(numnod)
!
      integer (kind = kint) :: inod, iele, k
      integer(kind = kint) :: ie_degrade(nnod_4_ele)
!
!
!$omp parallel workshare
      nele_4_node(1:numnod) = 0
!$omp end parallel workshare
!
      do iele = iele_st, iele_ed
        call find_degraded_node(iele, numele, nnod_4_ele, ie,           &
     &                          ie_degrade)
        do k = 1, nnod_4_ele
          if(ie_degrade(k) .gt. 0) cycle
          inod = ie(iele,k)
          nele_4_node(inod) = nele_4_node(inod) + 1
        end do
      end do
!
      end  subroutine count_iele_4_node
!
! -----------------------------------------------------------------------
!
      subroutine set_iele_4_node(numnod, numele, nnod_4_ele, ie,        &
     &          iele_st, iele_ed, ntot_ele_4_node, iele_stack_4_node,   &
     &          nele_4_node, iele_4_node, iconn_4_node)
!
      use quicksort
!
      integer (kind=kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind=kint), intent(in) :: iele_st, iele_ed
      integer (kind=kint), intent(in) :: ntot_ele_4_node
      integer (kind=kint), intent(in) :: iele_stack_4_node(0:numnod)
!
      integer (kind=kint), intent(inout) :: nele_4_node(numnod)
      integer (kind=kint), intent(inout)                                &
     &                    :: iele_4_node(ntot_ele_4_node)
      integer (kind=kint), intent(inout)                                &
     &                    :: iconn_4_node(ntot_ele_4_node)
!
      integer (kind = kint) :: inod, iele, icou, k, ist
      integer(kind = kint) :: ie_degrade(nnod_4_ele)
!
!
!$omp parallel workshare
      nele_4_node(1:numnod) = 0
!$omp end parallel workshare
!
      do iele = iele_st, iele_ed
        call find_degraded_node(iele, numele, nnod_4_ele, ie,           &
     &                          ie_degrade)
        do k = 1, nnod_4_ele
          if(ie_degrade(k) .gt. 0) cycle
!
          inod = ie(iele,k)
          nele_4_node(inod) = nele_4_node(inod) + 1
          icou = iele_stack_4_node(inod-1) + nele_4_node(inod)
          iele_4_node(icou) = iele
          iconn_4_node(icou) =  k
        end do
      end do
!
!$omp parallel do private(inod,ist)
      do inod = 1, numnod
        ist = iele_stack_4_node(inod-1) + 1
        if(nele_4_node(inod) .gt. 1) then
          call quicksort_w_index(nele_4_node(inod), iele_4_node(ist),   &
     &        ione, nele_4_node(inod), iconn_4_node(ist))
        end if
      end do
!$omp end parallel do
!
      end  subroutine set_iele_4_node
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_belonged_ele_4_node(numnod, numele, nnod_4_ele,  &
     &          ie, iele_st, iele_ed, nele_4_node)
!
      integer (kind=kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind=kint), intent(in) :: iele_st, iele_ed
!
      integer (kind=kint), intent(inout) :: nele_4_node(numnod)
!
!
      call count_iele_4_node(numnod, numele, ione, ie(1,1),             &
     &   iele_st, iele_ed, nele_4_node)
!
      end  subroutine count_belonged_ele_4_node
!
! -----------------------------------------------------------------------
!
      subroutine set_belonged_ele_4_node(numnod, numele, nnod_4_ele,    &
     &          ie, iele_st, iele_ed, ntot_ele_4_node,                  &
     &          iele_stack_4_node, nele_4_node, iele_4_node,            &
     &          iconn_4_node)
!
      integer (kind=kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind=kint), intent(in) :: iele_st, iele_ed
      integer (kind=kint), intent(in) :: ntot_ele_4_node
      integer (kind=kint), intent(in) :: iele_stack_4_node(0:numnod)
!
      integer (kind=kint), intent(inout) :: nele_4_node(numnod)
      integer (kind=kint), intent(inout)                                &
     &                    :: iele_4_node(ntot_ele_4_node)
      integer (kind=kint), intent(inout)                                &
     &                    :: iconn_4_node(ntot_ele_4_node)
!
!
      call set_iele_4_node(numnod, numele, ione, ie(1,1),               &
     &          iele_st, iele_ed, ntot_ele_4_node, iele_stack_4_node,   &
     &          nele_4_node, iele_4_node, iconn_4_node)
!
      end  subroutine set_belonged_ele_4_node
!
! -----------------------------------------------------------------------
!
      subroutine find_degraded_node(iele, numele, nnod_4_ele, ie,       &
     &                              ie_degrade)
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(inout) :: ie_degrade(nnod_4_ele)
!
      ie_degrade(1:nnod_4_ele) = 0
      if(ie(iele,1) .eq. ie(iele,2)) ie_degrade(2) = 1
      if(ie(iele,2) .eq. ie(iele,3)) ie_degrade(3) = 2
      if(ie(iele,3) .eq. ie(iele,4)) ie_degrade(4) = 3
      if(ie(iele,4) .eq. ie(iele,1)) ie_degrade(4) = 1
      if(ie(iele,5) .eq. ie(iele,6)) ie_degrade(6) = 5
      if(ie(iele,6) .eq. ie(iele,7)) ie_degrade(7) = 6
      if(ie(iele,7) .eq. ie(iele,8)) ie_degrade(8) = 7
      if(ie(iele,8) .eq. ie(iele,5)) ie_degrade(8) = 5
      if(ie(iele,1) .eq. ie(iele,5)) ie_degrade(5) = 1
      if(ie(iele,2) .eq. ie(iele,6)) ie_degrade(6) = 2
      if(ie(iele,3) .eq. ie(iele,7)) ie_degrade(7) = 3
      if(ie(iele,4) .eq. ie(iele,8)) ie_degrade(8) = 4
!
      end subroutine find_degraded_node
!
! -----------------------------------------------------------------------
!
      end module find_element_id_4_node
