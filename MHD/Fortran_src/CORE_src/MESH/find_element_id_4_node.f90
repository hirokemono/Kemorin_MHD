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
!
!
!$omp parallel workshare
      nele_4_node(1:numnod) = 0
!$omp end parallel workshare
!
      do iele = iele_st, iele_ed
        do k = 1, nnod_4_ele
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
      integer (kind = kint) :: inod, iele, icou, k
      integer (kind = kint) :: ist, num
!
!
!$omp parallel workshare
      nele_4_node(1:numnod) = 0
!$omp end parallel workshare
!
      do iele = iele_st, iele_ed
        do k = 1, nnod_4_ele
          inod = ie(iele,k)
          nele_4_node(inod) = nele_4_node(inod) + 1
          icou = iele_stack_4_node(inod-1) + nele_4_node(inod)
          iele_4_node(icou) = iele
          iconn_4_node(icou) =  k
        end do
      end do
!
!$omp parallel do private(inod,ist,num)
      do inod = 1, numnod
        ist = iele_stack_4_node(inod-1) + 1
        num = nele_4_node(inod)
        call quicksort_w_index                                          &
     &     (num, iele_4_node(ist), ione, num, iconn_4_node(ist))
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
      end module find_element_id_4_node
