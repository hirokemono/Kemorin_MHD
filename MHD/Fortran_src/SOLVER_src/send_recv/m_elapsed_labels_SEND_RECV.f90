!>@file   m_elapsed_labels_SEND_RECV.f90
!!@brief  module m_elapsed_labels_SEND_RECV
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2018
!
!>@brief  Labels for elapsed time monitor
!!
!!@verbatim
!!      subroutine elpsed_label_4_send_recv
!!
!!      subroutine reset_elapse_after_init_SR
!!@endverbatim
!!
      module m_elapsed_labels_SEND_RECV
!
      use m_precision
      use m_work_time
!
      implicit none
!
!
      logical, save :: iflag_SR_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_SR =   0
      integer(kind = kint), save :: ied_elapsed_SR =   0
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_send_recv
!
      integer(kind = kint), parameter :: num_append = 3
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_SR, ied_elapsed_SR)
!
      elps1%labels(ist_elapsed_SR+1) = 'set_to_send_buf_N    '
      elps1%labels(ist_elapsed_SR+2) = 'calypso_send_recv_core    '
      elps1%labels(ist_elapsed_SR+3) = 'set_from_recv_buf_rev_N    '
!
      iflag_SR_time = .TRUE.
!
      end subroutine elpsed_label_4_send_recv
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_SR
!
!
      if(iflag_SR_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_SR+1, ied_elapsed_SR)
!
      end subroutine reset_elapse_after_init_SR
!
!-----------------------------------------------------------------------
!
      end module m_elapsed_labels_SEND_RECV
