!>@file   m_elapsed_labels_4_REPART.f90
!!@brief  module m_elapsed_labels_4_REPART
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Initialize elepsed time monitoring
!!
!!@verbatim
!!      subroutine elpsed_label_4_repartition
!!@endverbatim
!
      module m_elapsed_labels_4_REPART
!
      use m_precision
      use m_work_time
!
      implicit none
!
      logical, save :: iflag_RPRT_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_RPRT =   0
      integer(kind = kint), save :: ied_elapsed_RPRT =   0
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_repartition
!
      integer(kind = kint), parameter :: num_append = 7
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_RPRT, ied_elapsed_RPRT)
!
      elps1%labels(ist_elapsed_RPRT+1)                                  &
     &                    = 'Repartitioning total    '
      elps1%labels(ist_elapsed_RPRT+2)                                  &
     &                    = 'Repartitioning          '
      elps1%labels(ist_elapsed_RPRT+3)                                  &
     &                    = 'Sleeve extension   '
      elps1%labels(ist_elapsed_RPRT+4)                                  &
     &                    = 'Transfer to repartitioned mesh    '
      elps1%labels(ist_elapsed_RPRT+5)                                  &
     &                    = 'Repartitioning preparation    '
      elps1%labels(ist_elapsed_RPRT+6)                                  &
     &                    = 'Repartition file IO    '
!
      elps1%labels(ist_elapsed_RPRT+7)                                  &
     &                    = 'ele. comm. table for extension    '
      iflag_RPRT_time = .TRUE.
!
      end subroutine elpsed_label_4_repartition
!
!-----------------------------------------------------------------------
!
      end module  m_elapsed_labels_4_REPART
