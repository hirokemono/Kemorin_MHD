!>@file   m_elapsed_labels_4_PART.f90
!!@brief  module m_elapsed_labels_4_PART
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2018
!
!>@brief  Labels for elapsed time monitor
!!
!!@verbatim
!!      subroutine elapsed_label_4_PARTITIONER
!!      subroutine reset_elapse_4_PARTITIONER
!!@endverbatim
!!
      module m_elapsed_labels_4_PART
!
      use m_precision
      use m_work_time
!
      implicit none
!
!
      logical, save :: iflag_PART_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_PART =  0
      integer(kind = kint), save :: ied_elapsed_PART =  0
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine elapsed_label_4_PARTITIONER
!
      integer(kind = kint), parameter :: num_append = 4
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_PART, ied_elapsed_PART)
!
      elps1%labels(ist_elapsed_PART+1) = 'increase_overlapping       '
      elps1%labels(ist_elapsed_PART+2) = 'mark_extented_overlap    '
      elps1%labels(ist_elapsed_PART+3)                                  &
     &                   = 'selective_extended_overlap   '
      elps1%labels(ist_elapsed_PART+4)                                  &
     &                   = 'gen_node_import/export_tables    '
!
      iflag_PART_time = .TRUE.
!
      end subroutine elapsed_label_4_PARTITIONER
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_4_PARTITIONER
!
!
      if(iflag_PART_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_PART+1, ied_elapsed_PART)
!
      end subroutine reset_elapse_4_PARTITIONER
!
!-----------------------------------------------------------------------
!
      end module m_elapsed_labels_4_PART
