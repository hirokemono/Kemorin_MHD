!>@file   m_elapsed_labels_4_VIZ.f90
!!@brief  module m_elapsed_labels_4_VIZ
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Initialize elepsed time monitoring
!!
!!@verbatim
!!      subroutine elpsed_label_4_VIZ
!!@endverbatim
!
      module m_elapsed_labels_4_VIZ
!
      use m_precision
      use m_work_time
!
      implicit none
!
      logical, save :: iflag_VIZ_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_VIZ =   0
      integer(kind = kint), save :: ied_elapsed_VIZ =   0
!
      logical, save :: iflag_PVR_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_PVR =   0
      integer(kind = kint), save :: ied_elapsed_PVR =   0
!
      logical, save :: iflag_LIC_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_LIC =   0
      integer(kind = kint), save :: ied_elapsed_LIC =   0
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_VIZ
!
!
      elapse_labels(12) = 'Visualizatio time         '
!
!
      call elpsed_label_4_VIZ_outline
      call elpsed_label_4_PVR
      call elpsed_label_4_LIC
!
      end subroutine elpsed_label_4_VIZ
!
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_VIZ_outline
!
      integer(kind = kint), parameter :: num_append = 10
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_VIZ, ied_elapsed_VIZ)
!
      elapse_labels(ist_elapsed_VIZ+ 1)                                 &
     &                    = 'Sectioning initialization.    '
      elapse_labels(ist_elapsed_VIZ+ 2)                                 &
     &                    = 'Isosurfaceing initialization.    '
      elapse_labels(ist_elapsed_VIZ+ 3)                                 &
     &                    = 'Volume rendering initialization.    '
      elapse_labels(ist_elapsed_VIZ+ 4)                                 &
     &                    = 'fieldline initialization.    '
      elapse_labels(ist_elapsed_VIZ+ 5)                                 &
     &                    = 'LIC rendering initialization.    '
!
      elapse_labels(ist_elapsed_VIZ+ 6) = 'Sectioning.    '
      elapse_labels(ist_elapsed_VIZ+ 7) = 'Isosurfaceing.    '
      elapse_labels(ist_elapsed_VIZ+ 8) = 'Volume rendering.    '
      elapse_labels(ist_elapsed_VIZ+ 9) = 'fieldline.    '
      elapse_labels(ist_elapsed_VIZ+10) = 'LIC rendering.    '
!
      iflag_VIZ_time = .TRUE.
!
      end subroutine elpsed_label_4_VIZ_outline
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_PVR
!
      integer(kind = kint), parameter :: num_append = 4
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_PVR, ied_elapsed_PVR)
!
      elapse_labels(ist_elapsed_PVR+1)                                  &
     &                    = 'Volume rendering w/o file output   '
      elapse_labels(ist_elapsed_PVR+2)                                  &
     &                    = 'Volume rendering file output   '
      elapse_labels(ist_elapsed_PVR+3)                                  &
     &                    = 'V. Rendering ray trace   '
      elapse_labels(ist_elapsed_PVR+4)                                  &
     &                    = 'V. Rendering subimage composit   '
!
      iflag_PVR_time = .TRUE.
!
      end subroutine elpsed_label_4_PVR
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_LIC
!
      integer(kind = kint), parameter :: num_append = 4
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_LIC, ied_elapsed_LIC)
!
      elapse_labels(ist_elapsed_LIC+1)                                  &
     &                    = 'LIC V. rendering w/o file output   '
      elapse_labels(ist_elapsed_LIC+2)                                  &
     &                    = 'LIC V. rendering file output   '
      elapse_labels(ist_elapsed_LIC+3)                                  &
     &                    = 'LIC V. Rendering ray trace   '
      elapse_labels(ist_elapsed_LIC+4)                                  &
     &                    = 'LIC V. Rendering subimage composit   '
!
      iflag_LIC_time = .TRUE.
!
      end subroutine elpsed_label_4_LIC
!
!-----------------------------------------------------------------------
!
      end module  m_elapsed_labels_4_VIZ
