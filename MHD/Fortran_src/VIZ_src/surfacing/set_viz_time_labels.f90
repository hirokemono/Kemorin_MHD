!>@file   set_viz_time_labels.f90
!!@brief  module set_viz_time_labels
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Initialize elepsed time monitoring
!!
!!@verbatim
!!      subroutine s_set_viz_time_labels
!!@endverbatim
!
      module set_viz_time_labels
!
      use m_precision
      use m_work_time
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_viz_time_labels
!
      use m_work_time
!
!
      elapse_labels(12) = 'Visualizatio time         '
!
!
      elapse_labels(60) = 'Sectioning initialization.    '
      elapse_labels(61) = 'Isosurfaceing initialization.    '
      elapse_labels(62) = 'Volume rendering initialization.    '
      elapse_labels(63) = 'fieldline initialization.    '
      elapse_labels(64) = 'LIC rendering initialization.    '
!
      elapse_labels(65) = 'Sectioning.    '
      elapse_labels(66) = 'Isosurfaceing.    '
      elapse_labels(67) = 'Volume rendering.    '
      elapse_labels(68) = 'fieldline.    '
      elapse_labels(69) = 'LIC rendering.    '
!
      elapse_labels(71) = 'Volume rendering w/o file output   '
      elapse_labels(72) = 'Volume rendering file output   '
      elapse_labels(73) = 'V. Rendering ray trace   '
      elapse_labels(74) = 'V. Rendering subimage composit   '
!
      end subroutine s_set_viz_time_labels
!
! ----------------------------------------------------------------------
!
      end module  set_viz_time_labels
