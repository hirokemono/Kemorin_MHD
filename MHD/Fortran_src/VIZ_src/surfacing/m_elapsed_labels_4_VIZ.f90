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
!!      subroutine reset_elapse_after_init_VIZ
!!@endverbatim
!
      module m_elapsed_labels_4_VIZ
!
      use m_precision
      use m_work_time
!
      use elapsed_labels_4_FLINE
!
      implicit none
!
      logical, save :: iflag_VIZ_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_VIZ =   0
      integer(kind = kint), save :: ied_elapsed_VIZ =   0
!
      type(elapsed_lables), save :: elps_PSF1
      type(elapsed_lables), save :: elps_ISO1
!
      type(elapsed_lables), save :: elps_PVR1
      type(elapsed_lables), save :: elps_LIC1
      type(elapsed_lables), save :: elps_map1
!
      type(elapsed_lables), save :: elps_fline1
      type(elapsed_lables), save :: elps_tracer1
!
      private :: elpsed_label_4_VIZ_outline
      private :: reset_elapse_after_init_VIZ_top
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_VIZ
!
      use elapsed_labels_4_PVR
      use elapsed_labels_4_FLINE
      use elapsed_labels_4_PSF
!
!
      call elpsed_label_4_VIZ_outline
      call elpsed_label_4_PVR(elps_PVR1, elps1)
      call elpsed_label_4_LIC(elps_LIC1, elps1)
!
      call elpsed_label_4_PSF(elps_PSF1, elps1)
      call elpsed_label_4_ISO(elps_ISO1, elps1)
      call elpsed_label_4_MAP(elps_map1, elps1)
!
      call elpsed_label_4_FLINE(elps_fline1, elps1)
      call elpsed_label_4_TRACER(elps_tracer1, elps1)
!
      end subroutine elpsed_label_4_VIZ
!
! ----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_VIZ
!
      use elapsed_labels_4_PVR
      use elapsed_labels_4_PSF
!
!
      call reset_elapse_after_init_VIZ_top
!      call reset_elapse_after_init_PVR(elps_PVR1, elps1)
      call reset_elapse_after_init_LIC(elps_LIC1, elps1)
!
      call reset_elapse_after_init_PSF(elps_PSF1, elps1)
      call reset_elapse_after_init_ISO(elps_ISO1, elps1)
      call reset_elapse_after_init_MAP(elps_map1, elps1)
!
      end subroutine reset_elapse_after_init_VIZ
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_VIZ_outline
!
      integer(kind = kint), parameter :: num_append = 17
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_VIZ, ied_elapsed_VIZ)
!
      elps1%labels(ist_elapsed_VIZ+ 1)                                  &
     &                    = 'Sectioning initialization.    '
      elps1%labels(ist_elapsed_VIZ+ 2) = 'Sectioning.    '
!
      elps1%labels(ist_elapsed_VIZ+ 3)                                  &
     &                    = 'Isosurfaceing initialization.    '
      elps1%labels(ist_elapsed_VIZ+ 4) = 'Isosurfaceing.    '
!
      elps1%labels(ist_elapsed_VIZ+ 5)                                  &
     &                    = 'Map projection initialization.    '
      elps1%labels(ist_elapsed_VIZ+ 6) = 'Map projection.    '
!
      elps1%labels(ist_elapsed_VIZ+ 7)                                  &
     &                    = 'Volume rendering initialization.    '
      elps1%labels(ist_elapsed_VIZ+ 8) = 'Volume rendering.    '
!
      elps1%labels(ist_elapsed_VIZ+ 9)                                  &
     &                    = 'LIC rendering initialization.    '
      elps1%labels(ist_elapsed_VIZ+10) = 'LIC rendering.    '
!
      elps1%labels(ist_elapsed_VIZ+11)                                  &
     &                    = 'Fieldline initialization.    '
      elps1%labels(ist_elapsed_VIZ+12) = 'Fieldline.    '
!
      elps1%labels(ist_elapsed_VIZ+13)                                  &
     &                    = 'Tracer initialization.    '
      elps1%labels(ist_elapsed_VIZ+14) = 'Tracer.    '
!
      elps1%labels(ist_elapsed_VIZ+15) = 'VTK output in viz module'
      elps1%labels(ist_elapsed_VIZ+16)                                  &
     &                    = 'ele. comm. table for LIC    '
      elps1%labels(ist_elapsed_VIZ+17)                                  &
     &                    = 'edge comm. table for vizualization    '
!
      iflag_VIZ_time = .TRUE.
!
      end subroutine elpsed_label_4_VIZ_outline
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_VIZ_top
!
      integer(kind = kint) :: i, i_viz
!
      if(iflag_VIZ_time .eqv. .FALSE.) return
      
      do i = 1, 8
        i_viz = 2*i + ist_elapsed_VIZ
        call reset_elapsed_times(i_viz, i_viz)
      end do
!
      end subroutine reset_elapse_after_init_VIZ_top
!
!-----------------------------------------------------------------------
!
      end module  m_elapsed_labels_4_VIZ
