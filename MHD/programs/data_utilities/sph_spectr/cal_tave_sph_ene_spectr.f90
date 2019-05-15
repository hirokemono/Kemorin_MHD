!>@file   cal_tave_sph_ene_spectr.f90
!!        module cal_tave_sph_ene_spectr
!!
!! @author H. Matsui
!! @date   Programmed in  May, 2008
!!
!
!> @brief Evaluate time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine sum_average_ene_spectr                               &
!!     &         (time_sph, pre_time, nri_sph, ltr_sph, ncomp, spec_l,  &
!!     &          ave_spec_l, rms_spec_l, pre_l)
!!
!!      subroutine init_ene_spectr_2_pre                                &
!!     &         (nri_sph, ltr_sph, ncomp, ave_spec_l, rms_spec_l)
!!      subroutine copy_ene_spectr_2_pre(time_sph, pre_time,            &
!!     &          nri_sph, ltr_sph, ncomp, spec_l, pre_l)
!!
!!      subroutine divide_average_ene_spectr                            &
!!     &         (time_sph, time_ini, nri_sph, ltr_sph,                 &
!!     &          ncomp, ave_spec_l, rms_spec_l, sdev_spec_l)
!!      subroutine copy_average_ene_spectr                              &
!!     &         (nri_sph, ltr_sph, ncomp, ave_spec_l, spectr_IO)
!!@endverbatim
!!
!!@n @param istep  time step number
!!@n @param icou   counter for snapshots
!!@n @param ierr   error flag
!
      module cal_tave_sph_ene_spectr
!
      use m_precision
      use m_constants
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sum_average_ene_spectr                                 &
     &         (time_sph, pre_time, nri_sph, ltr_sph, ncomp, spec_l,    &
     &          ave_spec_l, rms_spec_l, pre_l)
!
      real(kind = kreal), intent(in) :: time_sph
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spec_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout) :: pre_time
      real(kind = kreal), intent(inout)                                 &
     &                   :: ave_spec_l(ncomp, 0:ltr_sph, nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: rms_spec_l(ncomp, 0:ltr_sph, nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: pre_l(ncomp, 0:ltr_sph, nri_sph)
!
      integer(kind = kint) :: kr, nd, lth
!
!
!$omp parallel private(kr,lth,nd)
      do kr = 1, nri_sph
        do lth = 0, ltr_sph
!$omp do
          do nd = 1, ncomp
            ave_spec_l(nd,lth,kr) =  ave_spec_l(nd,lth,kr)              &
     &            + half * (spec_l(nd,lth,kr) + pre_l(nd,lth,kr))       &
     &                   * (time_sph - pre_time)
            rms_spec_l(nd,lth,kr) =  rms_spec_l(nd,lth,kr)              &
     &            + half * (spec_l(nd,lth,kr)**2 + pre_l(nd,lth,kr)**2) &
     &                   * (time_sph - pre_time)
            pre_l(nd,lth,kr) =  spec_l(nd,lth,kr)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      pre_time = time_sph
!
      end subroutine sum_average_ene_spectr
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine init_ene_spectr_2_pre                                  &
     &         (nri_sph, ltr_sph, ncomp, ave_spec_l, rms_spec_l)
!
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph, ncomp
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ave_spec_l(ncomp, 0:ltr_sph, nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: rms_spec_l(ncomp, 0:ltr_sph, nri_sph)
!
!
!$omp parallel workshare
            ave_spec_l(1:ncomp,0:ltr_sph,1:nri_sph) =  0.0d0
            rms_spec_l(1:ncomp,0:ltr_sph,1:nri_sph) =  0.0d0
!$omp end parallel workshare
!
      end subroutine init_ene_spectr_2_pre
!
!   --------------------------------------------------------------------
!
      subroutine copy_ene_spectr_2_pre(time_sph, pre_time,              &
     &          nri_sph, ltr_sph, ncomp, spec_l, pre_l)
!
      real(kind = kreal), intent(in) :: time_sph
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spec_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout) :: pre_time
      real(kind = kreal), intent(inout)                                 &
     &                   :: pre_l(ncomp, 0:ltr_sph, nri_sph)
!
!
!$omp parallel workshare
       pre_l(1:ncomp,0:ltr_sph,1:nri_sph)                               &
     &           =  spec_l(1:ncomp,0:ltr_sph,1:nri_sph)
!$omp end parallel workshare
!
      pre_time = time_sph
!
      end subroutine copy_ene_spectr_2_pre
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine divide_average_ene_spectr                              &
     &         (time_sph, time_ini, nri_sph, ltr_sph,                   &
     &          ncomp, ave_spec_l, rms_spec_l, sdev_spec_l)
!
      real(kind = kreal), intent(in) :: time_sph, time_ini
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout)                                 &
     &                   :: ave_spec_l(ncomp, 0:ltr_sph, nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: rms_spec_l(ncomp, 0:ltr_sph, nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: sdev_spec_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal) :: acou
!
!
!$omp parallel workshare
      sdev_spec_l(1:ncomp,0:ltr_sph,1:nri_sph)                          &
     &         = rms_spec_l(1:ncomp,0:ltr_sph,1:nri_sph)                &
     &          - ave_spec_l(1:ncomp,0:ltr_sph,1:nri_sph)**2
!$omp end parallel workshare
!
      acou = one / (time_sph - time_ini)
!$omp parallel workshare
      ave_spec_l(1:ncomp,0:ltr_sph,1:nri_sph)                           &
     &         = ave_spec_l(1:ncomp,0:ltr_sph,1:nri_sph) * acou
      rms_spec_l(1:ncomp,0:ltr_sph,1:nri_sph)                           &
     &         = sqrt(rms_spec_l(1:ncomp,0:ltr_sph,1:nri_sph) * acou)
      sdev_spec_l(1:ncomp,0:ltr_sph,1:nri_sph)                          &
     &         = sqrt(sdev_spec_l(1:ncomp,0:ltr_sph,1:nri_sph) * acou)
!$omp end parallel workshare
!
      end subroutine divide_average_ene_spectr
!
!   --------------------------------------------------------------------
!
      subroutine copy_average_ene_spectr                                &
     &         (nri_sph, ltr_sph, ncomp, ave_spec_l, spectr_IO)
!
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: ave_spec_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_IO(ncomp, 0:ltr_sph, nri_sph)
!
!
!$omp parallel workshare
      spectr_IO(1:ncomp,0:ltr_sph,1:nri_sph)                            &
     &         = ave_spec_l(1:ncomp,0:ltr_sph,1:nri_sph)
!$omp end parallel workshare
!
      end subroutine copy_average_ene_spectr
!
!   --------------------------------------------------------------------
!
      end module cal_tave_sph_ene_spectr
