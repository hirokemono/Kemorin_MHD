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
!!      subroutine sum_average_ene_sph(time_sph, pre_time, nri_sph,     &
!!     &          ncomp, spectr_t, ave_spec_t, spectr_pre_t)
!!      subroutine sum_average_ene_spectr                               &
!!     &         (time_sph, pre_time, nri_sph, ltr_sph, ncomp, spectr_l,&
!!     &          ave_spec_l, spectr_pre_l)
!!
!!      subroutine sum_deviation_ene_sph(time_sph, pre_time,            &
!!     &          nri_sph, ncomp, spectr_t, ave_spec_t,                 &
!!     &          sigma_spec_t, spectr_pre_t)
!!      subroutine sum_deviation_ene_spectr(time_sph, pre_time,         &
!!     &          nri_sph, ltr_sph, ncomp, spectr_l, ave_spec_l,        &
!!     &          sigma_spec_l, spectr_pre_l)
!!
!!      subroutine count_degree_on_layer_data
!!      subroutine count_degree_one_layer_data
!!      subroutine count_degree_on_volume_data
!!
!!      subroutine divide_average_ene_sph                               &
!!     &         (time_sph, time_ini, nri_sph, ncomp, ave_spec)
!!      subroutine divide_average_ene_spectr(time_sph, time_ini,        &
!!     &          nri_sph, ltr_sph, ncomp, ave_spec_l)
!!
!!      subroutine divide_deviation_ene_sph                             &
!!     &         (time_sph, time_ini, nri_sph, ncomp, sigma_spec_t)
!!      subroutine divide_deviation_ene_spectr(time_sph, time_ini,      &
!!     &          nri_sph, ltr_sph, ncomp, sigma_spec_l)
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
      use m_sph_ene_spectra
      use m_tave_sph_ene_spectr
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sum_average_ene_sph(time_sph, pre_time, nri_sph,       &
     &          ncomp, spectr_t, ave_spec_t, spectr_pre_t)
!
      real(kind = kreal), intent(in) :: time_sph, pre_time
      integer(kind = kint), intent(in) :: nri_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_t(ncomp, nri_sph)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ave_spec_t(ncomp, nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_pre_t(ncomp, nri_sph)
!
      integer(kind = kint) :: kr, nd
      real(kind= kreal) :: tmp, tmp_l, tmp_m, tmp_lm
!
!
!$omp parallel private(kr,nd,tmp,tmp_l,tmp_m,tmp_lm)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp_sph_spec
          tmp = spectr_t(nd,kr)
          ave_spec_t(nd,kr) = ave_spec_t(nd,kr)                         &
     &           + half * (tmp + spectr_pre_t(nd,kr))                   &
     &            * (time_sph - pre_time)
          spectr_pre_t(nd,kr) = tmp
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine sum_average_ene_sph
!
!   --------------------------------------------------------------------
!
      subroutine sum_average_ene_spectr                                 &
     &         (time_sph, pre_time, nri_sph, ltr_sph, ncomp, spectr_l,  &
     &          ave_spec_l, spectr_pre_l)
!
      real(kind = kreal), intent(in) :: time_sph, pre_time
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ave_spec_l(ncomp, 0:ltr_sph, nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_pre_l(ncomp, 0:ltr_sph, nri_sph)
!
      integer(kind = kint) :: kr, nd, lth
      real(kind= kreal) :: tmp, tmp_l, tmp_m, tmp_lm
!
!
!$omp parallel private(kr,lth,nd,tmp,tmp_l,tmp_m,tmp_lm)
      do kr = 1, nri_sph
        do lth = 0, ltr_sph
!$omp do
          do nd = 1, ncomp_sph_spec
            tmp_l =  spectr_l(nd,lth,kr)
            tmp_m =  spectr_m(nd,lth,kr)
            tmp_lm = spectr_lm(nd,lth,kr)
!
            ave_spec_l(nd,lth,kr) =  ave_spec_l(nd,lth,kr)              &
     &           + half * (tmp_l + spectr_pre_l(nd,lth,kr))             &
     &            * (time_sph - pre_time)
            spectr_pre_l(nd,lth,kr) =  tmp_l
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine sum_average_ene_spectr
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine sum_deviation_ene_sph(time_sph, pre_time,              &
     &          nri_sph, ncomp, spectr_t, ave_spec_t,                   &
     &          sigma_spec_t, spectr_pre_t)
!
      real(kind = kreal), intent(in) :: time_sph, pre_time
      integer(kind = kint), intent(in) :: nri_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_t(ncomp, nri_sph)
      real(kind = kreal), intent(in)                                    &
     &                   :: ave_spec_t(ncomp, nri_sph)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sigma_spec_t(ncomp, nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_pre_t(ncomp, nri_sph)
!
      integer(kind = kint) :: kr, nd
      real(kind= kreal) :: tmp
!
!
!$omp parallel private(kr,nd,tmp)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp_sph_spec
          tmp = (spectr_t(nd,kr) - ave_spec_t(nd,kr))**2
          sigma_spec_t(nd,kr) = sigma_spec_t(nd,kr)                     &
     &           + half * (tmp + spectr_pre_t(nd,kr))                   &
     &            * (time_sph - pre_time)
          spectr_pre_t(nd,kr) = tmp
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine sum_deviation_ene_sph
!
!   --------------------------------------------------------------------
!
      subroutine sum_deviation_ene_spectr(time_sph, pre_time,           &
     &          nri_sph, ltr_sph, ncomp, spectr_l, ave_spec_l,          &
     &          sigma_spec_l, spectr_pre_l)
!
      real(kind = kreal), intent(in) :: time_sph, pre_time
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_sph)
      real(kind = kreal), intent(in)                                    &
     &                   :: ave_spec_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sigma_spec_l(ncomp, 0:ltr_sph, nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_pre_l(ncomp, 0:ltr_sph, nri_sph)
!
      integer(kind = kint) :: kr, nd, lth
      real(kind= kreal) :: tmp_l
!
!
!$omp parallel private(kr,lth,nd,tmp_l)
      do kr = 1, nri_sph
        do lth = 0, ltr_sph
!$omp do
          do nd = 1, ncomp
            tmp_l =  (spectr_l(nd,lth,kr) - ave_spec_l(nd,lth,kr))**2
!
            sigma_spec_l(nd,lth,kr) =  sigma_spec_l(nd,lth,kr)          &
     &           + half * (tmp_l + spectr_pre_l(nd,lth,kr))             &
     &            * (time_sph - pre_time)
            spectr_pre_l(nd,lth,kr) =  tmp_l
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine sum_deviation_ene_spectr
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine divide_average_ene_sph                                 &
     &         (time_sph, time_ini, nri_sph, ncomp, ave_spec)
!
      real(kind = kreal), intent(in) :: time_sph, time_ini
      integer(kind = kint), intent(in) :: nri_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout) :: ave_spec(ncomp, nri_sph)
!
!
!$omp parallel workshare
      ave_spec(1:ncomp,1:nri_sph) = ave_spec(1:ncomp,1:nri_sph)         &
     &                             / (time_sph - time_ini)
!$omp end parallel workshare
!
      end subroutine divide_average_ene_sph
!
!   --------------------------------------------------------------------
!
      subroutine divide_average_ene_spectr(time_sph, time_ini,          &
     &          nri_sph, ltr_sph, ncomp, ave_spec_l)
!
      real(kind = kreal), intent(in) :: time_sph, time_ini
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout)                                 &
     &                   :: ave_spec_l(ncomp, 0:ltr_sph, nri_sph)
!
!
!$omp parallel workshare
      ave_spec_l(1:ncomp,0:ltr_sph,1:nri_sph)                           &
     &         = ave_spec_l(1:ncomp,0:ltr_sph,1:nri_sph)                &
     &          / (time_sph - time_ini)
!$omp end parallel workshare
!
      end subroutine divide_average_ene_spectr
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine divide_deviation_ene_sph                               &
     &         (time_sph, time_ini, nri_sph, ncomp, sigma_spec_t)
!
      real(kind = kreal), intent(in) :: time_sph, time_ini
      integer(kind = kint), intent(in) :: nri_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout) :: sigma_spec_t(ncomp, nri_sph)
!
!
!$omp parallel workshare
      sigma_spec_t(1:ncomp,1:nri_sph)                                   &
     &      = sqrt(sigma_spec_t(1:ncomp,1:nri_sph)                      &
     &       / (time_sph - time_ini))
!$omp end parallel workshare
!
      end subroutine divide_deviation_ene_sph
!
!   --------------------------------------------------------------------
!
      subroutine divide_deviation_ene_spectr(time_sph, time_ini,        &
     &          nri_sph, ltr_sph, ncomp, sigma_spec_l)
!
      real(kind = kreal), intent(in) :: time_sph, time_ini
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout)                                 &
     &                   :: sigma_spec_l(ncomp, 0:ltr_sph, nri_sph)
!
!
!$omp parallel workshare
      sigma_spec_l(1:ncomp,0:ltr_sph,1:nri_sph)                         &
     &      = sqrt(sigma_spec_l(1:ncomp,0:ltr_sph,1:nri_sph)            &
     &       / (time_sph - time_ini))
!$omp end parallel workshare
!
      end subroutine divide_deviation_ene_spectr
!
!   --------------------------------------------------------------------
!
      end module cal_tave_sph_ene_spectr
