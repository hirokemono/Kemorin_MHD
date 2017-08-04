!>@file   m_tave_field_on_circle.f90
!!@brief  module m_tave_field_on_circle
!!
!!@author H. Matsui
!!@date Programmed on June., 2011
!
!>@brief  time averaged field data on specific circle at (s,z)
!!
!!@verbatim
!!      subroutine allocate_tave_circle_field(d_circle)
!!      subroutine deallocate_tave_circle_field
!!
!!      subroutine sum_average_circle_field(circle, d_circle)
!!      subroutine sum_deviation_circle_field(circle, d_circle)
!!
!!      subroutine divide_average_circle_field(icou, d_circle)
!!      subroutine divide_deviation_circle_field(icou, d_circle)
!!
!!      subroutine copy_average_circle_field(circle, d_circle)
!!      subroutine copy_deviation_circle_field(circle, d_circle)
!!        type(fields_on_circle), intent(inout) :: circle
!!        type(phys_data), intent(inout) :: d_circle
!!@endverbatim
!
      module m_tave_field_on_circle
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_circle_transform
!
      use t_phys_data
      use t_field_on_circle
!
      implicit none
!
!>      file name for field data on a circle
      character(len=kchara) :: fname_tave_circle_fld                    &
     &                        = 'tave_circle_field.dat'
!>      file name for spectr power data on a circle
      character(len=kchara) :: fname_tave_circle_mag                    &
     &                        = 'tave_circle_spec_mag.dat'
!>      file name for spectr phase data on a circle
      character(len=kchara) :: fname_tave_circle_phs                    &
     &                        = 'tave_circle_spec_phase.dat'
!
!>      file name for field data on a circle
      character(len=kchara) :: fname_sigma_circle_fld                   &
     &                        = 'sigma_circle_field.dat'
!>      file name for spectr power data on a circle
      character(len=kchara) :: fname_sigma_circle_mag                   &
     &                        = 'sigma_circle_spec_mag.dat'
!>      file name for spectr phase data on a circle
      character(len=kchara) :: fname_sigma_circle_phs                   &
     &                        = 'sigma_circle_spec_phase.dat'
!
!
!>      Field data for circle point at equator
      real(kind = kreal), allocatable :: tave_v_rtp_circle(:,:)
!
!>      Spectr data for circle point collected to 0 process
      real(kind = kreal), allocatable :: tave_vrtm_mag(:,:)
!>      Spectr data for circle point collected to 0 process
      real(kind = kreal), allocatable :: tave_vrtm_phase(:,:)
!
!>      Field data for circle point at equator
      real(kind = kreal), allocatable :: sigma_v_rtp_circle(:,:)
!
!>      Spectr data for circle point collected to 0 process
      real(kind = kreal), allocatable :: sigma_vrtm_mag(:,:)
!>      Spectr data for circle point collected to 0 process
      real(kind = kreal), allocatable :: sigma_vrtm_phase(:,:)
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_tave_circle_field(d_circle)
!
      type(phys_data), intent(in) :: d_circle
!
!
      allocate(tave_v_rtp_circle(mphi_circle,d_circle%ntot_phys))
      allocate(sigma_v_rtp_circle(mphi_circle,d_circle%ntot_phys))
      tave_v_rtp_circle = 0.0d0
      sigma_v_rtp_circle = 0.0d0
!
      allocate(tave_vrtm_mag(0:mphi_circle,d_circle%ntot_phys) )
      allocate(tave_vrtm_phase(0:mphi_circle,d_circle%ntot_phys) )
      allocate(sigma_vrtm_mag(0:mphi_circle,d_circle%ntot_phys) )
      allocate(sigma_vrtm_phase(0:mphi_circle,d_circle%ntot_phys) )
      tave_vrtm_mag =    0.0d0
      tave_vrtm_phase =  0.0d0
      sigma_vrtm_mag =   0.0d0
      sigma_vrtm_phase = 0.0d0
!
      end subroutine allocate_tave_circle_field
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_tave_circle_field
!
!
      deallocate(sigma_vrtm_mag, sigma_vrtm_phase)
      deallocate(tave_v_rtp_circle, tave_vrtm_phase)
      deallocate(sigma_v_rtp_circle, tave_vrtm_mag)
!
      end subroutine deallocate_tave_circle_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sum_average_circle_field(circle, d_circle)
!
      type(fields_on_circle), intent(in) :: circle
      type(phys_data), intent(in) :: d_circle
!
      integer(kind = kint) :: mphi, nd
!
!$omp parallel do private(mphi)
      do nd = 1, d_circle%ntot_phys
        do mphi = 1, mphi_circle
          tave_v_rtp_circle(mphi,nd) = tave_v_rtp_circle(mphi,nd)       &
     &                                + d_circle%d_fld(mphi,nd)
        end do
!
        do mphi = 0, mphi_circle / 2
          tave_vrtm_mag(mphi,nd) = tave_vrtm_mag(mphi,nd)               &
     &                                + circle%vrtm_mag(mphi,nd)
          tave_vrtm_phase(mphi,nd) = tave_vrtm_phase(mphi,nd)           &
     &                                + circle%vrtm_phase(mphi,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine sum_average_circle_field
!
! ----------------------------------------------------------------------
!
      subroutine sum_deviation_circle_field(circle, d_circle)
!
      type(fields_on_circle), intent(in) :: circle
      type(phys_data), intent(in) :: d_circle
!
      integer(kind = kint) :: mphi, nd
!
!$omp parallel do private(mphi)
      do nd = 1, d_circle%ntot_phys
        do mphi = 1, mphi_circle
          sigma_v_rtp_circle(mphi,nd) = sigma_v_rtp_circle(mphi,nd)     &
     &      + (d_circle%d_fld(mphi,nd) - tave_v_rtp_circle(mphi,nd))**2
        end do
!
        do mphi = 0, mphi_circle / 2
          sigma_vrtm_mag(mphi,nd) = sigma_vrtm_mag(mphi,nd)             &
     &     + (circle%vrtm_mag(mphi,nd) - tave_vrtm_mag(mphi,nd))**2
          sigma_vrtm_phase(mphi,nd) = sigma_vrtm_phase(mphi,nd)         &
     &     + (circle%vrtm_phase(mphi,nd) - tave_vrtm_phase(mphi,nd))**2
        end do
      end do
!$omp end parallel do
!
      end subroutine sum_deviation_circle_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine divide_average_circle_field(icou, d_circle)
!
      integer(kind = kint), intent(in) :: icou
      type(phys_data), intent(in) :: d_circle
!
      integer(kind = kint) :: mphi, nd
!
!$omp parallel do private(mphi)
      do nd = 1, d_circle%ntot_phys
        do mphi = 1, mphi_circle
          tave_v_rtp_circle(mphi,nd) = tave_v_rtp_circle(mphi,nd)       &
     &                                / dble(icou)
        end do
!
        do mphi = 0, mphi_circle / 2
          tave_vrtm_mag(mphi,nd) = tave_vrtm_mag(mphi,nd)               &
     &                                / dble(icou)
          tave_vrtm_phase(mphi,nd) = tave_vrtm_phase(mphi,nd)           &
     &                                / dble(icou)
        end do
      end do
!$omp end parallel do
!
      end subroutine divide_average_circle_field
!
! ----------------------------------------------------------------------
!
      subroutine divide_deviation_circle_field(icou, d_circle)
!
      integer(kind = kint), intent(in) :: icou
      type(phys_data), intent(in) :: d_circle
!
      integer(kind = kint) :: mphi, nd
!
!$omp parallel do private(mphi)
      do nd = 1, d_circle%ntot_phys
        do mphi = 1, mphi_circle
          sigma_v_rtp_circle(mphi,nd)                                   &
     &        = sqrt(sigma_v_rtp_circle(mphi,nd))  / dble(icou)
        end do
!
        do mphi = 0, mphi_circle / 2
          sigma_vrtm_mag(mphi,nd)                                       &
     &        = sqrt(sigma_vrtm_mag(mphi,nd))  / dble(icou)
          sigma_vrtm_phase(mphi,nd)                                     &
     &        = sqrt( sigma_vrtm_phase(mphi,nd)) / dble(icou)
        end do
      end do
!$omp end parallel do
!
      end subroutine divide_deviation_circle_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_average_circle_field(circle, d_circle)
!
      type(fields_on_circle), intent(inout) :: circle
      type(phys_data), intent(inout) :: d_circle
!
      integer(kind = kint) :: mphi, nd
!
      circle%fname_circle_fld = fname_tave_circle_fld
      circle%fname_circle_mag = fname_tave_circle_mag
      circle%fname_circle_phs = fname_tave_circle_phs
!
!$omp parallel do private(mphi)
      do nd = 1, d_circle%ntot_phys
        do mphi = 1, mphi_circle
          d_circle%d_fld(mphi,nd) = tave_v_rtp_circle(mphi,nd)
        end do
!
        do mphi = 0, mphi_circle / 2
          circle%vrtm_mag(mphi,nd) = tave_vrtm_mag(mphi,nd)
          circle%vrtm_phase(mphi,nd) = tave_vrtm_phase(mphi,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_average_circle_field
!
! ----------------------------------------------------------------------
!
      subroutine copy_deviation_circle_field(circle, d_circle)
!
      type(fields_on_circle), intent(inout) :: circle
      type(phys_data), intent(inout) :: d_circle
!
      integer(kind = kint) :: mphi, nd
!
      circle%fname_circle_fld = fname_sigma_circle_fld
      circle%fname_circle_mag = fname_sigma_circle_mag
      circle%fname_circle_phs = fname_sigma_circle_phs
!
!$omp parallel do private(mphi)
      do nd = 1, d_circle%ntot_phys
        do mphi = 1, mphi_circle
          d_circle%d_fld(mphi,nd) = sigma_v_rtp_circle(mphi,nd)
        end do
!
        do mphi = 0, mphi_circle / 2
          circle%vrtm_mag(mphi,nd) = sigma_vrtm_mag(mphi,nd)
          circle%vrtm_phase(mphi,nd) = sigma_vrtm_phase(mphi,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_deviation_circle_field
!
! ----------------------------------------------------------------------
!
      end module m_tave_field_on_circle
