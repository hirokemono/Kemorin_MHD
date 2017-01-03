!>@file   m_ctl_data_4_pickup_sph.f90
!!        module m_ctl_data_4_pickup_sph
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine deallocate_vol_sopectr_ctl
!!
!!      subroutine read_pickup_sph_ctl
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin sph_monitor_ctl
!!    volume_average_prefix        'sph_ave_volume'
!!    volume_pwr_spectr_prefix     'sph_pwr_volume'
!!
!!    nusselt_number_prefix        'Nusselt'
!!!
!!    array volume_spectrum_ctl      2
!!      ...
!!    end array volume_spectrum_ctl
!!
!!    begin layered_spectrum_ctl
!!      ...
!!    end   layered_spectrum_ctl
!!
!!    begin gauss_coefficient_ctl
!!      ...
!!    end   gauss_coefficient_ctl
!!
!!    begin pickup_spectr_ctl
!!      ...
!!    end   pickup_spectr_ctl
!!
!!    begin mid_equator_monitor_ctl
!!      ...
!!    end   mid_equator_monitor_ctl
!!  end sph_monitor_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module m_ctl_data_4_pickup_sph
!
      use m_precision
!
      use t_ctl_data_4_sph_monitor
!
      implicit  none
!
!
      type(sph_monitor_control), save :: smonitor_ctl1
!
!    label for entry
!
      character(len=kchara), parameter                                  &
     &                     :: hd_pick_sph = 'sph_monitor_ctl'
      integer(kind = kint) :: i_pick_sph = 0
!
      private :: hd_pick_sph, i_pick_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_pickup_sph_ctl
!
!
      call read_sph_monitoring_ctl                                      &
     &   (hd_pick_sph, i_pick_sph, smonitor_ctl1)
!
      end subroutine read_pickup_sph_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_4_pickup_sph
