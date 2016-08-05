!m_pickup_sph_rms_data.f90
!      module m_pickup_sph_rms_data
!
!        programmed by H.Matsui on Dec., 2012
!
!>@file   m_pickup_sph_rms_data.f90
!!@brief  module m_pickup_sph_rms_data
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data IO routines for monitoring mean square spectrum data
!!
!!@verbatim
!!      subroutine allocate_pick_sph_rms
!!      subroutine deallocate_pick_sph_rms
!!
!!      subroutine write_sph_rms_4_monitor(my_rank, i_step, time)
!!
!!      subroutine open_sph_rms_read_monitor(id_pick)
!!      subroutine read_sph_rms_4_monitor(id_pick, i_step, time, ierr)
!!@endverbatim
!!
!!@n @param my_rank     Procdess ID
!!@n @param i_step      Time step
!!@n @param time        Time
!!@n @param ierr        Error flag (No error: 0)
!
      module m_pickup_sph_rms_data
!
      use m_precision
      use m_constants
!
      use t_pickup_sph_spectr_data
!
      implicit  none
!
!
!>        Structure for pickup list
      type(picked_spectrum_data), save :: pick_rms1
!
      character(len = kchara) :: pickup_sph_rms_head =  'picked_ene_spec'
!
      end module m_pickup_sph_rms_data
