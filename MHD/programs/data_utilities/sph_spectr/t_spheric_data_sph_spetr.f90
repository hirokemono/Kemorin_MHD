!>@file   t_spheric_data_sph_spetr.f90
!!@brief  module t_spheric_data_sph_spetr
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!!
!!@brief  indexing table of speherical harmonics transform
!!
!!@verbatim
!!      subroutine init_rms_4_sph_spectr_util(sph, rj_fld, monitor_s)
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(sph_spectr_monitor_data), intent(inout) :: monitor_s
!!      subroutine init_sph_rms_4_monitor(sph, pick_list, monitor_s)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(pickup_mode_list), intent(inout) :: pick_list
!!        type(sph_spectr_monitor_data), intent(inout) :: monitor_s
!!      subroutine write_rms_4_sph_spectr_util                          &
!!     &         (my_rank, time_d, sph, monitor_s)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_spectr_monitor_data), intent(inout) :: monitor_s
!!@endverbatim
!
      module t_spheric_data_sph_spetr
!
      use m_precision
!
!
      use t_ctl_data_4_sph_utils
      use t_SPH_mesh_field_data
      use t_time_data
      use t_field_data_IO
      use t_schmidt_poly_on_rtm
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
      use t_SPH_SGS_structure
!
      implicit none
!
!
      type sph_spectr_monitor_data
!>        Structure of energy label
        type(energy_label_param), save :: ene_labels
!
!>        Structure for pickup list
        type(picked_spectrum_data), save :: pick_rms
!
!>        Structure of mean square data
        type(sph_mean_squares), save :: pwr
!>        Work structure of mean square data
        type(sph_mean_square_work), save :: WK_pwr
      end type sph_spectr_monitor_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_rms_4_sph_spectr_util(sph, rj_fld, monitor_s)
!
      use cal_rms_fields_by_sph
      use init_energy_labels_sph_SGS
!
      type(sph_grids), intent(in) :: sph
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_spectr_monitor_data), intent(inout) :: monitor_s
!
!
      if(iflag_debug .gt. 0) write(*,*) 'init_rms_4_sph_spectr'
      call init_energy_labels_w_filter(monitor_s%ene_labels)
      call init_rms_4_sph_spectr                                        &
     &   (sph%sph_params, sph%sph_rj, rj_fld,                           &
     &    monitor_s%pwr, monitor_s%WK_pwr)
!
      end subroutine init_rms_4_sph_spectr_util
!
!  --------------------------------------------------------------------
!
      subroutine init_sph_rms_4_monitor(sph, pick_list, monitor_s)
!
      use pickup_sph_coefs
      use sph_mean_spectr_header_IO
!
      type(sph_grids), intent(in) :: sph
      type(sph_mean_squares), intent(in) :: pwr
!
      type(pickup_mode_list), intent(inout) :: pick_list
      type(sph_spectr_monitor_data), intent(inout) :: monitor_s
!
      integer(kind = kint) :: iflag_center = 0
!
!
      call init_sph_radial_monitor_list                                 &
     &   (sph%sph_rj, monitor_s%pick_rms, iflag_center)
!
      call const_picked_sph_address(iflag_center,                       &
     &    sph%sph_params%l_truncation, sph%sph_rj,                      &
     &    pick_list, monitor_s%pick_rms)
!
      call set_sph_rms_labels_4_monitor                                 &
     &   (monitor_s%ene_labels, monitor_s%pwr, monitor_s%pick_rms)
!
      end subroutine init_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine write_rms_4_sph_spectr_util                            &
     &         (my_rank, time_d, sph, monitor_s)
!
      use output_sph_m_square_file
!
      integer, intent(in) :: my_rank
      type(time_data), intent(in) :: time_d
      type(sph_grids), intent(in) :: sph
      type(sph_spectr_monitor_data), intent(inout) :: monitor_s
!
!
      call write_sph_vol_ave_file(monitor_s%ene_labels, time_d,         &
     &    sph%sph_params, sph%sph_rj, monitor_s%pwr)
      call write_sph_vol_ms_file                                        &
     &   (my_rank, monitor_s%ene_labels, time_d,                        &
     &    sph%sph_params, sph%sph_rj, monitor_s%pwr)
      call write_sph_vol_ms_spectr_file                                 &
     &   (my_rank, monitor_s%ene_labels, time_d,                        &
     &    sph%sph_params, sph%sph_rj, monitor_s%pwr)
      call write_sph_layer_ms_file(my_rank, monitor_s%ene_labels,       &
     &    time_d, sph%sph_params, monitor_s%pwr)
      call write_sph_layer_spectr_file(my_rank, monitor_s%ene_labels,   &
     &    time_d, sph%sph_params, monitor_s%pwr)
!
      end subroutine write_rms_4_sph_spectr_util
!
!  --------------------------------------------------------------------
!
      end module t_spheric_data_sph_spetr
