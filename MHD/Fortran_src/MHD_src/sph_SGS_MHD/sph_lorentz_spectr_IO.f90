!>@file   sph_lorentz_spectr_IO.f90
!!@brief  module sph_lorentz_spectr_IO
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!!      subroutine init_sph_lorentz_spectr_data(sph, ipol, ipol_LES,    &
!!     &          rj_fld, ene_labels, lor_spectr, WK_lor_spectr)
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(phys_data), intent(in) :: rj_fld
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(sph_mean_squares), intent(inout) :: lor_spectr
!!        type(sph_mean_square_work), intent(inout) :: WK_lor_spectr
!!      subroutine output_sph_lorentz_spectr_data                       &
!!     &         (time_d, sph, ene_labels, lor_spectr)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_grids), intent(in) :: sph
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(sph_mean_squares), intent(in) :: lor_spectr
!!@endverbatim
!
      module sph_lorentz_spectr_IO
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine init_sph_lorentz_spectr_data(sph, ipol, ipol_LES,      &
     &          rj_fld, ene_labels, lor_spectr, WK_lor_spectr)
!
      use m_error_IDs
      use t_spheric_parameter
      use t_phys_address
      use t_SGS_model_addresses
      use t_phys_data
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
!
      use calypso_mpi
      use calypso_mpi_logical
      use init_sph_lorentz_spectr
      use output_sph_pwr_volume_file
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(phys_data), intent(in) :: rj_fld
      type(energy_label_param), intent(in) :: ene_labels
!
      type(sph_mean_squares), intent(inout) :: lor_spectr
      type(sph_mean_square_work), intent(inout) :: WK_lor_spectr
!
      logical :: flag
!
!
      if((lor_spectr%iflag_layer_rms_spec                               &
     &   + lor_spectr%num_vol_spectr) .eq. 0) return
!
      if(iflag_debug .gt. 0) write(*,*) 's_init_rms_4_sph_spectr'
      call s_init_sph_lorentz_spectr                                    &
     &   (sph%sph_params, sph%sph_rj, ipol, ipol_LES, rj_fld,           &
     &    lor_spectr, WK_lor_spectr)
!
      if(lor_spectr%num_vol_spectr .le. 0) return
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &      'error_sph_vol_ms_file in init_sph_lorentz_spectr_data'
      flag = error_sph_vol_ms_file(my_rank, ene_labels,                 &
     &                             sph%sph_params, sph%sph_rj,          &
     &                             lor_spectr%v_spectr(1))
      call calypso_mpi_bcast_one_logical                                &
     &  (flag, lor_spectr%v_spectr(1)%irank_m)
      if(flag) then
        call calypso_mpi_barrier
        call calypso_mpi_abort(ierr_file,                               &
     &     'Field information might be updated.')
      end if
!
      end subroutine init_sph_lorentz_spectr_data
!
!  --------------------------------------------------------------------
!
      subroutine output_sph_lorentz_spectr_data                         &
     &         (time_d, sph, ene_labels, lor_spectr)
!
      use t_time_data
      use t_spheric_parameter
      use t_energy_label_parameters
      use t_rms_4_sph_spectr
      use cal_write_sph_monitor_data
!
      type(time_data), intent(in) :: time_d
      type(sph_grids), intent(in) :: sph
!
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_mean_squares), intent(in) :: lor_spectr
!
!
      if((lor_spectr%iflag_layer_rms_spec                               &
     &   + lor_spectr%num_vol_spectr) .eq. 0) return
!
      call output_sph_mean_square_files(ene_labels, time_d,             &
     &    sph%sph_params, sph%sph_rj, lor_spectr)
!
      end subroutine output_sph_lorentz_spectr_data
!
!  --------------------------------------------------------------------
!
      end module sph_lorentz_spectr_IO
