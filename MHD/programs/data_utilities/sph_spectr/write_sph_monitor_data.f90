!>@file   write_sph_monitor_data.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!         modified in Sep., 2022
!!
!
!> @brief Time spectrum data output routines for utilities
!!
!!@verbatim
!!      subroutine select_output_sph_pwr_head                           &
!!     &         (flag_gzip, id_file, flag_vol_ave, sph_OUT, zbuf)
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: id_file
!!        logical, intent(in) :: flag_vol_ave
!!        type(read_sph_spectr_params), intent(in) :: sph_OUT
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine select_output_sph_series_data                        &
!!     &         (id_file, flag_spectr, flag_vol_ave, sph_OUT)
!!        integer(kind = kint), intent(in) :: id_file
!!        logical, intent(in) :: flag_spectr, flag_vol_ave
!!        type(read_sph_spectr_params), intent(in) :: sph_OUT
!!@endverbatim
!
      module write_sph_monitor_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_read_sph_spectra
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine select_output_sph_pwr_head                             &
     &         (flag_gzip, id_file, flag_vol_ave, sph_OUT, zbuf)
!
      use t_buffer_4_gzip
      use gz_open_sph_monitor_file
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_vol_ave
      type(read_sph_spectr_params), intent(in) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      if(flag_vol_ave) then
        call write_sph_pwr_vol_head(flag_gzip, id_file,                 &
     &                              sph_pwr_labels, sph_OUT, zbuf)
      else
        call write_sph_pwr_layer_head(flag_gzip, id_file,               &
     &                                sph_OUT, zbuf)
      end if
!
      end subroutine select_output_sph_pwr_head
!
!   --------------------------------------------------------------------
!
      subroutine select_output_sph_series_data(flag_gzip, id_file,      &
     &          flag_spectr, flag_vol_ave, sph_OUT, zbuf)
!
      use gz_layer_mean_monitor_IO
      use gz_layer_spectr_monitor_IO
      use gz_volume_spectr_monitor_IO
      use select_gz_stream_file_IO
      use sph_monitor_data_text
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_spectr, flag_vol_ave
      type(read_sph_spectr_params), intent(in) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      if(flag_spectr) then
        if(flag_vol_ave) then
          call sel_gz_write_volume_spectr_mtr(flag_gzip, id_file,       &
     &        sph_OUT%i_step, sph_OUT%time, sph_OUT%ltr_sph,            &
     &        sph_OUT%ntot_sph_spec, sph_OUT%spectr_IO(1,0,1), zbuf)
        else
          call sel_gz_write_layer_spectr_mtr(flag_gzip, id_file,        &
     &        sph_OUT%i_step, sph_OUT%time,                             &
     &        sph_OUT%nri_sph, sph_OUT%kr_sph, sph_OUT%r_sph,           &
     &        sph_OUT%ltr_sph, sph_OUT%ntot_sph_spec,                   &
     &        sph_OUT%spectr_IO, zbuf)
        end if
      else
        if(flag_vol_ave) then
          call sel_gz_write_text_stream(flag_gzip, id_file,             &
     &        volume_pwr_data_text(sph_OUT%i_step, sph_OUT%time,        &
     &        sph_OUT%ntot_sph_spec, sph_OUT%spectr_IO(1,0,1)), zbuf)
        else
          call sel_gz_write_layer_mean_mtr                              &
     &         (flag_gzip, id_file, sph_OUT%i_step, sph_OUT%time,       &
     &          sph_OUT%nri_sph, sph_OUT%kr_sph, sph_OUT%r_sph,         &
     &          sph_OUT%ntot_sph_spec, sph_OUT%spectr_IO(1,0,1), zbuf)
        end if
      end if
!
      end subroutine select_output_sph_series_data
!
!   --------------------------------------------------------------------
!
      end module write_sph_monitor_data
