!>@file   gz_spl_sph_spectr_data_IO.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief gzipped spectr monitor data reading routines
!!
!!@verbatim
!!        subroutine sel_gz_input_sph_series_data(FPz_f, id_stream,     &
!!     &          flag_gzip, flag_old_fmt, flag_spectr, flag_vol_ave,   &
!!     &          sph_IN, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip
!!        logical, intent(in) :: flag_old_fmt, flag_spectr, flag_vol_ave
!!        type(read_sph_spectr_params), intent(inout) :: sph_IN
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine sel_gz_input_sph_layer_spectr                        &
!!     &         (FPz_f, id_stream, flag_gzip, flag_old_fmt,            &
!!     &          sph_IN, l_spec_IO, zbuf, ierr)
!!      subroutine sel_gz_input_sph_layer_mean                          &
!!     &         (FPz_f, id_stream, flag_gzip, flag_old_fmt,            &
!!     &          sph_IN, l_mean_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip
!!        logical, intent(in) :: flag_old_fmt
!!        type(read_sph_spectr_params), intent(inout) :: sph_IN
!!        type(layer_spectr_data_IO), intent(inout) :: l_spec_IO
!!        type(layer_mean_data_IO), intent(inout) :: l_mean_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        integer(kind = kint), intent(inout) :: ierr
!!
!!      subroutine gz_copy_spectr_monitor_data(FPz_f, id_ascii,         &
!!     &                                       zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_ascii
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        integer(kind = kint), intent(inout) :: ierr
!!
!!      subroutine gz_read_volume_pwr_sph(FPz_f, id_stream, flag_gzip,  &
!!     &          ntot_comp, i_step, time, spectr_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        integer(kind = kint), intent(inout) :: i_step
!!        real(kind = kreal), intent(inout) :: time
!!        real(kind = kreal), intent(inout) :: spectr_IO(ntot_comp)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        integer(kind = kint), intent(inout) :: ierr
!!@endverbatim
!
      module gz_spl_sph_spectr_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_read_sph_spectra
      use t_sph_monitor_data_IO
      use t_buffer_4_gzip
      use gzip_file_access
      use skip_gz_comment
!
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sel_gz_input_sph_series_data(FPz_f, id_stream,         &
     &          flag_gzip, flag_old_fmt, flag_spectr, flag_vol_ave,     &
     &          sph_IN, zbuf, ierr)
!
      use old_sph_spectr_data_IO
      use gz_volume_spectr_monitor_IO
      use gz_layer_mean_monitor_IO
      use gz_layer_spectr_monitor_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      logical, intent(in) :: flag_old_fmt, flag_spectr, flag_vol_ave
      type(read_sph_spectr_params), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(flag_vol_ave) then
        if(flag_spectr) then
          call sel_gz_read_volume_spectr_mtr(FPz_f, id_stream,          &
     &        flag_gzip, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,          &
     &        sph_IN%i_step, sph_IN%time, sph_IN%i_mode,                &
     &        sph_IN%spectr_IO(1,0,1), zbuf, ierr)
        else
          call gz_read_volume_pwr_sph(FPz_f, id_stream, flag_gzip,    &
     &        sph_IN%ntot_sph_spec, sph_IN%i_step, sph_IN%time,       &
     &        sph_IN%spectr_IO(1,0,1), zbuf, ierr)
        end if
      else
        if(flag_spectr) then
          if(flag_old_fmt) then
            call gz_read_layer_spectr_sph_old                           &
     &         (FPz_f, id_stream, flag_gzip,                            &
     &          sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,   &
     &          sph_IN%i_step, sph_IN%time, sph_IN%kr_sph,              &
     &          sph_IN%i_mode, sph_IN%spectr_IO(1,0,1), zbuf, ierr)
          else
            call sel_gz_read_layer_spectr_mtr                           &
     &         (FPz_f, id_stream, flag_gzip,                            &
     &          sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,   &
     &          sph_IN%i_step, sph_IN%time, sph_IN%kr_sph,              &
     &          sph_IN%r_sph, sph_IN%i_mode, sph_IN%spectr_IO(1,0,1),   &
     &          zbuf, ierr)
          end if
        else
          if(flag_old_fmt) then
            call gz_read_layer_pwr_sph_old                              &
     &         (FPz_f, id_stream, flag_gzip,                            &
     &          sph_IN%nri_sph, sph_IN%ntot_sph_spec, sph_IN%i_step,    &
     &          sph_IN%time, sph_IN%kr_sph, sph_IN%spectr_IO(1,0,1),    &
     &          zbuf, ierr)
          else
            call sel_gz_read_layer_mean_mtr                             &
     &         (FPz_f, id_stream, flag_gzip,                            &
     &          sph_IN%nri_sph, sph_IN%ntot_sph_spec, sph_IN%i_step,    &
     &          sph_IN%time, sph_IN%kr_sph, sph_IN%r_sph,               &
     &          sph_IN%spectr_IO(1,0,1), zbuf, ierr)
          end if
        end if
      end if
!
      end subroutine sel_gz_input_sph_series_data
!
!   --------------------------------------------------------------------
!
      subroutine sel_gz_input_sph_layer_spectr                          &
     &         (FPz_f, id_stream, flag_gzip, flag_old_fmt,              &
     &          sph_IN, l_spec_IO, zbuf, ierr)
!
      use old_sph_spectr_data_IO
      use gz_layer_spectr_monitor_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip, flag_old_fmt
      type(read_sph_spectr_params), intent(inout) :: sph_IN
      type(layer_spectr_data_IO), intent(inout) :: l_spec_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(flag_old_fmt) then
        call gz_read_layer_spectr_sph_old                               &
     &     (FPz_f, id_stream, flag_gzip,                                &
     &      sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,       &
     &      sph_IN%i_step, sph_IN%time, sph_IN%kr_sph,                  &
     &      sph_IN%i_mode, l_spec_IO%spec_r_IO, zbuf, ierr)
      else
        call sel_gz_read_layer_spectr_mtr                               &
     &     (FPz_f, id_stream, flag_gzip,                                &
     &      sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,       &
     &      sph_IN%i_step, sph_IN%time, sph_IN%kr_sph, sph_IN%r_sph,    &
     &      sph_IN%i_mode, l_spec_IO%spec_r_IO, zbuf, ierr)
      end if
!
      end subroutine sel_gz_input_sph_layer_spectr
!
!   --------------------------------------------------------------------
!
      subroutine sel_gz_input_sph_layer_mean                            &
     &         (FPz_f, id_stream, flag_gzip, flag_old_fmt,              &
     &          sph_IN, l_mean_IO, zbuf, ierr)
!
      use old_sph_spectr_data_IO
      use gz_layer_mean_monitor_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip, flag_old_fmt
      type(read_sph_spectr_params), intent(inout) :: sph_IN
      type(layer_mean_data_IO), intent(inout) :: l_mean_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(flag_old_fmt) then
        call gz_read_layer_pwr_sph_old(FPz_f, id_stream, flag_gzip,     &
     &      sph_IN%nri_sph, sph_IN%ntot_sph_spec, sph_IN%i_step,        &
     &      sph_IN%time, sph_IN%kr_sph, l_mean_IO%sq_r_IO, zbuf, ierr)
      else
        call sel_gz_read_layer_mean_mtr(FPz_f, id_stream, flag_gzip,    &
     &      sph_IN%nri_sph, sph_IN%ntot_sph_spec, sph_IN%i_step,        &
     &      sph_IN%time, sph_IN%kr_sph, sph_IN%r_sph,                   &
     &      l_mean_IO%sq_r_IO, zbuf, ierr)
      end if
!
      end subroutine sel_gz_input_sph_layer_mean
!
!   --------------------------------------------------------------------
!
      subroutine gz_copy_spectr_monitor_data(FPz_f, id_read, id_write,  &
     &                                       flag_gzip, zbuf, ierr)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_read, id_write
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 1
      call sel_read_line_gz_stream(FPz_f, id_read, flag_gzip, zbuf)
      if(zbuf%len_used .lt. 0) return
!
      write(id_write,'(a)') zbuf%fixbuf(1)(1:zbuf%len_used-1)
      ierr = 0
!
      end subroutine gz_copy_spectr_monitor_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine gz_read_volume_pwr_sph(FPz_f, id_stream, flag_gzip,    &
     &          ntot_comp, i_step, time, spectr_IO, zbuf, ierr)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: ntot_comp
!
      integer(kind = kint), intent(inout) :: i_step
      real(kind = kreal), intent(inout) :: time
      real(kind = kreal), intent(inout) :: spectr_IO(ntot_comp)
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 1
      call sel_read_line_gz_stream(FPz_f, id_stream, flag_gzip, zbuf)
      if(zbuf%len_used .lt. 0) return
!
      read(zbuf%fixbuf(1),*,err=99) i_step, time,                       &
     &                             spectr_IO(1:ntot_comp)
      ierr = 0
      return
!
   99 continue
      return
!
      end subroutine gz_read_volume_pwr_sph
!
!   --------------------------------------------------------------------
!
      end module gz_spl_sph_spectr_data_IO
