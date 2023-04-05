!>@file   trim_sph_volume_mean.f90
!!        program trim_sph_volume_mean
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2020
!!
!!
!> @brief Trim volume mean square data file
!!
!!@verbatim
!!      subroutine trim_sph_volume_mean_file                            &
!!     &         (read_file_name, trimmed_file_name)
!!        character(len=kchara), intent(in) :: read_file_name
!!        character(len=kchara), intent(in) :: trimmed_file_name
!!@endverbatim
      module trim_sph_volume_mean
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_read_sph_spectra
      use t_pick_copy_monitor_data
      use t_buffer_4_gzip
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine trim_sph_volume_mean_file                              &
     &         (read_file_name, trimmed_file_name, trim_end_time)
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_open_sph_monitor_file
!
      use transfer_to_long_integers
      use data_convert_by_zlib
!
      implicit none
!
      character(len=kchara), intent(in) :: read_file_name
      character(len=kchara), intent(in) :: trimmed_file_name
      real(kind = kreal), intent(in) :: trim_end_time
!
!
      type(read_sph_spectr_data), save :: sph_IN1
      type(read_sph_spectr_data), save :: sph_OUT1
      type(sph_spectr_head_labels), save :: sph_lbl_IN1
      type(sph_spectr_head_labels), save :: sph_lbl_OUT1
!
      integer(kind = kint), parameter :: id_read_file =  15
      integer(kind = kint), parameter :: id_write_file = 16
!
      integer(kind = kint) :: istep_start
      real(kind = kreal) :: time
!
      integer(kind = kint) :: ntot_pick
!
      logical :: flag_gzip1
      logical :: error
      type(buffer_4_gzip) :: zbuf1, zbuf2
      character, pointer, save  :: FPz_f1
      type(monitor_field_pickup_table), save :: comp_tbl1
!
!
      write(*,*) 'Open data file to append.'
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_read_file, read_file_name, flag_gzip1, zbuf1)
      call read_sph_volume_mean_head(FPz_f1, id_read_file, flag_gzip1,  &
     &                               sph_lbl_IN1, sph_IN1, zbuf1)
!
      write(*,*) 'Open target file', ': ', trim(trimmed_file_name)
      call sel_open_sph_vol_monitor_file                                &
     &   (id_write_file, trimmed_file_name, sph_lbl_IN1, sph_IN1,       &
     &    zbuf2, flag_gzip1)
!
!
      do
        call sel_read_line_gz_stream(FPz_f1, id_read_file, flag_gzip1,  &
     &                               zbuf1)
        if(zbuf1%len_used .lt. 0) exit
!
        read(zbuf1%fixbuf(1),*) istep_start, time
        if(time .gt. trim_end_time) exit
!
        if(flag_gzip1) then
          call gzip_defleate_characters_b(cast_long(zbuf1%len_used+1),  &
     &                                    zbuf1%fixbuf(1), zbuf2)
          write(id_write_file) zbuf2%gzip_buf(1:zbuf2%ilen_gzipped)
          call dealloc_zip_buffer(zbuf2)
        else
          write(id_write_file) zbuf1%fixbuf(1)(1:zbuf1%len_used+1)
        end if
      end do
!
      close(id_write_file)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_read_file, flag_gzip1, zbuf1)
!
      call dealloc_sph_espec_name(sph_IN1)
      return
!
      end subroutine trim_sph_volume_mean_file
!
! -----------------------------------------------------------------------
!
      end module trim_sph_volume_mean
