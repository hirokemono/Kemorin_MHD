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
      use gz_spl_sph_spectr_data_IO
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
      integer(kind = kint) :: ierr
      integer(kind = kint) :: icou = 0
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
      icou = 0
      do
        call sel_copy_sph_monitor_lines                                 &
     &     (FPz_f1, id_read_file, id_write_file, flag_gzip1,            &
     &      trim_end_time, ione, zbuf1, zbuf2, ierr)
        if(ierr .ne. 0) exit
        icou = icou + 1
      end do
      write(*,*)
      write(*,*) 'total count:',  icou
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
      subroutine trim_sph_volume_spectr_file                            &
     &         (read_file_name, trimmed_file_name, trim_end_time)
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_open_sph_monitor_file
      use gz_spl_sph_spectr_data_IO
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
      integer(kind = kint) :: istep
      real(kind = kreal) :: time
!
      integer(kind = kint) :: ierr
      integer(kind = kint) :: icou = 0
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
      call read_sph_volume_spectr_head                                  &
     &  (FPz_f1, id_read_file, flag_gzip1, sph_lbl_IN1, sph_IN1, zbuf1)
!
      write(*,*) 'Open target file', ': ', trim(trimmed_file_name)
      call sel_open_sph_vol_monitor_file                                &
     &   (id_write_file, trimmed_file_name, sph_lbl_IN1, sph_IN1,       &
     &    zbuf2, flag_gzip1)
!
!
      icou = 0
      do
        call sel_copy_sph_monitor_lines                                 &
     &     (FPz_f1, id_read_file, id_write_file, flag_gzip1,            &
     &      trim_end_time, (sph_IN1%ltr_sph+1), zbuf1, zbuf2, ierr)
        if(ierr .ne. 0) exit
        icou = icou + 1
      end do
      write(*,*)
      write(*,*) 'total count:',  icou
!
      close(id_write_file)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_read_file, flag_gzip1, zbuf1)
!
      call dealloc_sph_espec_name(sph_IN1)
      return
!
      end subroutine trim_sph_volume_spectr_file
!
! -----------------------------------------------------------------------
!
      subroutine sel_copy_sph_monitor_lines                             &
     &         (FPz_f, id_read, id_write, flag_gzip,                    &
     &          trim_end_time, num_line, zbuf_rd, zbuf_wt, ierr)
!
      use select_gz_stream_file_IO
      use transfer_to_long_integers
!
      character, pointer, intent(in)  :: FPz_f
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_read, id_write
      real(kind = kreal), intent(in) :: trim_end_time
      integer(kind = kint), intent(in) :: num_line
      type(buffer_4_gzip), intent(inout) :: zbuf_rd, zbuf_wt
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: istep, l, i
      real(kind = kreal) :: time
!
!
      ierr = 1
      call sel_read_line_gz_stream(FPz_f, id_read, flag_gzip,           &
     &                             zbuf_rd)
      if(zbuf_rd%len_used .lt. 0) return
!
      read(zbuf_rd%fixbuf(1),*) istep, time
      if(time .gt. trim_end_time) return
!
      zbuf_rd%fixbuf(1)(zbuf_rd%len_used:zbuf_rd%len_used) = char(10)
      call sel_gz_write_text_stream_w_len(flag_gzip, id_write,          &
     &    cast_long(zbuf_rd%len_used), zbuf_rd%fixbuf(1), zbuf_wt)
!
      do l = 2, num_line
        call sel_read_line_gz_stream(FPz_f, id_read, flag_gzip,         &
     &                               zbuf_rd)
        zbuf_rd%fixbuf(1)(zbuf_rd%len_used:zbuf_rd%len_used) = char(10)
        call sel_gz_write_text_stream_w_len(flag_gzip, id_write,        &
     &      cast_long(zbuf_rd%len_used), zbuf_rd%fixbuf(1), zbuf_wt)
      end do
!
      write(*,'(40a1,a4,1pe25.12,a11)',advance="NO")                    &
     &      (char(8),i=1,40), 't = ', time, ' is copied.'
      ierr = 0
!
      end subroutine sel_copy_sph_monitor_lines
!
! -----------------------------------------------------------------------
!
      end module trim_sph_volume_mean
