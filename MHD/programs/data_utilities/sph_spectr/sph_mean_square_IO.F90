!>@file   sph_mean_square_IO.F90
!!@brief  module sph_mean_square_IO
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
!!@verbatim
!!      subroutine open_write_sph_monitor_file(id_file, file_name)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: file_name
!!      subroutine sel_open_read_sph_monitor_file                       &
!!     &         (FPz_f, id_file, file_name, flag_gzip, zbuf)
!!      subroutine sel_close_sph_monitor_file                           &
!!     &         (FPz_f, id_file, flag_gzip, zbuf)
!!        character, pointer, intent(inout) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: file_name
!!        logical, intent(inout) :: flag_gzip
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine select_input_sph_series_head(FPz_f, id_file,         &
!!     &          flag_gzip, flag_old_fmt, flag_spectr, flag_vol_ave,   &
!!     &          sph_IN, zbuf)
!!      subroutine select_input_sph_series_data(FPz_f, id_file,         &
!!     &          flag_gzip, flag_old_fmt, flag_spectr, flag_vol_ave,   &
!!     &          sph_IN, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_file
!!        logical, intent(in) :: flag_gzip, flag_old_fmt
!!        logical, intent(in) :: flag_spectr, flag_vol_ave
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine select_read_sph_spectr_time(FPz_f, id_file,          &
!!     &          flag_gzip, nitem_snap, i_step, time, zbuf, ierr)
!!        integer(kind = kint), intent(in) :: id_file
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: nitem_snap
!!        integer(kind = kint), intent(inout) :: i_step, ierr
!!        real(kind = kreal), intent(inout) :: time
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine select_copy_sph_monitor_data                         &
!!     &         (FPz_f, id_read, id_write, flag_gzip,                  &
!!     &          n_line, nchara_line, textbuf, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_read, id_write
!!        integer(kind = kint), intent(in) :: n_line
!!        integer(kind = kint), intent(in) :: nchara_line
!!        logical, intent(in) :: flag_gzip
!!        character(len = 1), intent(inout) :: textbuf(nchara_line)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        integer(kind = kint), intent(inout) :: ierr
!!
!!      integer(kind = kint) function lengh_spectr_data_line            &
!!     &                   (flag_spectr, flag_vol_ave, sph_IN)
!!        logical, intent(in) :: flag_spectr, flag_vol_ave
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!@endverbatim
!
      module sph_mean_square_IO
!
      use m_precision
      use t_buffer_4_gzip
      use t_read_sph_spectra
      use simple_sph_spectr_head_IO
      use simple_sph_spectr_data_IO
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_write_mon = 16
!
      private :: select_read_sph_monitor_head, select_read_spectr_name
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine open_write_sph_monitor_file(id_file, file_name)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: file_name
!
!
      write(*,*) 'Open ASCII monitor file to write: ', trim(file_name)
      open(id_file, file = file_name)
!
      end subroutine open_write_sph_monitor_file
!
!   --------------------------------------------------------------------
!
      subroutine sel_open_read_sph_monitor_file                         &
     &         (FPz_f, id_file, file_name, flag_gzip, zbuf)
!
      use set_parallel_file_name
      use skip_comment_f
      use skip_gz_comment
!
      character, pointer, intent(inout) :: FPz_f
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: file_name
      logical, intent(inout) :: flag_gzip
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      character(len=kchara) :: prefix, extension
!
      call split_extrension(file_name, prefix, extension)
!
!
      flag_gzip = .FALSE.
#ifdef ZLIB_IO
      if(cmp_no_case(extension, gz_ext)) flag_gzip = .TRUE.
      if(flag_gzip) then
        write(*,*) 'read gzipped monitor file: ', trim(file_name)
        call open_rd_gzfile_a(FPz_f, file_name, zbuf)
        return
      end if
#endif
!
      write(*,*) 'read ASCII monitor file: ', trim(file_name)
      open(id_file, file = file_name, status='old')
!
      end subroutine sel_open_read_sph_monitor_file
!
!   --------------------------------------------------------------------
!
      subroutine sel_close_sph_monitor_file                             &
     &         (FPz_f, id_file, flag_gzip, zbuf)
!
      use skip_gz_comment
!
      character, pointer, intent(inout) :: FPz_f
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_gzip
      type(buffer_4_gzip), intent(inout) :: zbuf
!
#ifdef ZLIB_IO
      if(flag_gzip) then
        call close_gzfile_a(FPz_f, zbuf)
        return
      end if
#endif
!
      close(id_file)
!
      end subroutine sel_close_sph_monitor_file
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine select_input_sph_series_head(FPz_f, id_file,           &
     &          flag_gzip, flag_old_fmt, flag_spectr, flag_vol_ave,     &
     &          sph_IN, zbuf)
!
      use simple_sph_spectr_head_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_gzip, flag_old_fmt
      logical, intent(in) :: flag_spectr, flag_vol_ave
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      call select_num_of_labels_4_monitor                               &
     &   (flag_old_fmt, flag_spectr, flag_vol_ave, sph_IN)
      call select_read_sph_monitor_head                                 &
     &   (FPz_f, id_file, flag_gzip, flag_vol_ave, sph_IN, zbuf)
!
      call alloc_sph_espec_name(sph_IN)
      call select_read_spectr_name                                      &
     &   (FPz_f, id_file, flag_gzip, sph_IN, zbuf)
!
      if(flag_spectr .eqv. .FALSE.) sph_IN%ltr_sph = 0
      call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
!
      end subroutine select_input_sph_series_head
!
!   --------------------------------------------------------------------
!
      subroutine select_input_sph_series_data(FPz_f, id_file,           &
     &          flag_gzip, flag_old_fmt, flag_spectr, flag_vol_ave,     &
     &          sph_IN, zbuf, ierr)
!
      use gz_spl_sph_spectr_data_IO
      use simple_sph_spectr_data_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_gzip, flag_old_fmt
      logical, intent(in) :: flag_spectr, flag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
#ifdef ZLIB_IO
      if(flag_gzip) then
        call sel_gz_input_sph_series_data                               &
     &     (FPz_f, flag_old_fmt, flag_spectr, flag_vol_ave,             &
     &      sph_IN, zbuf, ierr)
        return
      end if
#endif
!
      call sel_input_sph_series_data(id_file, flag_old_fmt,             &
     &    flag_spectr, flag_vol_ave, sph_IN, ierr)
!
      end subroutine select_input_sph_series_data
!
!   --------------------------------------------------------------------
!
      subroutine select_read_sph_spectr_time(FPz_f, id_file,            &
     &          flag_gzip, nitem_snap, i_step, time, zbuf, ierr)
!
      use gz_spl_sph_spectr_data_IO
      use count_monitor_time_series
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: nitem_snap
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
#ifdef ZLIB_IO
      if(flag_gzip) then
        call gz_read_sph_spectr_time(FPz_f, nitem_snap, i_step, time,   &
     &                               zbuf, ierr)
        return
      end if
#endif
!
      call read_sph_spectr_time(id_file, nitem_snap,                    &
     &                          i_step, time, ierr)
!
      end subroutine select_read_sph_spectr_time
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine select_read_sph_monitor_head(FPz_f, id_file,           &
     &         flag_gzip, flag_vol_ave, sph_IN, zbuf)
!
      use gz_spl_sph_spectr_head_IO
      use simple_sph_spectr_head_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_gzip, flag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
#ifdef ZLIB_IO
      if(flag_gzip) then
        call sel_gz_read_sph_monitor_head                               &
     &     (FPz_f, flag_vol_ave, sph_IN, zbuf)
        return
      end if
#endif
!
      call sel_read_sph_monitor_head(id_file, flag_vol_ave, sph_IN)
!
      end subroutine select_read_sph_monitor_head
!
!   --------------------------------------------------------------------
!
      subroutine select_read_spectr_name(FPz_f, id_file, flag_gzip,     &
     &                                   sph_IN, zbuf)
!
      use gz_spl_sph_spectr_head_IO
      use simple_sph_spectr_head_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_gzip
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
#ifdef ZLIB_IO
      if(flag_gzip) then
        call gz_read_sph_spectr_name                                    &
     &     (FPz_f, sph_IN%nfield_sph_spec, sph_IN%num_labels,           &
     &      sph_IN%ncomp_sph_spec, sph_IN%ene_sph_spec_name, zbuf)
        return
      end if
#endif
!
      call read_sph_spectr_name                                         &
     &   (id_file, sph_IN%nfield_sph_spec, sph_IN%num_labels,           &
     &    sph_IN%ncomp_sph_spec, sph_IN%ene_sph_spec_name)
!
      end subroutine select_read_spectr_name
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine select_copy_sph_monitor_data                           &
     &         (FPz_f, id_read, id_write, flag_gzip,                    &
     &          n_line, nchara_line, textbuf, zbuf, ierr)
!
      use gz_spl_sph_spectr_data_IO
      use count_monitor_time_series
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_read, id_write
      integer(kind = kint), intent(in) :: nchara_line
      integer(kind = kint), intent(in) :: n_line
      logical, intent(in) :: flag_gzip
!
      character(len = 1), intent(inout) :: textbuf(nchara_line)
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: line
!
!
#ifdef ZLIB_IO
      if(flag_gzip) then
        do line = 1, n_line
          call gz_copy_spectr_monitor_data(FPz_f, id_write, zbuf, ierr)
          if(ierr .gt. 0) return
        end do
        return
      end if
#endif
!
      do line = 1, n_line
        call read_write_line_text(id_read, id_write,                    &
     &                            nchara_line, textbuf, ierr)
          if(ierr .gt. 0) return
      end do
!
      end subroutine select_copy_sph_monitor_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine select_num_of_labels_4_monitor                         &
     &         (flag_old_fmt, flag_spectr, flag_vol_ave, sph_IN)
!
      logical, intent(in) :: flag_old_fmt, flag_spectr, flag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      if(flag_vol_ave) then
        sph_IN%nri_sph = 1
        if(flag_spectr) then
          sph_IN%num_time_labels = 3
        else
          sph_IN%num_time_labels = 2
        end if
      else
        if(flag_spectr) then
          if(flag_old_fmt) then
            sph_IN%num_time_labels = 4
          else
            sph_IN%num_time_labels = 5
          end if
        else
          if(flag_old_fmt) then
            sph_IN%num_time_labels = 3
          else
            sph_IN%num_time_labels = 4
          end if
        end if
      end if
!
      end subroutine select_num_of_labels_4_monitor
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      integer(kind = kint) function lengh_spectr_data_line              &
     &                   (flag_spectr, flag_vol_ave, sph_IN)
!
      logical, intent(in) :: flag_spectr, flag_vol_ave
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: nchara_line
!
      if(flag_spectr) then
        if(flag_vol_ave) then
          nchara_line = sph_IN%ntot_sph_spec * 25 + 16+25+16
        else
          nchara_line = sph_IN%ntot_sph_spec * 25 + 16+25+16+25+16
        end if
      else
        if(flag_vol_ave) then
          nchara_line = sph_IN%ntot_sph_spec * 25 + 16+25
        else
          nchara_line = sph_IN%ntot_sph_spec * 25 + 16+25+16+25
        end if
      end if
      lengh_spectr_data_line = nchara_line
!
      end function lengh_spectr_data_line
!
!   --------------------------------------------------------------------
!
      end module sph_mean_square_IO
