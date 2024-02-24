!>@file   gz_gauss_coefs_monitor_IO.f90
!!@brief  module gz_gauss_coefs_monitor_IO
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays for gauss coefficients output
!!
!!@verbatim
!!      character, pointer, intent(inout) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(inout) :: flag_gzip
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!!
!!      subroutine check_gauss_coefs_time_series(file_name, gauss_IO)
!!      subroutine load_gauss_coefs_time_series                         &
!!     &         (flag_log, file_name, start_time, end_time,            &
!!     &          true_start, true_end, gauss_IO)
!!      subroutine read_gauss_coefs_header(FPz_f, id_stream, flag_gzip, &
!!     &                                   gauss_IO, zbuf)
!!      subroutine read_gauss_coefs_labels(FPz_f, id_stream, flag_gzip, &
!!     &                                   gauss_IO, zbuf)
!!        logical, intent(in) :: flag_log
!!        character(len=kchara), intent(in) :: file_name
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!        real(kind = kreal), intent(inout) :: true_start, true_end
!!        type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!!
!!      subroutine read_gauss_coefs_4_monitor                           &
!!     &         (FPz_f, id_stream, flag_gzip, i_step, time,            &
!!     &          gauss_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(inout) :: i_step, ierr
!!        real(kind = kreal), intent(inout) :: time
!!        type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
      module gz_gauss_coefs_monitor_IO
!
      use m_precision
      use m_constants
      use t_gauss_coefs_monitor_IO
      use t_buffer_4_gzip
!
      implicit  none
!
!>      File ID for Gauss coefficients IO
      integer(kind = kint), parameter :: id_gauss_coef = 23
!
      private :: read_gauss_coefs_series
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_gauss_coefs_time_series(file_name, gauss_IO)
!
      use select_gz_stream_file_IO
      use count_monitor_time_series
!
      character(len=kchara), intent(in) :: file_name
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
      logical :: flag_gzip1
      type(buffer_4_gzip), save :: zbuf1
      character, pointer, save  :: FPz_f1
!
      integer(kind = kint) :: i
      integer(kind = kint) :: i_start, i_end
      real(kind = kreal) :: start_time, end_time
!
!
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_gauss_coef, file_name, flag_gzip1, zbuf1)
      call read_gauss_coefs_header(FPz_f1, id_gauss_coef,               &
     &                             flag_gzip1, gauss_IO, zbuf1)
      call alloc_gauss_coef_monitor(gauss_IO)
      call read_gauss_coefs_labels(FPz_f1, id_gauss_coef,               &
     &                             flag_gzip1, gauss_IO, zbuf1)
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f1, id_gauss_coef, flag_gzip1, zbuf1)
      read(zbuf1%fixbuf(1),*) i_start, start_time
!
      do
        call sel_skip_comment_gz_stream                                 &
     &     (FPz_f1, id_gauss_coef, flag_gzip1, zbuf1)
        if(zbuf1%len_used .lt. 0) exit
        read(zbuf1%fixbuf(1),*) i_end, end_time
      end do
!
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_gauss_coef, flag_gzip1, zbuf1)
!
      write(*,*) 'Start step and time: ', i_start, start_time
      write(*,*) 'End step and time: ', i_end, end_time
!
      write(*,*) 'Saved Gauss coefficients at r = ',                    &
     &          gauss_IO%radius_gauss, ': '
      do i = 1, gauss_IO%num_mode
        write(*,*) i, trim(gauss_IO%gauss_coef_name(i))
      end do
!
      call dealloc_gauss_coef_monitor(gauss_IO)
!
      end subroutine check_gauss_coefs_time_series
!
! -----------------------------------------------------------------------
!
      subroutine load_gauss_coefs_time_series                           &
     &         (flag_log, file_name, start_time, end_time,              &
     &          true_start, true_end, gauss_IO, sph_IN_g, g_series)
!
      use t_sph_volume_mean_series
      use gzip_file_access
      use select_gz_stream_file_IO
      use count_monitor_time_series
!
      logical, intent(in) :: flag_log
      character(len=kchara), intent(in) :: file_name
      real(kind = kreal), intent(in) :: start_time, end_time
!
      real(kind = kreal), intent(inout) :: true_start, true_end
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
      type(read_sph_spectr_data), intent(inout) :: sph_IN_g
      type(sph_volume_mean_series), intent(inout) :: g_series
!
      logical :: flag_gzip1
      type(buffer_4_gzip), save :: zbuf1
      character, pointer, save  :: FPz_f1
!
      integer(kind = kint) :: num_count, icou_skip, ierr
      integer(kind = kint) :: i
!
!
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_gauss_coef, file_name, flag_gzip1, zbuf1)
      call read_gauss_coefs_header(FPz_f1, id_gauss_coef,               &
     &                             flag_gzip1, gauss_IO, zbuf1)
      call alloc_gauss_coef_monitor(gauss_IO)
      call read_gauss_coefs_labels(FPz_f1, id_gauss_coef,               &
     &                             flag_gzip1, gauss_IO, zbuf1)
!
      call s_count_monitor_time_series                                  &
     &   (flag_log, FPz_f1,id_gauss_coef, flag_gzip1, ione,             &
     &    start_time, end_time, true_start, true_end,                   &
     &    num_count, icou_skip, zbuf1)
      if(flag_gzip1) then
        ierr =  rewind_gzfile(FPz_f1)
      else
        rewind(id_gauss_coef)
      end if
!
      call read_gauss_coefs_header(FPz_f1, id_gauss_coef,               &
     &                             flag_gzip1, gauss_IO, zbuf1)
      call read_gauss_coefs_labels(FPz_f1, id_gauss_coef,               &
     &                             flag_gzip1, gauss_IO, zbuf1)
!
      call alloc_gauss_coefs_series(num_count, gauss_IO)
      call read_gauss_coefs_series(flag_log, FPz_f1, id_gauss_coef,     &
     &    flag_gzip1, start_time, end_time, gauss_IO, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_gauss_coef, flag_gzip1, zbuf1)
!
      write(*,*) 'Start step and time: ',                               &
     &           true_start, true_end, num_count
!
!
      sph_IN_g%kr_inner = 0
      sph_IN_g%kr_outer = 0
      sph_IN_g%r_inner =  0.0d0
      sph_IN_g%r_outer =  gauss_IO%radius_gauss
!
      sph_IN_g%nfield_sph_spec =  gauss_IO%num_mode
      sph_IN_g%ntot_sph_spec =    gauss_IO%num_mode
      sph_IN_g%num_time_labels =  2
      call alloc_sph_espec_name(sph_IN_g)
      sph_IN_g%ncomp_sph_spec(1:sph_IN_g%nfield_sph_spec) = 1
      do i = 1, sph_IN_g%nfield_sph_spec
        sph_IN_g%ene_sph_spec_name(i) = gauss_IO%gauss_coef_name(i)
      end do

      g_series%n_step =    gauss_IO%n_step
      g_series%ntot_comp = gauss_IO%num_mode
!
      end subroutine load_gauss_coefs_time_series
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_gauss_coefs_series                                &
     &         (flag_log, FPz_f, id_stream, flag_gzip,                  &
     &          start_time, end_time, gauss_IO, zbuf)
!
      use count_monitor_time_series
!
      logical, intent(in) :: flag_log
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      real(kind = kreal), intent(in) :: start_time, end_time
!
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: icou, i_step, ierr, i
      real(kind = kreal) :: time
!
      icou = 0
      do
        call read_gauss_coefs_4_monitor(FPz_f, id_stream, flag_gzip,    &
     &      i_step, time, gauss_IO, zbuf, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          icou = icou + 1
          call copy_to_gauss_coefs_series(icou, i_step, time, gauss_IO)
!
          if(flag_log) then
            write(*,'(69a1,a5,i12,a4,1pe16.8e3,a20,i12)',advance="NO")  &
     &          (char(8),i=1,69), 'step ', i_step,                      &
     &          ' at ', time, ' is read. count is  ', icou
          end if
        end if
!
        if(time .ge. end_time) exit
      end do
      if(flag_log) write(*,*)
!
      end subroutine read_gauss_coefs_series
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_gauss_coefs_header(FPz_f, id_stream, flag_gzip,   &
     &                                   gauss_IO, zbuf)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
!
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      gauss_IO%radius_gauss = 2.82
      call sel_skip_comment_gz_stream(FPz_f, id_stream,                 &
     &                                flag_gzip, zbuf)
      call sel_skip_comment_gz_stream(FPz_f, id_stream,                 &
     &                                flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) gauss_IO%num_mode, gauss_IO%radius_gauss
      write(*,*) 'gauss_IO%num_mode, gauss_IO%radius_gauss', &
     &     gauss_IO%num_mode, gauss_IO%radius_gauss
!
      end subroutine read_gauss_coefs_header
!
! -----------------------------------------------------------------------
!
      subroutine read_gauss_coefs_labels(FPz_f, id_stream, flag_gzip,   &
     &                                   gauss_IO, zbuf)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
!
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      character(len=255) :: tmpchara
!
      call sel_skip_comment_gz_stream(FPz_f, id_stream,                 &
     &                                flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) tmpchara, tmpchara,                        &
     &               gauss_IO%gauss_coef_name(1:gauss_IO%num_mode)
!
      end subroutine read_gauss_coefs_labels
!
! -----------------------------------------------------------------------
!
      subroutine read_gauss_coefs_4_monitor                             &
     &         (FPz_f, id_stream, flag_gzip, i_step, time,              &
     &          gauss_IO, zbuf, ierr)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
!
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      ierr = 1
      call sel_read_line_gz_stream(FPz_f, id_stream,                    &
     &                             flag_gzip, zbuf)
      if(zbuf%len_used .lt. 0) return
!
      read(zbuf%fixbuf(1),*,err=99,end=99) i_step, time,                &
     &                   gauss_IO%gauss_coef(1:gauss_IO%num_mode)
      ierr = 0
      return
!
   99 continue
      return
!
      end subroutine read_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
!
      end module gz_gauss_coefs_monitor_IO
