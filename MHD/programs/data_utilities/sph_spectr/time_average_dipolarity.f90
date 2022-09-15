!>@file   time_average_dipolarity.f90
!!@brief  module time_average_dipolarity
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Top subroutines for time averaging of dipolarity file
!!
!!@verbatim
!!      integer(c_int) function                                         &
!!    &     time_average_dipolarity_f(cname, cstart, cend) Bind(C)
!!        character(1,C_char), intent(in) :: cname(*)
!!        real(C_double), Value :: cstart, cend
!!
!!      subroutine s_time_average_dipolarity                            &
!!     &         (file_name, start_time, end_time)
!!        character(len=kchara), intent(in) :: file_name
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!@endverbatim
!
      module time_average_dipolarity
!
      use ISO_C_BINDING
      use m_precision
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     time_average_dipolarity_f(cname, cstart, cend) Bind(C)
!
      use count_monitor_time_series
!
      character(1,C_char), intent(in) :: cname(*)
      real(C_double), Value :: cstart, cend
!
      real(kind = kreal) :: start_time, end_time
      character(len=kchara) :: file_name
!
      write(file_name,'(a)') trim(c_to_fstring(cname))
      start_time = cstart
      end_time = cend
      call s_time_average_dipolarity(file_name, start_time, end_time)
!
      time_average_dipolarity_f = 0
      end function time_average_dipolarity_f
!
! -------------------------------------------------------------------
!
      subroutine s_time_average_dipolarity                              &
     &         (file_name, start_time, end_time)
!
      use t_buffer_4_gzip
      use t_CMB_dipolarity
      use select_gz_stream_file_IO
!
      character(len=kchara), intent(in) :: file_name
      real(kind = kreal), intent(in) :: start_time, end_time

      type(dipolarity_data), save :: dip_t
!
      real(kind = kreal) :: prev_fdip = 0.0d0
      real(kind = kreal) :: ave_fdip, sdev_fdip
      integer(kind = kint), parameter :: id_dipolarity = 15
!
      logical :: flag_gzip1
      type(buffer_4_gzip), save :: zbuf1
      character, pointer, save  :: FPz_f1
!
      integer(kind = kint) :: i_step, icou, i
      real(kind = kreal) :: acou, time, prev_time, radius_CMB
      real(kind = kreal) :: true_start
      character(len = kchara) :: tmpchara
!
!
      dip_t%dipolarity_file_name = file_name
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_dipolarity, dip_t%dipolarity_file_name,            &
     &    flag_gzip1, zbuf1)
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f1, id_dipolarity, flag_gzip1, zbuf1)
      read(zbuf1%fixbuf(1),*)  dip_t%ltr_max, radius_CMB
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f1, id_dipolarity, flag_gzip1, zbuf1)
      read(zbuf1%fixbuf(1),*)  tmpchara
!
!       Evaluate time average
!
      icou = 0
      time = start_time
      prev_time = start_time
      ave_fdip = 0.0d0
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', i_step,  ' averaging finished. Count=  ', icou
      do
        call sel_read_line_gz_stream(FPz_f1, id_dipolarity,             &
     &                               flag_gzip1, zbuf1)
        read(zbuf1%fixbuf(1),*,err=99) i_step, time, dip_t%f_dip
        if(time .ge. start_time) then
!
          if(icou .eq. 0) then
            true_start = time
            prev_time = time
            prev_fdip = dip_t%f_dip
          else
            ave_fdip = ave_fdip + half*(dip_t%f_dip + prev_fdip)        &
     &                 * (time - prev_time)
          end if
!
          icou = icou + 1
        end if
        prev_time = time
        prev_fdip = dip_t%f_dip
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59),&
     &       'step= ', i_step,  ' averaging finished. Count=   ', icou
        if(time .ge. end_time) exit
      end do
  99  continue
      write(*,*)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_dipolarity, flag_gzip1, zbuf1)
!
      acou = one / (time - true_start)
      ave_fdip = ave_fdip * acou
!
!       Evaluate standard deviation
!
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_dipolarity, dip_t%dipolarity_file_name,            &
     &    flag_gzip1, zbuf1)
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f1, id_dipolarity, flag_gzip1, zbuf1)
      read(zbuf1%fixbuf(1),*)  dip_t%ltr_max, radius_CMB
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f1, id_dipolarity, flag_gzip1, zbuf1)
      read(zbuf1%fixbuf(1),*)  tmpchara
!
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', i_step,  ' deviation finished. Count=  ', icou
      icou = 0
      time = start_time
      prev_time = start_time
      sdev_fdip = 0.0d0
      do
        call sel_read_line_gz_stream(FPz_f1, id_dipolarity,             &
     &                               flag_gzip1, zbuf1)
        read(zbuf1%fixbuf(1),*,err=99) i_step, time, dip_t%f_dip
!
        if(time .ge. start_time) then
!
          if(icou .eq. 0) then
            true_start = time
            prev_time = time
            prev_fdip = dip_t%f_dip
          else
            sdev_fdip = sdev_fdip                                       &
     &           + half*( (dip_t%f_dip - ave_fdip)**2 + prev_fdip)      &
     &                   * (time - prev_time)
          end if
!
          icou = icou + 1
        end if
        prev_time = time
        prev_fdip = (dip_t%f_dip - ave_fdip)**2
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', i_step,  ' deviation finished. Count=   ', icou
        if(time .ge. end_time) exit
      end do
  98  continue
      write(*,*)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_dipolarity, flag_gzip1, zbuf1)
!
      acou = one / (time - true_start)
      sdev_fdip = sqrt(sdev_fdip * acou)
!
!    output Results
!
      write(*,'(a,1p2e25.15e3)') 'Start and end time:     ',            &
     &                          true_start, end_time
      write(*,'(a)') 'Average and Std. Dev. of dipolarity at CMB'
      write(*,'(1p2e25.15e3)')  ave_fdip, sdev_fdip
!
      end subroutine s_time_average_dipolarity
!
! -------------------------------------------------------------------
!
      end module time_average_dipolarity
