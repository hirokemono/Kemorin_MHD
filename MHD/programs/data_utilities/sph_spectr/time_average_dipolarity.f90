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
      use t_read_sph_spectra
      use t_CMB_dipolarity
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use sph_monitor_data_text
      use gz_open_sph_monitor_file
!
      character(len=kchara), intent(in) :: file_name
      real(kind = kreal), intent(in) :: start_time, end_time

      type(dipolarity_data), save :: dip_t
!
      real(kind = kreal), allocatable :: prev_fdip(:)
      real(kind = kreal), allocatable :: ave_fdip(:)
      real(kind = kreal), allocatable :: sdev_fdip(:)
      integer(kind = kint), parameter :: id_dipolarity = 15
!
      logical :: flag_gzip1
      type(buffer_4_gzip), save :: zbuf1
      character, pointer, save  :: FPz_f1
      type(read_sph_spectr_data), save :: sph_IN1
      type(sph_spectr_head_labels), save :: sph_lbl_IN1
!
      integer(kind = kint) :: i_step, icou, i
      real(kind = kreal) :: acou, time, prev_time, radius_CMB
      real(kind = kreal) :: true_start
      character(len = kchara) :: tmpchara, ave_file_name
!
      logical :: flag_gzip_lc
      type(read_sph_spectr_data) :: sph_OUT_d
!
!
      dip_t%dipolarity_file_name = file_name
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_dipolarity, dip_t%dipolarity_file_name,            &
     &    flag_gzip1, zbuf1)
      call s_select_input_sph_series_head(FPz_f1, id_dipolarity,        &
     &    flag_gzip1, flag_current_fmt, spectr_off, volume_on,          &
     &    sph_lbl_IN1, sph_IN1, zbuf1)
!
      sph_IN1%nri_dat = 1
      call alloc_sph_spectr_data(izero, sph_IN1)
!
      dip_t%krms_CMB = sph_IN1%kr_outer
      dip_t%rdip_CMB = sph_IN1%r_outer
      call alloc_dipolarity_data(sph_IN1%ntot_sph_spec, dip_t)
!
      dip_t%dip_name(1:dip_t%num_dip)                                   &
     &        = sph_IN1%ene_sph_spec_name(1:dip_t%num_dip)
      do i = 1, dip_t%num_dip
        write(tmpchara,'(a)')                                           &
     &          dip_t%dip_name(i)(len(dip_ltr_label)+1:kchara)
        read(tmpchara,*) dip_t%ltr_max(i)
        write(*,*) 'Truncatin: ', dip_t%ltr_max(i),                     &
     &                           trim(dip_t%dip_name(i))
      end do
      call dealloc_sph_espec_data(sph_IN1)
      call dealloc_sph_espec_name(sph_IN1)
!
      allocate(prev_fdip(dip_t%num_dip))
      allocate(ave_fdip(dip_t%num_dip))
      prev_fdip(1:dip_t%num_dip) = 0.0d0
      ave_fdip(1:dip_t%num_dip) = 0.0d0
!
!
!       Evaluate time average
!
      icou = 0
      time = start_time
      prev_time = start_time
      ave_fdip(1:dip_t%num_dip) = 0.0d0
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', i_step,  ' averaging finished. Count=  ', icou
      do
        call sel_read_line_gz_stream(FPz_f1, id_dipolarity,             &
     &                               flag_gzip1, zbuf1)
        read(zbuf1%fixbuf(1),*,err=99) i_step, time,                    &
     &                                dip_t%f_dip(1:dip_t%num_dip)
        if(time .ge. start_time) then
!
          if(icou .eq. 0) then
            true_start = time
            prev_time = time
            prev_fdip(1:dip_t%num_dip) = dip_t%f_dip(1:dip_t%num_dip)
          else
            ave_fdip(1:dip_t%num_dip) = ave_fdip(1:dip_t%num_dip)       &
     &                           + half*(dip_t%f_dip(1:dip_t%num_dip)   &
     &                                   + prev_fdip(1:dip_t%num_dip))  &
     &                            * (time - prev_time)
          end if
!
          icou = icou + 1
        end if
        prev_time = time
        prev_fdip(1:dip_t%num_dip) = dip_t%f_dip(1:dip_t%num_dip)
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
      ave_fdip(1:dip_t%num_dip) = ave_fdip(1:dip_t%num_dip) * acou
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
      sdev_fdip(1:dip_t%num_dip) = 0.0d0
      do
        call sel_read_line_gz_stream(FPz_f1, id_dipolarity,             &
     &                               flag_gzip1, zbuf1)
        read(zbuf1%fixbuf(1),*,err=98) i_step, time,                    &
     &                                dip_t%f_dip(1:dip_t%num_dip)
!
        if(time .ge. start_time) then
!
          if(icou .eq. 0) then
            true_start = time
            prev_time = time
            prev_fdip(1:dip_t%num_dip) = dip_t%f_dip
          else
            sdev_fdip(1:dip_t%num_dip) = sdev_fdip(1:dip_t%num_dip)     &
     &                          + half*((dip_t%f_dip(1:dip_t%num_dip)   &
     &                                  - ave_fdip(1:dip_t%num_dip))**2 &
     &                           + prev_fdip(1:dip_t%num_dip))          &
     &                          * (time - prev_time)
          end if
!
          icou = icou + 1
        end if
        prev_time = time
        prev_fdip(1:dip_t%num_dip) = (dip_t%f_dip(1:dip_t%num_dip)      &
     &                              - ave_fdip(1:dip_t%num_dip))**2
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
      sdev_fdip(1:dip_t%num_dip)                                        &
     &     = sqrt(sdev_fdip(1:dip_t%num_dip) * acou)
!
!    output Results
!
      write(*,'(a,1p2e25.15e3)') 'Start and end time:     ',            &
     &                          true_start, end_time
      write(*,'(a)') 'Average and Std. Dev. of dipolarity at CMB'
      do i = 1, dip_t%num_dip
        write(*,'(a,a4,1p2e25.15e3)') trim(dip_t%dip_name(i)), ':   ',  &
     &                               ave_fdip(i), sdev_fdip(i)
      end do
!
!
      flag_gzip_lc = .FALSE.
      write(ave_file_name,'(a4,a)')                                     &
     &                   't_ave_', trim(dip_t%dipolarity_file_name)
      call dup_dipolarity_header_to_IO                                  &
     &   (sph_IN1%ltr_sph, sph_IN1%nri_sph,                             &
     &    sph_IN1%kr_ICB, sph_IN1%kr_CMB, dip_t, sph_OUT_d)
      call sel_open_sph_vol_monitor_file(id_dipolarity, ave_file_name,  &
     &    sph_dipolarity_labels, sph_OUT_d, zbuf1, flag_gzip_lc)
      call dealloc_sph_espec_name(sph_OUT_d)
!
      call sel_gz_write_text_stream(flag_gzip_lc, id_dipolarity,        &
     &    volume_pwr_data_text(i_step, time, dip_t%num_dip, ave_fdip),  &
     &    zbuf1)
      call sel_gz_write_text_stream(flag_gzip_lc, id_dipolarity,        &
     &    volume_pwr_data_text(i_step, time, dip_t%num_dip, sdev_fdip), &
     &    zbuf1)
!
      close(id_dipolarity)
!
      end subroutine s_time_average_dipolarity
!
! -------------------------------------------------------------------
!
      end module time_average_dipolarity
