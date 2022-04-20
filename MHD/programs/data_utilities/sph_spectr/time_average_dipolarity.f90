!time_average_dipolarity.f90
!      program time_average_dipolarity
!
!        programmed by H.Matsui on Apr., 2014
!
!
!! -----------------------------------------------------------------
!!    Input control file:  control_sph_time_average
!!
!!  begin time_averaging_sph_monitor
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    dipolarity_prefix    'dipolarity'
!!  end time_averaging_sph_monitor
!! -----------------------------------------------------------------
!

      program time_average_dipolarity
!
      use m_precision
      use m_constants
!
      use t_CMB_dipolarity
      use t_ctl_data_tave_sph_monitor
      use set_parallel_file_name
!
      implicit  none
!
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
      type(dipolarity_data), save :: dip_t
!
      real(kind = kreal) :: prev_fdip = 0.0d0
      real(kind = kreal) :: ave_fdip, sdev_fdip
      integer(kind = kint), parameter :: id_dipolarity = 15
!
      integer(kind = kint) :: i_step, icou, i
      real(kind = kreal) :: acou, time, prev_time, radius_CMB
      real(kind = kreal) :: start_time, end_time, true_start
      character(len = kchara) :: file_name, tmpchara
!
!
      write(*,*) '-----------------------------------------------'
      write(*,*) '                  CAUTION!!'
      write(*,*) 'This program can evaluate dipolarity'
      write(*,*) 'only when there is NO internal heat source'
      write(*,*) 'in the outer core!!'
      write(*,*) '-----------------------------------------------'
      write(*,*) ''
      write(*,*) 'Input picked harmonics coefficients file prefix'
!
      call read_control_file_sph_monitor(0, tave_sph_ctl1)
!
      if(tave_sph_ctl1%dipolarity_file_prefix%iflag .eq. 0) then
        write(*,*) 'Set File prefix for dipolarity'
        stop
      end if
      dip_t%dipolarity_prefix                                           &
     &      = tave_sph_ctl1%dipolarity_file_prefix%charavalue
!
      if(tave_sph_ctl1%start_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set start time'
        stop
      end if
      start_time = tave_sph_ctl1%start_time_ctl%realvalue
!
      if(tave_sph_ctl1%end_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set end time'
        stop
      end if
      end_time = tave_sph_ctl1%end_time_ctl%realvalue
!
!       Open Nusselt data file
!
      file_name = add_dat_extension(dip_t%dipolarity_prefix)
      open(id_dipolarity, file = file_name,                             &
     &     form='formatted', status='old')
      read(id_dipolarity,*)  tmpchara
      read(id_dipolarity,*)  dip_t%ltr_max, radius_CMB
      read(id_dipolarity,*)  tmpchara
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
        read(id_dipolarity,*,err=99) i_step, time, dip_t%f_dip
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
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', i_step,  ' averaging finished. Count=   ', icou
        if(time .ge. end_time) exit
      end do
  99  continue
      write(*,*)
      close(id_dipolarity)
!
      acou = one / (time - true_start)
      ave_fdip = ave_fdip * acou
!
!       Evaluate standard deviation
!
      file_name = add_dat_extension(dip_t%dipolarity_prefix)
      open(id_dipolarity, file = file_name,                             &
     &     form='formatted', status='old')
      read(id_dipolarity,*)  tmpchara
      read(id_dipolarity,*)  dip_t%ltr_max, radius_CMB
      read(id_dipolarity,*)  tmpchara
!
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', i_step,  ' deviation finished. Count=  ', icou
      icou = 0
      time = start_time
      prev_time = start_time
      sdev_fdip = 0.0d0
      do
        read(id_dipolarity,*,err=98) i_step, time, dip_t%f_dip
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
      close(id_dipolarity)
!
      acou = one / (time - true_start)
      sdev_fdip = sqrt(sdev_fdip * acou)
!
!    output Results
!
      write(*,'(a,1p2e25.15e3)') 'Start and end time:     ',            &
     &                          true_start, end_time
      write(*,'(a)') 'Average and Std. Dev. of Nu at CMB:'
      write(*,'(1p2e25.15e3)')  ave_fdip, sdev_fdip
!
      write(*,*) '***** program finished *****'
      stop
!
      end program time_average_dipolarity
