!tave_picked_gauss_coefs.f90
!
!        programmed by H.Matsui on Dec., 2012
!
!! -----------------------------------------------------------------
!!    Input control file:  control_sph_time_average
!!
!!  begin time_averaging_sph_monitor
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    gauss_coefs_prefix        'gauss_coefs_Re'
!!  end time_averaging_sph_monitor
!! -----------------------------------------------------------------
!
      program tave_picked_gauss_coefs
!
      use m_precision
      use m_constants
!
      use t_gauss_coefs_monitor_IO
      use t_ctl_data_tave_sph_monitor
!
      implicit  none
!
!>      Structure for control data
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
!>      Structure for gauss coeffciients
      type(picked_gauss_coefs_IO), save :: gauss_IO
!
      real(kind = kreal), allocatable :: ave_gauss(:)
      real(kind = kreal), allocatable :: rms_gauss(:)
      real(kind = kreal), allocatable :: sdev_gauss(:)
      real(kind = kreal), allocatable :: prev_gauss(:)
!
      character(len=kchara) :: tave_pick_gauss_head
      character(len=kchara) :: trms_pick_gauss_head
      character(len=kchara) :: sdev_pick_gauss_head
      integer(kind = kint), parameter :: id_pick = 15
!
      integer(kind = kint) :: i_step, ierr, icou, ipick
      real(kind = kreal) :: acou, time, prev_time
      real(kind = kreal) :: start_time, end_time, true_start
!
!
      call read_control_file_psf_compare(0, tave_sph_ctl1)
!
      if(tave_sph_ctl1%gauss_coefs_prefix%iflag .eq. 0) then
        write(*,*) 'Set File prefix for Gauss coefficients'
        stop
      end if
      gauss_IO%file_prefix                                              &
     &      = tave_sph_ctl1%gauss_coefs_prefix%charavalue
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
      write(tave_pick_gauss_head,'(a6,a)')                              &
        't_ave_',  trim(gauss_IO%file_prefix)
      write(trms_pick_gauss_head,'(a6,a)')                              &
        't_rms_',  trim(gauss_IO%file_prefix)
      write(sdev_pick_gauss_head,'(a6,a)')                              &
        't_sdev_', trim(gauss_IO%file_prefix)
!
!       Open Nusselt data file
!
      call open_gauss_coefs_read_monitor(id_pick, gauss_IO)
!
      allocate(ave_gauss(gauss_IO%num_mode))
      allocate(rms_gauss(gauss_IO%num_mode))
      allocate(sdev_gauss(gauss_IO%num_mode))
      allocate(prev_gauss(gauss_IO%num_mode))
!
!$omp parallel workshare
      ave_gauss = 0.0d0
      rms_gauss = 0.0d0
      sdev_gauss = 0.0d0
      prev_gauss = 0.0d0
!$omp end parallel workshare
!
!
      icou = 0
      true_start = start_time
      prev_time =  start_time
      do
        call read_gauss_coefs_4_monitor                                 &
     &     (id_pick, i_step, time, gauss_IO, ierr)
        if(ierr .gt. 0) exit
!
!
        if(time .ge. start_time) then
          if(icou .eq. 0) then
            true_start = time
          else
!$omp parallel do
            do ipick = 1, gauss_IO%num_mode
              ave_gauss(ipick) = ave_gauss(ipick) + half                &
     &         * (gauss_IO%gauss_coef(ipick) + prev_gauss(ipick))       &
     &         * (time - prev_time)
              rms_gauss(ipick) = rms_gauss(ipick) + half                &
     &         * (gauss_IO%gauss_coef(ipick)**2 + prev_gauss(ipick)**2) &
     &         * (time - prev_time)
            end do
!$omp end parallel do
          end if
!
!$omp parallel do
          do ipick = 1, gauss_IO%num_mode
            prev_gauss(ipick) = gauss_IO%gauss_coef(ipick)
          end do
!$omp end parallel do
!
          icou = icou + 1
          write(*,*) 'step ', i_step,                                   &
     &        ' is added for time average: count is  ', icou, time
        end if
        prev_time = time
!
        if(time .ge. end_time) exit
      end do
      close(id_pick)
!
      acou = one / (time - true_start)
!$omp parallel do
      do ipick = 1, gauss_IO%num_mode
        sdev_gauss(ipick) = rms_gauss(ipick) - ave_gauss(ipick)**2
!
        ave_gauss(ipick) =  ave_gauss(ipick) * acou
        rms_gauss(ipick) =   sqrt(rms_gauss(ipick) * acou)
        sdev_gauss(ipick) =  sqrt(sdev_gauss(ipick) * acou)
      end do
!$omp end parallel do
      call dealloc_gauss_coef_monitor(gauss_IO)
!
!  Output time average
!$omp parallel workshare
      gauss_IO%gauss_coef(1:gauss_IO%num_mode)                          &
     &      = ave_gauss(1:gauss_IO%num_mode)
!$omp end parallel workshare
!
      gauss_IO%file_prefix = tave_pick_gauss_head
      call write_gauss_coefs_4_monitor(0, i_step, time, gauss_IO)
!
!  Output time average
!$omp parallel workshare
      gauss_IO%gauss_coef(1:gauss_IO%num_mode)                          &
     &      = rms_gauss(1:gauss_IO%num_mode)
!$omp end parallel workshare
!
      gauss_IO%file_prefix = trms_pick_gauss_head
      call write_gauss_coefs_4_monitor(0, i_step, time, gauss_IO)
!
!  Output time average
!$omp parallel workshare
      gauss_IO%gauss_coef(1:gauss_IO%num_mode)                          &
     &      = sdev_gauss(1:gauss_IO%num_mode)
!$omp end parallel workshare
!
      gauss_IO%file_prefix = sdev_pick_gauss_head
      call write_gauss_coefs_4_monitor(0, i_step, time, gauss_IO)
!
      deallocate(ave_gauss, rms_gauss, sdev_gauss)
!
      write(*,*) '***** program finished *****'
      stop
!
      end program tave_picked_gauss_coefs
