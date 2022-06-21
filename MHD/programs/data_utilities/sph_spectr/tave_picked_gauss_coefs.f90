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
      use set_parallel_file_name
      use count_monitor_time_series
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
      character(len=kchara) :: input_file_name, file_prefix
      character(len=kchara) :: tave_pick_gauss_fname
      character(len=kchara) :: trms_pick_gauss_fname
      character(len=kchara) :: sdev_pick_gauss_fname
!
      integer(kind = kint) :: i_step, ierr, icou, ipick
      real(kind = kreal) :: acou, time, prev_time
      real(kind = kreal) :: start_time, end_time, true_start, true_end
!
!
      call read_control_file_sph_monitor(0, tave_sph_ctl1)
!
      if(tave_sph_ctl1%gauss_coefs_prefix%iflag .eq. 0) then
        write(*,*) 'Set File prefix for Gauss coefficients'
        stop
      end if
      file_prefix = tave_sph_ctl1%gauss_coefs_prefix%charavalue
      input_file_name = add_dat_extension(file_prefix)
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
      write(tave_pick_gauss_fname,'(a6,a)')                             &
     &                           't_ave_',  trim(input_file_name)
      write(trms_pick_gauss_fname,'(a6,a)')                             &
     &                           't_rms_',  trim(input_file_name)
      write(sdev_pick_gauss_fname,'(a8,a)')                             &
     &                           't_sigma_', trim(input_file_name)
!
!       Open data file
      call check_gauss_coefs_time_series(input_file_name, gauss_IO)
      call load_gauss_coefs_time_series                                 &
     &   (.TRUE., input_file_name, start_time, end_time,                &
     &    true_start, true_end, gauss_IO)
!
      allocate(ave_gauss(gauss_IO%num_mode))
      allocate(rms_gauss(gauss_IO%num_mode))
      allocate(sdev_gauss(gauss_IO%num_mode))
      allocate(prev_gauss(gauss_IO%num_mode))
!
      call cal_time_ave_picked_sph_spectr                               &
     &   (gauss_IO%n_step, gauss_IO%d_time, gauss_IO%num_mode,          &
     &    gauss_IO%d_gauss, ave_gauss, rms_gauss, sdev_gauss)
!
!
      do icou = 1, gauss_IO%num_mode
        write(*,*) icou, ave_gauss(icou), rms_gauss(icou),              &
     &           sdev_gauss(icou), trim(gauss_IO%gauss_coef_name(icou))
      end do
!
!  Output time average
!$omp parallel workshare
      gauss_IO%gauss_coef(1:gauss_IO%num_mode)                          &
     &      = ave_gauss(1:gauss_IO%num_mode)
!$omp end parallel workshare
!
      gauss_IO%gauss_coef_file_name = tave_pick_gauss_fname
      call write_gauss_coefs_4_monitor(0, i_step, time, gauss_IO)
!
!  Output time average
!$omp parallel workshare
      gauss_IO%gauss_coef(1:gauss_IO%num_mode)                          &
     &      = rms_gauss(1:gauss_IO%num_mode)
!$omp end parallel workshare
!
      gauss_IO%gauss_coef_file_name = trms_pick_gauss_fname
      call write_gauss_coefs_4_monitor(0, i_step, time, gauss_IO)
!
!  Output time average
!$omp parallel workshare
      gauss_IO%gauss_coef(1:gauss_IO%num_mode)                          &
     &      = sdev_gauss(1:gauss_IO%num_mode)
!$omp end parallel workshare
!
      gauss_IO%gauss_coef_file_name = sdev_pick_gauss_fname
      call write_gauss_coefs_4_monitor(0, i_step, time, gauss_IO)
!
      deallocate(ave_gauss, rms_gauss, sdev_gauss)
      call dealloc_gauss_coef_monitor(gauss_IO)
      call dealloc_gauss_coefs_series(gauss_IO)
!
      write(*,*) '***** program finished *****'
      stop
!
      end program tave_picked_gauss_coefs
