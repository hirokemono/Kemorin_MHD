!tave_picked_gauss_coefs.f90
!
!        programmed by H.Matsui on Dec., 2012
!
      program tave_picked_gauss_coefs
!
      use m_precision
      use m_constants
!
      use t_gauss_coefs_monitor_IO
!
      implicit  none
!
!>      Structure for gauss coeffciients
      type(picked_gauss_coefs_IO), save :: gauss_IO
!
      real(kind = kreal), allocatable :: ave_gauss(:)
      real(kind = kreal), allocatable :: rms_gauss(:)
      real(kind = kreal), allocatable :: sdev_gauss(:)
!
      character(len=kchara) :: tave_pick_gauss_head
      character(len=kchara) :: trms_pick_gauss_head
      character(len=kchara) :: sdev_pick_gauss_head
      integer(kind = kint), parameter :: id_pick = 15
!
      integer(kind = kint) :: istep_start, istep_end, istep_inc
      integer(kind = kint) :: i_step, ierr, icou, ipick
      real(kind = kreal) :: acou, time
!
!
      write(*,*) 'input picked gauss coefficients file prefix'
      read(5,*) gauss_IO%file_prefix
!
      write(tave_pick_gauss_head,'(a6,a)')                              &
        't_ave_',  trim(gauss_IO%file_prefix)
      write(trms_pick_gauss_head,'(a6,a)')                              &
        't_rms_',  trim(gauss_IO%file_prefix)
      write(sdev_pick_gauss_head,'(a6,a)')                              &
        't_sdev_', trim(gauss_IO%file_prefix)
!
      write(*,*) 'input start, end, increment steps'
      read(5,*) istep_start, istep_end, istep_inc
!
      call open_gauss_coefs_read_monitor(id_pick, gauss_IO)
!
      allocate(ave_gauss(gauss_IO%num_mode))
      allocate(rms_gauss(gauss_IO%num_mode))
      allocate(sdev_gauss(gauss_IO%num_mode))
!
!$omp parallel workshare
      ave_gauss = 0.0d0
      rms_gauss = 0.0d0
      sdev_gauss = 0.0d0
!$omp end parallel workshare
!
!
      icou = 0
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
     &          * (gauss_IO%gauss_coef(ipick) + prev_spec(ipick))       &
     &          * (time - prev_time)
              rms_gauss(ipick) = rms_gauss(ipick) + half                &
     &          * (gauss_IO%gauss_coef(ipick)**2 + prev_spec(ipick)**2) &
     &          * (time - prev_time)
            end do
!$omp end parallel do
          end if
!
!$omp parallel do
          do ipick = 1, gauss_IO%num_mode
            prev_spec(ipick) = gauss_IO%gauss_coef(ipick)
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
!$omp parallel do
      do ipick = 1, gauss_IO%num_mode
        gauss_IO%gauss_coef(1,ipick) = ave_gauss(ipick) 
      end do
!$omp end parallel do
!
      gauss_IO%file_prefix = tave_pick_gauss_head
      call write_gauss_coefs_4_monitor(0, i_step, time, gauss_IO)
!
!  Output time average
!$omp parallel do
      do ipick = 1, gauss_IO%num_mode
        gauss_IO%gauss_coef(1,ipick) = rms_gauss(ipick) 
      end do
!$omp end parallel do
!
      gauss_IO%file_prefix = trms_pick_gauss_head
      call write_gauss_coefs_4_monitor(0, i_step, time, gauss_IO)
!
!  Output time average
!$omp parallel do
      do ipick = 1, gauss_IO%num_mode
        gauss_IO%gauss_coef(1,ipick) = sdev_gauss(ipick)
      end do
!$omp end parallel do
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
