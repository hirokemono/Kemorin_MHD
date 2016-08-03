!tave_picked_gauss_coefs.f90
!
!        programmed by H.Matsui on Dec., 2012
!
      program tave_picked_gauss_coefs
!
      use m_precision
      use m_constants
!
      use m_gauss_coefs_monitor_data
!
      implicit  none
!
      real(kind = kreal), allocatable :: ave_gauss(:)
      character(len=kchara) :: tave_pick_gauss_head
      integer(kind = kint), parameter :: id_pick = 15
!
      integer(kind = kint) :: istep_start, istep_end, istep_inc
      integer(kind = kint) :: i_step, ierr, icou, ipick
      real(kind = kreal) :: acou, time
!
!
      write(*,*) 'input picked gauss coefficients file prefix'
      read(5,*) gauss_coefs_file_head
!
      write(tave_pick_gauss_head,'(a6,a)')                              &
        't_ave_', trim(gauss_coefs_file_head)
!
      write(*,*) 'input start, end, increment steps'
      read(5,*) istep_start, istep_end, istep_inc
!
      call open_gauss_coefs_read_monitor(id_pick)
!
      allocate( ave_gauss(num_pick_gauss_mode))
      ave_gauss = 0.0d0
!
!
      icou = 0
      do
        call read_gauss_coefs_4_monitor(id_pick, i_step, time, ierr)
        if(ierr .gt. 0) exit
!
        if(mod((i_step-istep_start),istep_inc) .eq. 0                   &
     &     .and. i_step.ge.istep_start) then
!
          do ipick = 1, num_pick_gauss_mode
            ave_gauss(ipick) = ave_gauss(ipick) + gauss_coef_gl(ipick)
          end do
          icou = icou + 1
          write(*,*) 'step ', i_step, ' is added: count is  ', icou
        end if
!
        if(i_step .ge. istep_end) exit
      end do
      close(id_pick)
!
      acou = one / dble(icou)
      do ipick = 1, num_pick_gauss_mode
        gauss_coef_gl(ipick) = ave_gauss(ipick) * acou
      end do
!
      deallocate(ave_gauss)
!
      gauss_coefs_file_head = tave_pick_gauss_head
      call write_gauss_coefs_4_monitor(izero, i_step, time)
!
      write(*,*) '***** program finished *****'
      stop
!
      end program tave_picked_gauss_coefs
