!tave_picked_gauss_coefs.f90
!
!        programmed by H.Matsui on Dec., 2012
!
      program tave_picked_gauss_coefs
!
      use m_precision
      use m_constants
!
      use t_pickup_sph_spectr_data
      use gauss_coefs_monitor_IO
!
      implicit  none
!
!>      Structure for gauss coeffciients
      type(picked_spectrum_data), save :: gauss_org
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
      read(5,*) gauss_org%file_prefix
!
      write(tave_pick_gauss_head,'(a6,a)')                              &
        't_ave_', trim(gauss_org%file_prefix)
!
      write(*,*) 'input start, end, increment steps'
      read(5,*) istep_start, istep_end, istep_inc
!
      call open_gauss_coefs_read_monitor(id_pick, gauss_org)
!
      allocate( ave_gauss(gauss_org%num_sph_mode))
      ave_gauss = 0.0d0
!
!
      icou = 0
      do
        call read_gauss_coefs_4_monitor                                 &
     &     (id_pick, i_step, time, gauss_org, ierr)
        if(ierr .gt. 0) exit
!
        if(mod((i_step-istep_start),istep_inc) .eq. 0                   &
     &     .and. i_step.ge.istep_start) then
!
          do ipick = 1, gauss_org%num_sph_mode
            ave_gauss(ipick)                                            &
     &          = ave_gauss(ipick) + gauss_org%d_rj_gl(1,ipick)
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
      do ipick = 1, gauss_org%num_sph_mode
        gauss_org%d_rj_gl(1,ipick) = ave_gauss(ipick) * acou
      end do
!
      deallocate(ave_gauss)
!
      gauss_org%file_prefix = tave_pick_gauss_head
      call write_gauss_coefs_4_monitor(izero, i_step, time, gauss_org)
!
      write(*,*) '***** program finished *****'
      stop
!
      end program tave_picked_gauss_coefs
