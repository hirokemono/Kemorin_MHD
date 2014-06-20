!tave_picked_sph_rms_data.f90
!      program tave_picked_sph_rms_data
!
!        programmed by H.Matsui on Dec., 2012
!
      program tave_picked_sph_rms_data
!
      use m_precision
      use m_constants
!
      use m_pickup_sph_spectr_data
      use m_pickup_sph_rms_data
!
      implicit  none
!
      real(kind = kreal), allocatable :: ave_rms_pick_sph(:,:)
      character(len=kchara) :: tave_pick_sph_rms_head
      integer(kind = kint), parameter :: id_pick = 15
!
      integer(kind = kint) :: istep_start, istep_end, istep_inc
      integer(kind = kint) :: i_step, ierr, num, icou, ipick, nd
      real(kind = kreal) :: acou, time
!
!
      write(*,*) 'input picked RMS evolution file header'
      read(5,*) pickup_sph_rms_head
      write(*,*) 'input time averaged file header'
      read(5,*) tave_pick_sph_rms_head
      if(tave_pick_sph_rms_head .eq. pickup_sph_rms_head) then
        write(*,*) 'set different file header for averaged data'
        stop
      end if
!
      write(*,*) 'input start, end, increment steps'
      read(5,*) istep_start, istep_end, istep_inc
!
      call open_sph_rms_read_monitor(id_pick)
!
      num = ntot_pick_sph_rms_mode*num_pick_layer
      allocate( ave_rms_pick_sph(ncomp_pick_sph_rms,num) )
      ave_rms_pick_sph = 0.0d0
!
!
      icou = 0
      do
        call  read_sph_rms_4_monitor(id_pick, i_step, time, ierr)
        if(ierr .gt. 0) exit
!
        if(mod((i_step-istep_start),istep_inc) .eq. 0                   &
     &     .and. i_step.ge.istep_start) then
!
          do ipick = 1, ntot_pick_sph_rms_mode*num_pick_layer
            do nd = 1, ncomp_pick_sph_rms
              ave_rms_pick_sph(nd,ipick) = ave_rms_pick_sph(nd,ipick)   &
     &                                    + d_rms_pick_sph_gl(nd,ipick)
            end do
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
      do ipick = 1, ntot_pick_sph_rms_mode*num_pick_layer
        do nd = 1, ncomp_pick_sph_rms
          d_rms_pick_sph_gl(nd,ipick) = ave_rms_pick_sph(nd,ipick)      &
     &                                 * acou
        end do
      end do
!
      deallocate(ave_rms_pick_sph)
!
      pickup_sph_rms_head = tave_pick_sph_rms_head
      call write_sph_rms_4_monitor(izero, i_step, time)
!
      write(*,*) '***** program finished *****'
      stop
!
      end program tave_picked_sph_rms_data
