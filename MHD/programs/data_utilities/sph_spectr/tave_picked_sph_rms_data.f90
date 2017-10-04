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
      use t_pickup_sph_spectr_data
      use picked_sph_spectr_data_IO
!
      implicit  none
!
      type(picked_spectrum_data), save :: pick_rms
      character(len=kchara) :: pickup_sph_rms_head
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
      read(5,*) pick_rms%file_prefix
      write(tave_pick_sph_rms_head,'(a,a)')                             &
     &         't_ave_', trim(pick_rms%file_prefix)
!
      write(*,*) 'input start, end, increment steps'
      read(5,*) istep_start, istep_end, istep_inc
!
      call open_sph_spec_read(id_pick, pick_rms)
!
      num = pick_rms%num_sph_mode * pick_rms%num_layer
      allocate( ave_rms_pick_sph(pick_rms%ntot_comp_rj,num) )
      ave_rms_pick_sph = 0.0d0
!
!
      icou = 0
      do
        call read_sph_spec_monitor                                      &
     &     (id_pick, i_step, time, pick_rms, ierr)
        if(ierr .gt. 0) exit
!
        if(mod((i_step-istep_start),istep_inc) .eq. 0                   &
     &     .and. i_step.ge.istep_start) then
!
          do ipick = 1, pick_rms%num_sph_mode * pick_rms%num_layer
            do nd = 1, pick_rms%ntot_comp_rj
              ave_rms_pick_sph(nd,ipick) = ave_rms_pick_sph(nd,ipick)   &
     &                                    + pick_rms%d_rj_gl(nd,ipick)
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
      do ipick = 1, pick_rms%num_sph_mode * pick_rms%num_layer
        do nd = 1, pick_rms%ntot_comp_rj
          pick_rms%d_rj_gl(nd,ipick) = ave_rms_pick_sph(nd,ipick)      &
     &                                 * acou
        end do
      end do
!
      deallocate(ave_rms_pick_sph)
!
      pick_rms%file_prefix = tave_pick_sph_rms_head
      call write_sph_spec_monitor(izero, i_step, time, pick_rms)
!
      write(*,*) '***** program finished *****'
      stop
!
      end program tave_picked_sph_rms_data
