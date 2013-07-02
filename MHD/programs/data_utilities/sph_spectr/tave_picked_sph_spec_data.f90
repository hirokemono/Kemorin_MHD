!tave_picked_sph_spec_data.f90
!      program tave_picked_sph_spec_data
!
!        programmed by H.Matsui on Dec., 2012
!
      program tave_picked_sph_spec_data
!
      use m_precision
      use m_constants
!
      use m_pickup_sph_spectr_data
!
      implicit  none
!
      real(kind = kreal), allocatable :: ave_spec(:,:)
      real(kind = kreal), allocatable :: sdev_spec(:,:)
      character(len=kchara) :: evo_header
      character(len=kchara) :: tave_header
      character(len=kchara) :: sdev_header
      integer(kind = kint), parameter :: id_pick = 15
!
      integer(kind = kint) :: istep_start, istep_end, istep_inc
      integer(kind = kint) :: i_step, ierr, num, icou, ipick, nd
      real(kind = kreal) :: acou, time
!
!
      write(*,*) 'input picked spectr evolution file header'
      read(5,*) evo_header
!
      pickup_sph_head = evo_header
      write(tave_header,'(a5,a)') 'tave_', trim(evo_header)
      write(sdev_header,'(a6,a)') 'sigma_', trim(evo_header)
!
      write(*,*) 'input start, end, increment steps'
      read(5,*) istep_start, istep_end, istep_inc
!
      call open_sph_spec_read_monitor(id_pick)
!
      num = ntot_pick_sph_mode*num_pick_layer
      allocate( ave_spec(ncomp_pick_sph_coef,num) )
      allocate( sdev_spec(ncomp_pick_sph_coef,num) )
      ave_spec =   0.0d0
      sdev_spec = 0.0d0
!
!       Evaluate time average
!
      icou = 0
      do
        call read_sph_spec_4_monitor(id_pick, i_step, time, ierr)
        if(ierr .gt. 0) exit
!
        if(mod((i_step-istep_start),istep_inc) .eq. 0                   &
     &     .and. i_step.ge.istep_start) then
!
          do ipick = 1, num_pick_sph_mode*num_pick_layer
            do nd = 1, ncomp_pick_sph_coef
              ave_spec(nd,ipick) = ave_spec(nd,ipick)           &
     &                                + d_rj_pick_sph_gl(nd,ipick)
            end do
          end do
          icou = icou + 1
          write(*,*) 'step ', i_step,                                   &
     &        ' is added for time average: count is  ', icou
        end if
!
        if(i_step .ge. istep_end) exit
      end do
      close(id_pick)
!
      acou = one / dble(icou)
      do ipick = 1, num_pick_sph_mode*num_pick_layer
        do nd = 1, ncomp_pick_sph_coef
          ave_spec(nd,ipick) = ave_spec(nd,ipick) * acou
        end do
      end do
!
      call deallocate_pick_sph_monitor
      call deallocate_num_pick_layer
!
!       Evaluate standard deviation
!
      call open_sph_spec_read_monitor(id_pick)
!
      icou = 0
      do
        call read_sph_spec_4_monitor(id_pick, i_step, time, ierr)
        if(ierr .gt. 0) exit
!
        if(mod((i_step-istep_start),istep_inc) .eq. 0                   &
     &     .and. i_step.ge.istep_start) then
!
          do ipick = 1, num_pick_sph_mode*num_pick_layer
            do nd = 1, ncomp_pick_sph_coef
              sdev_spec(nd,ipick) = sdev_spec(nd,ipick)                 &
     &          + (d_rj_pick_sph_gl(nd,ipick) - ave_spec(nd,ipick))**2
            end do
          end do
          icou = icou + 1
          write(*,*) 'step ', i_step,                                   &
     &        ' is added for standard deviation: count is  ', icou
        end if
!
        if(i_step .ge. istep_end) exit
      end do
      close(id_pick)
!
      acou = one / dble(icou)
      do ipick = 1, num_pick_sph_mode*num_pick_layer
        do nd = 1, ncomp_pick_sph_coef
          sdev_spec(nd,ipick) = sqrt(sdev_spec(nd,ipick)) * acou
        end do
      end do
!
!    output time average
!
      do ipick = 1, num_pick_sph_mode*num_pick_layer
        do nd = 1, ncomp_pick_sph_coef
          d_rj_pick_sph_gl(nd,ipick) = ave_spec(nd,ipick)
        end do
      end do
!
      pickup_sph_head = tave_header
      call write_sph_spec_4_monitor(izero, i_step, time)
!
!    output standard deviation
!
      do ipick = 1, num_pick_sph_mode*num_pick_layer
        do nd = 1, ncomp_pick_sph_coef
          d_rj_pick_sph_gl(nd,ipick) = sdev_spec(nd,ipick)
        end do
      end do
!
      pickup_sph_head = sdev_header
      call write_sph_spec_4_monitor(izero, i_step, time)
!
      call deallocate_pick_sph_monitor
      call deallocate_num_pick_layer
      deallocate(ave_spec, sdev_spec)
!
      write(*,*) '***** program finished *****'
      stop
!
      end program tave_picked_sph_spec_data
