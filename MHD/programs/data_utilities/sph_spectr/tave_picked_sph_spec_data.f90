!tave_picked_sph_spec_data.f90
!      program tave_picked_sph_spec_data
!
!        programmed by H.Matsui on Dec., 2012
!
      program tave_picked_sph_spec_data
!
      use m_precision
      use m_constants
      use t_pickup_sph_spectr_data
      use picked_sph_spectr_data_IO
!
      implicit  none
!
      type(picked_spectrum_data), save :: pick
!
      real(kind = kreal), allocatable :: prev_spec(:,:)
      real(kind = kreal), allocatable :: ave_spec(:,:)
      real(kind = kreal), allocatable :: rms_spec(:,:)
      real(kind = kreal), allocatable :: sdev_spec(:,:)
!
      character(len=kchara) :: evo_header
      character(len=kchara) :: tave_header
      character(len=kchara) :: trms_header
      character(len=kchara) :: sdev_header
      integer(kind = kint), parameter :: id_pick = 15
!
      integer(kind = kint) :: i_step, ierr, num, icou, ipick, nd
      real(kind = kreal) :: acou, time, prev_time
      real(kind = kreal) :: start_time, end_time, true_start
!
!
      write(*,*) 'Input picked spectr evolution file header'
      read(5,*) evo_header
!
      write(tave_header,'(a6,a)') 't_ave_', trim(evo_header)
      write(trms_header,'(a8,a)') 't_rms_', trim(evo_header)
      write(sdev_header,'(a8,a)') 't_sigma_', trim(evo_header)
!
      write(*,*) 'Input start and end time'
      read(5,*) start_time, end_time
!
      pick%file_prefix = evo_header
      call open_sph_spec_read(id_pick, pick)
!
      num = pick%num_sph_mode * pick%num_layer
      allocate( prev_spec(pick%ntot_comp_rj,num) )
      allocate( ave_spec(pick%ntot_comp_rj,num) )
      allocate( rms_spec(pick%ntot_comp_rj,num) )
      allocate( sdev_spec(pick%ntot_comp_rj,num) )
!
!$omp parallel workshare
      prev_spec =  0.0d0
      ave_spec =   0.0d0
      rms_spec =   0.0d0
      sdev_spec =  0.0d0
!$omp end parallel workshare
!
!       Evaluate time average
!
      icou = 0
      do
        call read_sph_spec_monitor(id_pick, i_step, time, pick, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          if(icou .eq. 0) then
            true_start = time
          else
!$omp parallel
            do ipick = 1, pick%num_sph_mode*pick%num_layer
!$omp do
              do nd = 1, pick%ntot_comp_rj
                ave_spec(nd,ipick) = ave_spec(nd,ipick) + half          &
     &           * (pick%d_rj_gl(nd,ipick) + prev_spec(nd,ipick))       &
     &           * (time - prev_time)
                rms_spec(nd,ipick) = rms_spec(nd,ipick) + half          &
     &           * (pick%d_rj_gl(nd,ipick)**2 + prev_spec(nd,ipick)**2) &
     &           * (time - prev_time)
              end do
!$omp end do nowait
            end do
!$omp end parallel
          end if
!
!$omp parallel
          do ipick = 1, pick%num_sph_mode*pick%num_layer
!$omp do
            do nd = 1, pick%ntot_comp_rj
              prev_spec(nd,ipick) = pick%d_rj_gl(nd,ipick)
            end do
!$omp end do nowait
          end do
!$omp end parallel
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
!$omp parallel
      do ipick = 1, pick%num_sph_mode*pick%num_layer
!$omp do
        do nd = 1, pick%ntot_comp_rj
          sdev_spec(nd,ipick)                                           &
     &        = rms_spec(nd,ipick) - ave_spec(nd,ipick)**2
!
          ave_spec(nd,ipick) =   ave_spec(nd,ipick) * acou
          rms_spec(nd,ipick) =   sqrt(rms_spec(nd,ipick) * acou)
          sdev_spec(nd,ipick) =  sqrt(sdev_spec(nd,ipick) * acou)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!    output time average
!
      do ipick = 1, pick%num_sph_mode*pick%num_layer
        do nd = 1, pick%ntot_comp_rj
          pick%d_rj_gl(nd,ipick) = ave_spec(nd,ipick)
        end do
      end do
!
      pick%file_prefix = tave_header
      call write_sph_spec_monitor(0, i_step, time, pick)
!
!    output RMS deviation
!
      do ipick = 1, pick%num_sph_mode*pick%num_layer
        do nd = 1, pick%ntot_comp_rj
          pick%d_rj_gl(nd,ipick) = rms_spec(nd,ipick)
        end do
      end do
!
      pick%file_prefix = trms_header
      call write_sph_spec_monitor(0, i_step, time, pick)
!
!    output standard deviation
!
      do ipick = 1, pick%num_sph_mode*pick%num_layer
        do nd = 1, pick%ntot_comp_rj
          pick%d_rj_gl(nd,ipick) = sdev_spec(nd,ipick)
        end do
      end do
!
      pick%file_prefix = sdev_header
      call write_sph_spec_monitor(0, i_step, time, pick)
!
      call dealloc_pick_sph_monitor(pick)
      call dealloc_num_pick_layer(pick)
      deallocate(prev_spec, ave_spec, sdev_spec)
!
      write(*,*) '***** program finished *****'
      stop
!
      end program tave_picked_sph_spec_data
