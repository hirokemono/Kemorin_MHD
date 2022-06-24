!
!
!
      program append_sph_monitor_files
!
      use m_precision
      use m_constants
      use t_ctl_data_append_sph_mntr
      use t_ctl_data_tave_sph_monitor
      use t_ctl_param_sph_series_util
!
      use t_picked_sph_spectr_data_IO
!
      use set_control_4_pickup_sph
      use picked_sph_spectr_data_IO
      use count_monitor_time_series
!
      implicit none
!
      character(len=kchara), parameter                                  &
     &                      :: ctl_file_name = 'control_append_monitor'
!>      Structure for control data
      type(ctl_data_add_sph_monitor), save :: add_mtr_ctl1
      type(sph_spectr_file_param), save :: spec_evo_append
      type(sph_spectr_file_param), save :: spec_evo_target
!
      type(picked_spectrum_data_IO), save :: append_pick_IO
      type(picked_spectrum_data_IO), save :: target_pick_IO
!
      integer(kind = kint), parameter :: id_append_file = 15
      integer(kind = kint), parameter :: id_target_file = 16
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: istep_start, istep_end, i_step, ierr
      real(kind = kreal) :: start_time, end_time, time
!
      integer(kind = kint) :: i, nd, ic, icou, ntot_pick
!
      call read_ctl_file_add_sph_mntr(ctl_file_name, add_mtr_ctl1)
      call set_spec_series_file_param(add_mtr_ctl1%append_monitor_ctl,  &
     &                                spec_evo_append)
      call set_spec_series_file_param(add_mtr_ctl1%target_monitor_ctl,  &
     &                                spec_evo_target)
      call dealloc_ctl_data_add_sph_mntr(add_mtr_ctl1)
!
!
      if(spec_evo_append%vol_series%num_file                            &
     &     .ne. spec_evo_target%vol_series%num_file) then
        write(*,*) '# of file of volume average data does not match.',  &
     &            spec_evo_append%vol_series%num_file,                  &
     &            spec_evo_target%vol_series%num_file
        stop
      end if
      if(spec_evo_append%vol_spec_series%num_file                       &
     &     .ne. spec_evo_target%vol_spec_series%num_file) then
        write(*,*) '# of file of volume spectr data does not match.',   &
     &            spec_evo_append%vol_spec_series%num_file,             &
     &            spec_evo_target%vol_spec_series%num_file
        stop
      end if
      if(spec_evo_append%layer_series%num_file                          &
     &     .ne. spec_evo_target%layer_series%num_file) then
        write(*,*) '# of file of sphere average data does not match.',  &
     &            spec_evo_append%layer_series%num_file,                &
     &            spec_evo_target%layer_series%num_file
        stop
      end if
      if(spec_evo_append%layer_spec_series%num_file                     &
     &     .ne. spec_evo_target%layer_spec_series%num_file) then
        write(*,*) '# of file of sphere spectr data does not match.',   &
     &            spec_evo_append%layer_spec_series%num_file,           &
     &            spec_evo_target%layer_spec_series%num_file
        stop
      end if
      if(spec_evo_append%pick_spec_series%num_file                      &
     &     .ne. spec_evo_target%pick_spec_series%num_file) then
        write(*,*) '# of file of picked spectr data does not match.',   &
     &            spec_evo_append%pick_spec_series%num_file,            &
     &            spec_evo_target%pick_spec_series%num_file
        stop
      end if
!
      do i = 1, spec_evo_target%pick_spec_series%num_file
        file_name = spec_evo_append%pick_spec_series%evo_file_name(i)
        write(*,*) 'Open append file', i, ': ', trim(file_name)
!        call check_picked_sph_spectr(file_name, append_pick_IO)
        open(id_append_file, file = file_name)
        call read_pick_series_head(id_append_file, append_pick_IO)
        call alloc_pick_sph_monitor_IO(append_pick_IO)
        call read_pick_series_comp_name(id_append_file, append_pick_IO)
        call read_sph_spectr_time(id_append_file, ione,                 &
     &      istep_start, start_time, ierr)
        backspace(id_append_file)
!
        file_name = spec_evo_target%pick_spec_series%evo_file_name(i)
        write(*,*) 'Open target file', i, ': ', trim(file_name)
!        call check_picked_sph_spectr(file_name, target_pick_IO)
        open(id_target_file, file = file_name, position='append')
        backspace(id_target_file)
        call read_sph_spectr_time(id_target_file, ione,                 &
     &      istep_end, end_time, ierr)
        rewind(id_target_file)
        call read_pick_series_head(id_target_file, target_pick_IO)
        call alloc_pick_sph_monitor_IO(target_pick_IO)
        call read_pick_series_comp_name(id_target_file, target_pick_IO)
!
        if(append_pick_IO%num_layer .ne. target_pick_IO%num_layer) then
          write(*,*) '# of radial layer does not match',                &
     &        append_pick_IO%num_layer, target_pick_IO%num_layer
          stop
        end if
        if(append_pick_IO%num_mode .ne. target_pick_IO%num_mode) then
          write(*,*) '# of spherical harmonics mode does not match',    &
     &        append_pick_IO%num_mode, target_pick_IO%num_mode
          stop
        end if
        if(append_pick_IO%ntot_comp .ne. target_pick_IO%ntot_comp) then
          write(*,*) '# of components in files does not match',         &
     &        append_pick_IO%num_mode, target_pick_IO%num_mode
          stop
        end if
!
        do nd = 1, target_pick_IO%ntot_comp
          if(append_pick_IO%spectr_name(nd)                             &
     &              .ne. target_pick_IO%spectr_name(nd)) then
            write(*,*) nd, '-th components in files does not match',    &
     &              trim(append_pick_IO%spectr_name(nd)),               &
     &              trim(target_pick_IO%spectr_name(nd))
            stop
          end if
        end do
!
        do nd = 1, target_pick_IO%ntot_comp
          if(append_pick_IO%spectr_name(nd)                             &
     &              .ne. target_pick_IO%spectr_name(nd)) then
            write(*,*) nd, '-th components in files does not match',    &
     &              trim(append_pick_IO%spectr_name(nd)),               &
     &              trim(target_pick_IO%spectr_name(nd))
            stop
          end if
        end do
!
        write(*,*) 'end step and time for target file',                 &
     &          istep_end, end_time
        write(*,*) 'start step and time for append file',               &
     &          istep_start, start_time
!
        ntot_pick = target_pick_IO%num_layer * target_pick_IO%num_mode
        icou = 0
        if(istep_start .gt. istep_end) then
          close(id_target_file)
          open(id_target_file, file = file_name, position='append')
        else
          do
            call read_sph_spectr_time(id_target_file, ntot_pick,        &
     &                                i_step, time, ierr)
            write(*,'(69a1,a5,i12,a4,1pe16.8e3,a20,i12)',advance="NO")  &
     &          (char(8),ic=1,69), 'step ', i_step,                     &
     &          ' at ', time, ' is read. count is  ', icou
            if(i_step .ge. istep_start) exit
            if(ierr .gt. 0) exit
          end do
        end if
        write(*,*)
        write(*,*) 'Start Append'
!
        do
          icou = icou + 1
          call read_sph_spec_monitor                                    &
     &       (id_append_file, i_step, time, append_pick_IO, ierr)
          write(*,'(69a1,a5,i12,a4,1pe16.8e3,a20,i12)',advance="NO")    &
     &          (char(8),ic=1,69), 'step ', i_step,                     &
     &          ' at ', time, ' is read. count is  ', icou
          if(ierr .gt. 0) exit
!
!$omp parallel do
          do ic = 1, ntot_pick
            target_pick_IO%idx_sph(ic,1) = append_pick_IO%idx_sph(ic,1)
            target_pick_IO%idx_sph(ic,2) = append_pick_IO%idx_sph(ic,2)
            target_pick_IO%idx_sph(ic,3) = append_pick_IO%idx_sph(ic,3)
            target_pick_IO%idx_sph(ic,4) = append_pick_IO%idx_sph(ic,4)
            target_pick_IO%radius(ic) =    append_pick_IO%radius(ic)
          end do
!$omp end parallel do
!$omp parallel workshare
          target_pick_IO%d_pk(1:target_pick_IO%ntot_data)               &
     &          = append_pick_IO%d_pk(1:target_pick_IO%ntot_data)
!$omp end parallel workshare
!
          call write_picked_sph_snap                                    &
     &       (id_target_file, i_step, time, target_pick_IO)
        end do
!
        close(id_target_file)
        close(id_append_file)
!
        call dealloc_pick_sph_monitor_IO(append_pick_IO)
        call dealloc_pick_sph_monitor_IO(target_pick_IO)
      end do
!
      call dealloc_spec_series_file_param(spec_evo_append)
      call dealloc_spec_series_file_param(spec_evo_target)
!
      write(*,*) 'Program is done.'
      stop
!
      end program append_sph_monitor_files
