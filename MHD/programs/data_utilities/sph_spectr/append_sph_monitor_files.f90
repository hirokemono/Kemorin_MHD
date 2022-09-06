!
!
!
      program append_sph_monitor_files
!
      use m_precision
      use m_constants
      use m_file_format_labels
      use t_ctl_data_append_sph_mntr
      use t_ctl_data_tave_sph_monitor
      use t_ctl_param_sph_series_util
      use t_read_sph_spectra
!
      use set_control_4_pickup_sph
      use count_monitor_time_series
      use set_parallel_file_name
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
      type(multi_flag_labels), save :: gzip_flags1
!
      character(len=kchara) :: append_gauss_name
      character(len=kchara) :: target_gauss_name
!
      integer(kind = kint), parameter :: id_append_file = 15
!
      integer(kind = kint) :: i, ntot_pick
      integer(kind = kint) :: iflag_gauss
!
      call read_ctl_file_add_sph_mntr(ctl_file_name, add_mtr_ctl1)
!
      call init_multi_flags_by_labels(itwo, gzip_names, gzip_flags1)
      call set_spec_series_file_param(add_mtr_ctl1%folder_to_read_ctl,  &
     &    add_mtr_ctl1%read_monitor_file_format_ctl, gzip_flags1,       &
     &    add_mtr_ctl1%monitor_list_ctl, spec_evo_append)
      call set_spec_series_file_param(add_mtr_ctl1%folder_to_add_ctl,   &
     &    dummy_item, gzip_flags1, add_mtr_ctl1%monitor_list_ctl,       &
     &    spec_evo_target)
!
      iflag_gauss                                                       &
     &    = add_mtr_ctl1%monitor_list_ctl%gauss_coefs_prefix%iflag
      if(iflag_gauss .gt. 0) then
        call set_sph_series_file_name(add_mtr_ctl1%folder_to_read_ctl,  &
     &     add_mtr_ctl1%read_monitor_file_format_ctl, gzip_flags1,      &
     &     add_mtr_ctl1%monitor_list_ctl%gauss_coefs_prefix%charavalue, &
     &     append_gauss_name)
        call set_sph_series_file_name                                   &
     &    (add_mtr_ctl1%folder_to_add_ctl, dummy_item, gzip_flags1,     &
     &     add_mtr_ctl1%monitor_list_ctl%gauss_coefs_prefix%charavalue, &
     &     target_gauss_name)
      end if
!
!
      call dealloc_ctl_data_add_sph_mntr(add_mtr_ctl1)
!
!      if(iflag_gauss .gt. 0) then
!        call append_gauss_coef_file                                    &
!     &     (append_gauss_name, target_gauss_name)
!      end if
!
      if(spec_evo_append%vol_series%num_file                            &
     &     .ne. spec_evo_target%vol_series%num_file) then
        write(*,*) '# of file of volume average data does not match.',  &
     &            spec_evo_append%vol_series%num_file,                  &
     &            spec_evo_target%vol_series%num_file
        stop
      end if
      do i = 1, spec_evo_target%vol_series%num_file
        call append_sph_mean_sq_file(spectr_off, volume_on,             &
     &      spec_evo_append%vol_series%evo_file_name(i),                &
     &      spec_evo_target%vol_series%evo_file_name(i))
      end do
!
      if(spec_evo_append%vol_spec_series%num_file                       &
     &     .ne. spec_evo_target%vol_spec_series%num_file) then
        write(*,*) '# of file of volume spectr data does not match.',   &
     &            spec_evo_append%vol_spec_series%num_file,             &
     &            spec_evo_target%vol_spec_series%num_file
        stop
      end if
      do i = 1, spec_evo_target%vol_spec_series%num_file
        call append_sph_mean_sq_file(spectr_on, volume_on,              &
     &      spec_evo_append%vol_spec_series%evo_file_name(i),           &
     &      spec_evo_target%vol_spec_series%evo_file_name(i))
      end do
!
      if(spec_evo_append%layer_series%num_file                          &
     &     .ne. spec_evo_target%layer_series%num_file) then
        write(*,*) '# of file of sphere average data does not match.',  &
     &            spec_evo_append%layer_series%num_file,                &
     &            spec_evo_target%layer_series%num_file
        stop
      end if
      do i = 1, spec_evo_target%layer_series%num_file
        call append_sph_mean_sq_file(spectr_off, volume_off,            &
     &      spec_evo_append%layer_series%evo_file_name(i),              &
     &      spec_evo_target%layer_series%evo_file_name(i))
      end do
!
      if(spec_evo_append%layer_spec_series%num_file                     &
     &     .ne. spec_evo_target%layer_spec_series%num_file) then
        write(*,*) '# of file of sphere spectr data does not match.',   &
     &            spec_evo_append%layer_spec_series%num_file,           &
     &            spec_evo_target%layer_spec_series%num_file
        stop
      end if
      do i = 1, spec_evo_target%layer_spec_series%num_file
        call append_sph_mean_sq_file(spectr_on, volume_off,             &
     &      spec_evo_append%layer_spec_series%evo_file_name(i),         &
     &      spec_evo_target%layer_spec_series%evo_file_name(i))
      end do
!
!
      if(spec_evo_append%pick_spec_series%num_file                      &
     &     .ne. spec_evo_target%pick_spec_series%num_file) then
        write(*,*) '# of file of picked spectr data does not match.',   &
     &            spec_evo_append%pick_spec_series%num_file,            &
     &            spec_evo_target%pick_spec_series%num_file
        stop
      end if
      do i = 1, spec_evo_target%pick_spec_series%num_file
        call append_picked_spectr_file                                  &
     &     (spec_evo_append%pick_spec_series%evo_file_name(i),          &
     &      spec_evo_target%pick_spec_series%evo_file_name(i))
      end do
!
      call dealloc_spec_series_file_param(spec_evo_append)
      call dealloc_spec_series_file_param(spec_evo_target)
!
      write(*,*) 'Program is done.'
      stop
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine append_gauss_coef_file                                 &
     &         (append_file_name, target_file_name)
!
      use t_gauss_coefs_monitor_IO
      use gauss_coefs_monitor_IO
      use gz_spl_sph_spectr_data_IO
!
      implicit none
!
      character(len=kchara), intent(in) :: append_file_name
      character(len=kchara), intent(in) :: target_file_name
!
!
      type(picked_gauss_coefs_IO), save :: append_gauss_IN
      type(picked_gauss_coefs_IO), save :: target_gauss_IN
!
      integer(kind = kint), parameter :: id_append_file = 15
      integer(kind = kint), parameter :: id_write_file = 16
!
      integer(kind = kint) :: istep_start, istep_end, i_step, ierr
      real(kind = kreal) :: start_time, end_time, time
!
      integer(kind = kint) :: nd, ic, icou
      integer(kind = kint) :: nchara_line
      character(len = 1), allocatable :: textbuf(:)
!
      logical :: flag_gzip
      type(buffer_4_gzip), save :: zbuf
      character, pointer, save  :: FPz_f1
!
!
      write(*,*) 'Open file to append.'
        open(id_append_file, file=append_file_name)
        call read_gauss_coefs_header(id_append_file, append_gauss_IN)
        call alloc_gauss_coef_monitor(append_gauss_IN)
        call read_gauss_coefs_labels(id_append_file, append_gauss_IN)
        call read_sph_spectr_time                                       &
     &     (id_append_file, ione, istep_start, start_time, ierr)
        backspace(id_append_file)
!
        write(*,*) 'Open file to write', ': ', trim(target_file_name)
        open(id_write_file, file=target_file_name)
        call read_gauss_coefs_header(id_write_file, target_gauss_IN)
        call alloc_gauss_coef_monitor(target_gauss_IN)
        call read_gauss_coefs_labels(id_write_file, target_gauss_IN)
        close(id_write_file)
!
        if(append_gauss_IN%radius_gauss                                 &
     &         .ne. target_gauss_IN%radius_gauss) then
          write(*,*) 'Radius does not match',                           &
     &       append_gauss_IN%radius_gauss, target_gauss_IN%radius_gauss
          stop
        end if
        if(append_gauss_IN%num_mode .ne. target_gauss_IN%num_mode) then
          write(*,*) '# of gauss coefficients does not match',          &
     &        append_gauss_IN%num_mode, target_gauss_IN%num_mode
          stop
        end if
!
        do nd = 1, target_gauss_IN%num_mode
          if(append_gauss_IN%gauss_coef_name(nd)                        &
     &              .ne. target_gauss_IN%gauss_coef_name(nd)) then
            write(*,*) nd, '-th coeffient in files does not match',     &
     &              trim(append_gauss_IN%gauss_coef_name(nd)), '    ',  &
     &              trim(target_gauss_IN%gauss_coef_name(nd))
            stop
          end if
        end do
!
        open(id_write_file, file=target_file_name, position='append')
        backspace(id_write_file)
        call read_sph_spectr_time(id_write_file, ione,                  &
     &                            istep_end, end_time, ierr)
!
        write(*,*) 'end step and time for target file',                 &
     &          istep_end, end_time
        write(*,*) 'start step and time for append file',               &
     &          istep_start, start_time
!
        icou = 0
        if(istep_start .le. istep_end) then
          do
            backspace(id_write_file)
            call read_sph_spectr_time(id_write_file, ione,              &
     &                                i_step, time, ierr)
            write(*,'(78a1,a5,i12,a4,1pe16.8e3,a29,i12)',advance="NO")  &
     &          (char(8),ic=1,78), 'step ', i_step,                     &
     &          ' at ', time, ' is read. Backeward count is  ', icou
            if(i_step .lt. istep_start) exit
            if(ierr .gt. 0) exit
            backspace(id_write_file)
            icou = icou + 1
          end do
        end if
        write(*,*)
        write(*,*) 'Start Append'
!
        nchara_line = target_gauss_IN%num_mode * 25 + 16 + 25
        allocate(textbuf(nchara_line))
        icou = 0
        do
          icou = icou + 1
          call gz_copy_spectr_monitor_data(FPz_f1, id_append_file, id_write_file,      &
     &                              flag_gzip, zbuf, ierr)
!
          write(*,'(33a1,i12,a21)',advance="NO")                        &
     &          (char(8),ic=1,33), icou, '-th step is appended.'
          if(ierr .gt. 0) exit
        end do
        deallocate(textbuf)
        write(*,*)
!
        close(id_write_file)
        close(id_append_file)
!
        call dealloc_gauss_coef_monitor(append_gauss_IN)
        call dealloc_gauss_coef_monitor(target_gauss_IN)
!
      end subroutine append_gauss_coef_file
!
! -----------------------------------------------------------------------
!
      subroutine append_sph_mean_sq_file(flag_spectr, flag_vol_ave,     &
     &          append_file_name, target_file_name)
!
      use t_read_sph_spectra
      use t_buffer_4_gzip
      use select_gz_stream_file_IO
      use gz_spl_sph_spectr_head_IO
      use gz_spl_sph_spectr_data_IO
!
      implicit none
!
      logical, intent(in) :: flag_spectr, flag_vol_ave
      character(len=kchara), intent(in) :: append_file_name
      character(len=kchara), intent(in) :: target_file_name
!
!
      type(read_sph_spectr_data), save :: append_sph_IN
      type(read_sph_spectr_data), save :: target_sph_IN
!
      integer(kind = kint), parameter :: id_append_file = 15
      integer(kind = kint), parameter :: id_write_file = 16
!
      integer(kind = kint) :: istep_start
      real(kind = kreal) :: start_time
!
      integer(kind = kint) :: nd, ic, icou, n_line, ierr
!
      logical :: flag_gzip
      type(buffer_4_gzip), save :: zbuf
      character, pointer, save  :: FPz_f1
!
!
      write(*,*) 'Open data file to append.'
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_append_file, append_file_name, flag_gzip, zbuf)
      call select_input_sph_series_head(FPz_f1, id_append_file,         &
     &    flag_gzip, flag_current_fmt, flag_spectr, flag_vol_ave,       &
     &    append_sph_IN, zbuf)
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f1, id_append_file, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) istep_start, start_time
      write(*,*) 'zbuf%len_used', zbuf%num_word, zbuf%len_used
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_append_file, flag_gzip, zbuf)
!
!
      write(*,*) 'Open target file', ': ', trim(target_file_name)
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_write_file, target_file_name, flag_gzip, zbuf)
      call select_input_sph_series_head(FPz_f1, id_write_file,          &
     &    flag_gzip, flag_current_fmt, flag_spectr, flag_vol_ave,       &
     &    target_sph_IN, zbuf)
!
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_write_file, flag_gzip, zbuf)
!
!
      if(append_sph_IN%nri_sph .ne. target_sph_IN%nri_sph) then
        write(*,*) '# of radial layer does not match',                  &
     &      append_sph_IN%nri_sph, target_sph_IN%nri_sph
        stop
      end if
      if(append_sph_IN%ltr_sph .ne. target_sph_IN%ltr_sph) then
        write(*,*) '# of truncation does not match',                    &
     &      append_sph_IN%ltr_sph, target_sph_IN%ltr_sph
        stop
      end if
      if(append_sph_IN%ntot_sph_spec                                    &
     &      .ne. target_sph_IN%ntot_sph_spec) then
        write(*,*) '# of components in files does not match',           &
     &      append_sph_IN%ntot_sph_spec, target_sph_IN%ntot_sph_spec
        stop
      end if
!
      do nd = 1, target_sph_IN%ntot_sph_spec
        if(append_sph_IN%ene_sph_spec_name(nd)                          &
     &            .ne. target_sph_IN%ene_sph_spec_name(nd)) then
          write(*,*) nd, '-th components in files does not match',      &
     &            trim(append_sph_IN%ene_sph_spec_name(nd)), '    ',    &
     &            trim(target_sph_IN%ene_sph_spec_name(nd))
          stop
        end if
      end do
      call dealloc_sph_espec_data(append_sph_IN)
!
        if(flag_vol_ave) then
          ntot_pick = (target_sph_IN%ltr_sph+1)
        else
          ntot_pick = target_sph_IN%nri_sph * (target_sph_IN%ltr_sph+1)
        end if
        write(*,*) 'ntot_pick', ntot_pick, &
     &         target_sph_IN%nri_sph, target_sph_IN%ltr_sph
!
      call open_bwd_serch_to_append(target_file_name, id_write_file,    &
     &    istep_start, start_time, ntot_pick)
!
      write(*,*)
      write(*,*) 'Open file to append again'
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_append_file, append_file_name, flag_gzip, zbuf)
!
      call select_input_sph_series_head(FPz_f1, id_append_file,         &
     &    flag_gzip, flag_current_fmt, flag_spectr, flag_vol_ave,       &
     &    append_sph_IN, zbuf)
!
      icou = 0
      do
        icou = icou + 1
        call gz_copy_spectr_monitor_data(FPz_f1, id_append_file,        &
     &      id_write_file, flag_gzip, zbuf, ierr)
        if(ierr .gt. 0) exit
!
        write(*,'(33a1,i12,a21)',advance="NO")                          &
     &          (char(8),ic=1,33), icou, '-th step is appended.'
      end do
      write(*,*)
!
      close(id_write_file)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_append_file, flag_gzip, zbuf)
!
      call dealloc_sph_espec_data(append_sph_IN)
      call dealloc_sph_espec_data(target_sph_IN)
!
      end subroutine append_sph_mean_sq_file
!
! -----------------------------------------------------------------------
!
      subroutine append_picked_spectr_file                              &
     &         (append_file_name, target_file_name)
!
      use t_picked_sph_spectr_data_IO
      use picked_sph_spectr_data_IO
!
      implicit none
!
      character(len=kchara), intent(in) :: append_file_name
      character(len=kchara), intent(in) :: target_file_name
!
!
      type(picked_spectrum_data_IO), save :: append_pick_IO
      type(picked_spectrum_data_IO), save :: target_pick_IO
!
      integer(kind = kint), parameter :: id_append_file = 15
      integer(kind = kint), parameter :: id_write_file = 16
!
      integer(kind = kint) :: istep_start, istep_end, i_step, ierr
      real(kind = kreal) :: start_time, end_time, time
!
      integer(kind = kint) :: nd, ic, icou, ntot_pick
!
!
        write(*,*) 'Open append file', ': ', trim(append_file_name)
        open(id_append_file, file=append_file_name)
        call read_pick_series_head(id_append_file, append_pick_IO)
        call alloc_pick_sph_monitor_IO(append_pick_IO)
        call read_pick_series_comp_name(id_append_file, append_pick_IO)
        call read_sph_spectr_time(id_append_file, ione,                 &
     &                            istep_start, start_time, ierr)
        backspace(id_append_file)
!
        write(*,*) 'Open target file', ': ', trim(target_file_name)
        open(id_write_file, file=target_file_name)
        call read_pick_series_head(id_write_file, target_pick_IO)
        call alloc_pick_sph_monitor_IO(target_pick_IO)
        call read_pick_series_comp_name(id_write_file, target_pick_IO)
        close(id_write_file)
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
     &        append_pick_IO%ntot_comp, target_pick_IO%ntot_comp
          stop
        end if
!
        do nd = 1, target_pick_IO%ntot_comp
          if(append_pick_IO%spectr_name(nd)                             &
     &              .ne. target_pick_IO%spectr_name(nd)) then
            write(*,*) nd, '-th components in files does not match',    &
     &              trim(append_pick_IO%spectr_name(nd)), '    ',       &
     &              trim(target_pick_IO%spectr_name(nd))
            stop
          end if
        end do
!
        open(id_write_file, file=target_file_name, position='append')
        backspace(id_write_file)
        call read_sph_spectr_time(id_write_file, ione,                  &
     &                            istep_end, end_time, ierr)
        write(*,*) 'end step and time for target file',                 &
     &          istep_end, end_time
        write(*,*) 'start step and time for append file',               &
     &          istep_start, start_time
!
        ntot_pick = target_pick_IO%num_layer * target_pick_IO%num_mode
        icou = 0
        if(istep_start .le. istep_end) then
          do
            backspace(id_write_file)
            call read_sph_spectr_time(id_write_file, ione,              &
     &                                i_step, time, ierr)
            write(*,'(78a1,a5,i12,a4,1pe16.8e3,a29,i12)',advance="NO")  &
     &          (char(8),ic=1,78), 'step ', i_step,                     &
     &          ' at ', time, ' is read. Backward count is  ', icou
            if(i_step .lt. istep_start) exit
            if(ierr .gt. 0) exit
            do ic = 1, ntot_pick
              backspace(id_write_file)
            end do
            icou = icou + 1
          end do
        end if
        write(*,*)
        write(*,*) 'Start Append'
!
        icou = 0
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
     &       (id_write_file, i_step, time, target_pick_IO)
        end do
        write(*,*)
!
        close(id_write_file)
        close(id_append_file)
!
        call dealloc_pick_sph_monitor_IO(append_pick_IO)
        call dealloc_pick_sph_monitor_IO(target_pick_IO)
!
      end subroutine append_picked_spectr_file
!
! -----------------------------------------------------------------------
!
      subroutine open_bwd_serch_to_append(file_name, id_file, &
     &    istep_start, start_time, nline_snap)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(inout) :: nline_snap
!
      integer(kind = kint), intent(inout) :: istep_start
      real(kind = kreal) , intent(inout):: start_time
!
      integer(kind = kint) :: i_step, istep_end, ic, ierr
      real(kind = kreal) :: time, end_time
!
      integer(kind = kint) :: icou
!
!
        open(id_file, file=file_name, position='append')
        backspace(id_file)
        call read_sph_spectr_time(id_file, ione,                        &
     &                            istep_end, end_time, ierr)
!
        write(*,*) 'end step and time for target file',                 &
     &          istep_end, end_time
        write(*,*) 'start step and time for append file',               &
     &          istep_start, start_time
!
        icou = 0
        if(istep_start .le. istep_end) then
          do
            backspace(id_file)
            call read_sph_spectr_time(id_file, ione,                    &
     &                                i_step, time, ierr)
            write(*,'(78a1,a5,i12,a4,1pe16.8e3,a29,i12)',advance="NO")  &
     &          (char(8),ic=1,78), 'step ', i_step,                     &
     &          ' at ', time, ' is read. Backeward count is  ', icou
            if(i_step .lt. istep_start) exit
            if(ierr .gt. 0) exit
            do ic = 1, nline_snap
              backspace(id_file)
            end do
            icou = icou + 1
          end do
        end if
!
      end subroutine  open_bwd_serch_to_append
!
! -----------------------------------------------------------------------
!
      end program append_sph_monitor_files
