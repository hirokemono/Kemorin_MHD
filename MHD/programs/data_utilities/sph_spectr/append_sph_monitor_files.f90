!
!
!
      program append_sph_monitor_files
!
      use m_precision
      use m_constants
      use t_ctl_data_append_sph_mntr
      use t_ctl_data_sph_monitor_list
      use t_ctl_param_sph_series_util
      use t_read_sph_spectra
      use t_append_sph_mean_sq_data
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
      integer(kind = kint) :: iflag_gauss
      character(len=kchara) :: append_gauss_name
      character(len=kchara) :: target_gauss_name
!
      integer(kind = kint) :: iflag_nusselt
      character(len=kchara) :: append_Nusselt_name
      character(len=kchara) :: target_Nusselt_name
!
      integer(kind = kint), parameter :: id_append_file = 15
!
      integer(kind = kint) :: i 
!
      call read_ctl_file_add_sph_mntr(ctl_file_name, add_mtr_ctl1)
!
      call set_spec_series_file_param(add_mtr_ctl1%folder_to_read_ctl,  &
     &    add_mtr_ctl1%read_monitor_fmt_ctl,                            &
     &    add_mtr_ctl1%monitor_list_ctl, spec_evo_append)
      call set_spec_series_file_param(add_mtr_ctl1%folder_to_add_ctl,   &
     &    dummy_item, add_mtr_ctl1%monitor_list_ctl,                    &
     &    spec_evo_target)
!
      iflag_gauss                                                       &
     &    = add_mtr_ctl1%monitor_list_ctl%gauss_coefs_prefix%iflag
      if(iflag_gauss .gt. 0) then
        call set_sph_series_file_name(add_mtr_ctl1%folder_to_read_ctl,  &
     &     add_mtr_ctl1%read_monitor_fmt_ctl,                           &
     &     add_mtr_ctl1%monitor_list_ctl%gauss_coefs_prefix%charavalue, &
     &     append_gauss_name)
        call set_sph_series_file_name                                   &
     &    (add_mtr_ctl1%folder_to_add_ctl, dummy_item,                  &
     &     add_mtr_ctl1%monitor_list_ctl%gauss_coefs_prefix%charavalue, &
     &     target_gauss_name)
      end if
!
      iflag_nusselt                                                     &
     &    = add_mtr_ctl1%monitor_list_ctl%Nusselt_file_prefix%iflag
      if(iflag_nusselt .gt. 0) then
        call set_sph_series_file_name(add_mtr_ctl1%folder_to_read_ctl,  &
     &    add_mtr_ctl1%read_monitor_fmt_ctl,                            &
     &    add_mtr_ctl1%monitor_list_ctl%Nusselt_file_prefix%charavalue, &
     &    append_Nusselt_name)
        call set_sph_series_file_name                                   &
     &   (add_mtr_ctl1%folder_to_add_ctl, dummy_item,                   &
     &    add_mtr_ctl1%monitor_list_ctl%Nusselt_file_prefix%charavalue, &
     &    target_Nusselt_name)
      end if
!
!
      call dealloc_ctl_data_add_sph_mntr(add_mtr_ctl1)
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
      if(iflag_gauss .gt. 0) then
        call append_gauss_coef_file                                     &
     &     (append_gauss_name, target_gauss_name)
      end if
!
      if(iflag_nusselt .gt. 0) then
        call append_Nusselt_file                                        &
     &     (append_Nusselt_name, target_Nusselt_name)
      end if
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
      use select_gz_stream_file_IO
      use gz_gauss_coefs_monitor_IO
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
      integer(kind = kint) :: istep_start
      real(kind = kreal) :: start_time
!
      integer(kind = kint) :: nd
!
      logical :: flag_gzip1
      type(buffer_4_gzip), save :: zbuf1
      character, pointer, save  :: FPz_f1
!
!
      write(*,*) 'Open file to append.'
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_append_file, append_file_name, flag_gzip1, zbuf1)
      call read_gauss_coefs_header(FPz_f1, id_append_file,              &
     &                             flag_gzip1, append_gauss_IN, zbuf1)
      call alloc_gauss_coef_monitor(append_gauss_IN)
      call read_gauss_coefs_labels(FPz_f1, id_append_file,              &
     &                             flag_gzip1, append_gauss_IN, zbuf1)
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
      read(zbuf1%fixbuf(1),*) istep_start, start_time
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
!
!
      write(*,*) 'Open file to write', ': ', trim(target_file_name)
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_write_file, target_file_name, flag_gzip1, zbuf1)
      call read_gauss_coefs_header(FPz_f1, id_write_file,               &
     &                             flag_gzip1, target_gauss_IN, zbuf1)
      call alloc_gauss_coef_monitor(target_gauss_IN)
      call read_gauss_coefs_labels(FPz_f1, id_write_file,               &
     &                             flag_gzip1, target_gauss_IN, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_write_file, flag_gzip1, zbuf1)
!
      if(append_gauss_IN%radius_gauss                                   &
     &       .ne. target_gauss_IN%radius_gauss) then
        write(*,*) 'Radius does not match',                             &
     &     append_gauss_IN%radius_gauss, target_gauss_IN%radius_gauss
        stop
      end if
      if(append_gauss_IN%num_mode .ne. target_gauss_IN%num_mode) then
        write(*,*) '# of gauss coefficients does not match',            &
     &      append_gauss_IN%num_mode, target_gauss_IN%num_mode
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
      call open_bwd_serch_to_append(target_file_name, id_write_file,    &
     &    istep_start, start_time, ione)

      write(*,*) 'Start Append'
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_append_file, append_file_name, flag_gzip1, zbuf1)
      call read_gauss_coefs_header(FPz_f1, id_append_file,              &
     &                             flag_gzip1, append_gauss_IN, zbuf1)
      call read_gauss_coefs_labels(FPz_f1, id_append_file,              &
     &                             flag_gzip1, append_gauss_IN, zbuf1)
!
      call copy_sph_monitor_to_end(FPz_f1, id_append_file, flag_gzip1,  &
     &                             id_write_file, ione, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
      close(id_write_file)
!
      call dealloc_gauss_coef_monitor(append_gauss_IN)
      call dealloc_gauss_coef_monitor(target_gauss_IN)
!
      end subroutine append_gauss_coef_file
!
! -----------------------------------------------------------------------
!
      subroutine append_Nusselt_file                                    &
     &         (append_file_name, target_file_name)
!
      use select_gz_stream_file_IO
      use gz_Nusselt_monitor_IO
      use gz_spl_sph_spectr_data_IO
!
      implicit none
!
      character(len=kchara), intent(in) :: append_file_name
      character(len=kchara), intent(in) :: target_file_name
!
!
      type(nusselt_number_data), save :: append_Nu_type
      type(nusselt_number_data), save :: target_NU_type
!
      integer(kind = kint), parameter :: id_append_file = 15
      integer(kind = kint), parameter :: id_write_file = 16
!
      integer(kind = kint) :: istep_start
      real(kind = kreal) :: start_time
!
      logical :: flag_gzip1
      type(buffer_4_gzip), save :: zbuf1
      character, pointer, save  :: FPz_f1
!
!
      write(*,*) 'Open file to append.'
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_append_file, append_file_name, flag_gzip1, zbuf1)
      call read_Nusselt_header(FPz_f1, id_append_file,                  &
     &                         flag_gzip1, append_Nu_type, zbuf1)
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
      read(zbuf1%fixbuf(1),*) istep_start, start_time
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
!
!
      write(*,*) 'Open file to write', ': ', trim(target_file_name)
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_write_file, target_file_name, flag_gzip1, zbuf1)
      call read_Nusselt_header(FPz_f1, id_write_file,                   &
     &                         flag_gzip1, target_NU_type, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_write_file, flag_gzip1, zbuf1)
!
      if(append_Nu_type%Nu_ICB .ne. target_NU_type%Nu_ICB) then
        write(*,*) 'ICB Radius does not match',                         &
     &     append_Nu_type%Nu_ICB, target_NU_type%Nu_ICB
        stop
      end if
      if(append_Nu_type%Nu_CMB .ne. target_NU_type%Nu_CMB) then
        write(*,*) 'CMB Radius does not match',                         &
     &     append_Nu_type%Nu_CMB, target_NU_type%Nu_CMB
        stop
      end if
!
      call open_bwd_serch_to_append(target_file_name, id_write_file,    &
     &    istep_start, start_time, ione)

      write(*,*) 'Start Append'
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_append_file, append_file_name, flag_gzip1, zbuf1)
      call read_Nusselt_header(FPz_f1, id_append_file,                  &
     &                         flag_gzip1, append_Nu_type, zbuf1)
!
      call copy_sph_monitor_to_end(FPz_f1, id_append_file, flag_gzip1,  &
     &                             id_write_file, ione, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
      close(id_write_file)
!
      end subroutine append_Nusselt_file
!
! -----------------------------------------------------------------------
!
      subroutine append_picked_spectr_file                              &
     &         (append_file_name, target_file_name)
!
      use select_gz_stream_file_IO
      use t_picked_sph_spectr_data_IO
      use picked_sph_spectr_data_IO
      use gz_spl_sph_spectr_data_IO
!
      implicit none
!
      character(len=kchara), intent(in) :: append_file_name
      character(len=kchara), intent(in) :: target_file_name
!
!
      type(picked_spectrum_data_IO), save :: read_pick_IO
      type(picked_spectrum_data_IO), save :: write_pick_IO
!
      integer(kind = kint), parameter :: id_append_file = 15
      integer(kind = kint), parameter :: id_write_file = 16
!
      character, pointer :: FPz_f1
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1
!
      integer(kind = kint) :: istep_start
      real(kind = kreal) :: start_time
!
      integer(kind = kint) :: nd, nline_snap
!
!
      write(*,*) 'Open append file', ': ', trim(append_file_name)
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_append_file, append_file_name, flag_gzip1, zbuf1)
      call read_pick_series_head(FPz_f1, id_append_file, flag_gzip1,    &
     &                           read_pick_IO, zbuf1)
      call alloc_pick_sph_monitor_IO(read_pick_IO)
      call read_pick_series_comp_name                                   &
     &   (FPz_f1, id_append_file, flag_gzip1, read_pick_IO, zbuf1)
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
      read(zbuf1%fixbuf(1),*) istep_start, start_time
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
!
      write(*,*) 'Open target file', ': ', trim(target_file_name)
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_write_file, target_file_name, flag_gzip1, zbuf1)
      call read_pick_series_head(FPz_f1, id_write_file, flag_gzip1,     &
     &                           write_pick_IO, zbuf1)
      call alloc_pick_sph_monitor_IO(write_pick_IO)
      call read_pick_series_comp_name                                   &
     &   (FPz_f1, id_write_file, flag_gzip1, write_pick_IO, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_write_file, flag_gzip1, zbuf1)
!
      if(read_pick_IO%num_layer .ne. write_pick_IO%num_layer) then
        write(*,*) '# of radial layer does not match',                  &
     &        read_pick_IO%num_layer, write_pick_IO%num_layer
        stop
      end if
      if(read_pick_IO%num_mode .ne. write_pick_IO%num_mode) then
        write(*,*) '# of spherical harmonics mode does not match',      &
     &      read_pick_IO%num_mode, write_pick_IO%num_mode
        stop
      end if
      if(read_pick_IO%ntot_comp .ne. write_pick_IO%ntot_comp) then
        write(*,*) '# of components in files does not match',           &
     &      read_pick_IO%ntot_comp, write_pick_IO%ntot_comp
        stop
      end if
!
      do nd = 1, write_pick_IO%ntot_comp
        if(read_pick_IO%spectr_name(nd)                                 &
     &            .ne. write_pick_IO%spectr_name(nd)) then
          write(*,*) nd, '-th components in files does not match',      &
     &            trim(read_pick_IO%spectr_name(nd)), '    ',           &
     &            trim(write_pick_IO%spectr_name(nd))
          stop
        end if
      end do
!
      nline_snap = read_pick_IO%num_mode * read_pick_IO%num_layer
      call open_bwd_serch_to_append(target_file_name, id_write_file,    &
     &    istep_start, start_time, nline_snap)
!
      write(*,*) 'Open file to append again'
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_append_file, append_file_name, flag_gzip1, zbuf1)
      call read_pick_series_head(FPz_f1, id_append_file, flag_gzip1,    &
     &                           read_pick_IO, zbuf1)
      call read_pick_series_comp_name                                   &
     &   (FPz_f1, id_append_file, flag_gzip1, read_pick_IO, zbuf1)
!
      call copy_sph_monitor_to_end(FPz_f1, id_append_file, flag_gzip1,  &
     &                             id_write_file, nline_snap, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
      close(id_write_file)
!
      call dealloc_pick_sph_monitor_IO(read_pick_IO)
      call dealloc_pick_sph_monitor_IO(write_pick_IO)
!
      end subroutine append_picked_spectr_file
!
! -----------------------------------------------------------------------
!
      end program append_sph_monitor_files
