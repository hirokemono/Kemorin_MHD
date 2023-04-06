!>@file   trim_sph_monitor_files.f90
!!        program trim_sph_monitor_files
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!> @brief Program to trim spherical harmonics monitor files
!!
!!@verbatim
!! -----------------------------------------------------------------
!!
!!      control file name: control_trim_monitor
!!
!!  begin monitor_data_connect_ctl
!!    folder_to_read_ctl    'no02'
!!    folder_to_add_ctl     'monitor'
!!
!!    begin monitor_file_list_ctl
!!      array vol_integrate_prefix
!!        vol_integrate_prefix     'sph_ave_volume'
!!        ...
!!      end array vol_integrate_prefix
!!
!!      array vol_spectr_prefix
!!        vol_spectr_prefix     'sph_pwr_volume_l'
!!        ...
!!      end array vol_spectr_prefix
!!
!!      array sph_integrate_prefix
!!        sph_integrate_prefix     'sph_pwr_layer_s'
!!        ...
!!      end array sph_integrate_prefix
!!
!!      array layer_sph_spectr_prefix
!!        layer_sph_spectr_prefix     'sph_pwr_layer_l'
!!        ...
!!      end array layer_sph_spectr_prefix
!!
!!      array picked_sph_prefix
!!        picked_sph_prefix        'monitor/picked_mode'
!!        ...
!!      end array picked_sph_prefix
!!    end monitor_file_list_ctl
!!  end monitor_data_connect_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      program trim_sph_monitor_files
!
      use m_precision
      use m_constants
      use t_ctl_data_append_sph_mntr
      use t_ctl_data_sph_monitor_list
      use t_ctl_param_sph_series_util
      use t_read_sph_spectra
      use t_append_picked_spectr_file
!
      use set_control_4_pickup_sph
      use count_monitor_time_series
      use set_parallel_file_name
      use trim_sph_volume_mean
      use append_sph_volume_spectr
      use append_sph_layer_mean
      use append_sph_layer_spectr
!
      implicit none
!
      character(len=kchara), parameter                                  &
     &                      :: ctl_file_name = 'control_trim_monitor'
!>      Structure for control data
      type(ctl_data_add_sph_monitor), save :: add_mtr_ctl1
      type(sph_spectr_file_param), save :: spec_evo_append
      type(sph_spectr_file_param), save :: spec_evo_target
      real(kind = kreal) :: trim_end_time = 0.0d0
!
      integer(kind = kint) :: i 
!
!
      call read_ctl_file_add_sph_mntr(ctl_file_name, add_mtr_ctl1)
!
      call set_spec_series_file_param(add_mtr_ctl1%folder_to_read_ctl,  &
     &    add_mtr_ctl1%monitor_list_ctl, spec_evo_append)
      call set_spec_series_file_param(add_mtr_ctl1%folder_to_add_ctl,   &
     &    add_mtr_ctl1%monitor_list_ctl, spec_evo_target)
!
      if(add_mtr_ctl1%end_time_ctl%iflag .eq. 0) then
        write(*,*) 'set end_time_ctl to trim'
        stop
      end if
      trim_end_time = add_mtr_ctl1%end_time_ctl%realvalue
!      write(*,*) 'trim_end_time', trim_end_time
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
!
      do i = 1, spec_evo_target%vol_series%num_file
        call trim_sph_volume_mean_file                                  &
     &     (spec_evo_append%vol_series%evo_file_name(i),                &
     &      spec_evo_target%vol_series%evo_file_name(i), trim_end_time)
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
        call trim_sph_volume_spectr_file                                &
     &     (spec_evo_append%vol_spec_series%evo_file_name(i),           &
     &      spec_evo_target%vol_spec_series%evo_file_name(i),           &
     &      trim_end_time)
      end do
      stop
!
      if(spec_evo_append%layer_series%num_file                          &
     &     .ne. spec_evo_target%layer_series%num_file) then
        write(*,*) '# of file of sphere average data does not match.',  &
     &            spec_evo_append%layer_series%num_file,                &
     &            spec_evo_target%layer_series%num_file
        stop
      end if
      do i = 1, spec_evo_target%layer_series%num_file
        call append_sph_layer_mean_file                                 &
     &     (spec_evo_append%layer_series%evo_file_name(i),              &
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
        call append_sph_layer_spectr_file                               &
     &     (spec_evo_append%layer_spec_series%evo_file_name(i),         &
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
      end program trim_sph_monitor_files
