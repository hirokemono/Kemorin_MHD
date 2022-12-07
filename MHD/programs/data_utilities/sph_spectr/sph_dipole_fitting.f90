!>@file   t_average_sph_ene_spec.f90
!!        program t_average_sph_ene_spec
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Evaluate time average and standard deviation 
!!        from spherical harmonics spectrum data
!!
!!@verbatim
!! -----------------------------------------------------------------
!!  begin sph_fitted_dipole_ratio
!!    start_time_ctl     2.0
!!    end_time_ctl       2.5
!!  
!!    old_format_flag     'Off'
!!    layer_degree_spectr_file_name     'sph_pwr_layer_l.dat.gz'
!!    layer_order_spectr_file_name      'sph_pwr_layer_m.dat.gz'
!!    fitted_ratio_file_name            'fitted_ratio.dat'
!!
!!    odd_mode_only_ctl        'Yes'
!!    fit_truncation_ctl         25
!!  end sph_fitted_dipole_ratio
!! -----------------------------------------------------------------
!!@endverbatim
!
      program sph_dipole_fitting
!
      use m_precision
      use m_constants
!
      use t_buffer_4_gzip
      use t_ctl_data_dipole_fit_ratio
      use t_ctl_param_dipole_fit
      use t_read_sph_spectra
      use t_ctl_param_dipole_fit
      use t_sph_monitor_data_IO
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_spl_sph_spectr_data_IO
      use gz_layer_mean_monitor_IO
      use gz_layer_spectr_monitor_IO
      use write_sph_monitor_data
      use set_parallel_file_name
!
      implicit none
!
      type(dipole_fit_ratio_ctl), save :: d_fit_ctl1
      type(dipole_fit_ratio_data), save :: fit_dat1
!
      integer(kind = kint), parameter :: id_file_rms =    34
      integer(kind = kint), parameter :: id_file_fitted = 35
      logical, parameter :: flag_current_format = .FALSE.
      integer(kind = kint) :: ierr, ist_true, i, nri_tmp
      character, pointer :: FPz_f1
      type(read_sph_spectr_params) :: sph_IN1
      type(read_sph_spectr_params) :: sph_OUT1
      type(layer_spectr_data_IO) :: l_spec_IN1
      type(layer_mean_data_IO) :: l_mean_OUT1
      type(sph_spectr_head_labels) :: sph_lbl_IN1
!
      logical :: flag_gzip1
      type(buffer_4_gzip), save :: zbuf_s, zbuf_o
!
      real(kind = kreal) :: true_start, true_end
!
      logical, parameter :: vol_ave_on = .TRUE.
      logical, parameter :: vol_ave_off = .FALSE.
!
      integer(kind = kint) :: icou
!
!
      call read_ctl_file_dipole_fit_ratio(0, d_fit_ctl1)
      call set_sph_dipole_fit_params(d_fit_ctl1, fit_dat1)
      call reset_ctl_dipole_fit_ratio(d_fit_ctl1)
!
!
!
      write(*,*) 'Open file ', trim(fit_dat1%layer_l_spectr_file_name)
      call sel_open_read_gz_stream_file(FPz_f1, id_file_rms,            &
     &    fit_dat1%layer_l_spectr_file_name, flag_gzip1, zbuf_s)
      call s_select_input_sph_series_head                               &
     &   (FPz_f1, id_file_rms, flag_gzip1, flag_current_format,         &
     &    spectr_on, volume_off, sph_lbl_IN1, sph_IN1, zbuf_s)
!
      sph_IN1%nri_dat = sph_IN1%nri_sph
      call alloc_sph_spectr_data(sph_IN1%ltr_sph, sph_IN1)
      call alloc_layer_spectr_data_IO                                   &
     &   (sph_IN1%ntot_sph_spec, sph_IN1%ltr_sph, sph_IN1%nri_sph,      &
     &    l_spec_IN1)
!
      call init_dipole_fitting_data(sph_IN1, fit_dat1)
      call copy_read_ene_head_params(sph_IN1, sph_OUT1)
      sph_OUT1%num_time_labels = sph_OUT1%num_time_labels - 1
!
      sph_OUT1%nfield_sph_spec = 8
      sph_OUT1%ntot_sph_spec =   8
      sph_OUT1%num_labels                                               &
     &        = sph_OUT1%ntot_sph_spec + sph_OUT1%num_time_labels
!
      call alloc_sph_espec_name(sph_OUT1)
      sph_OUT1%ncomp_sph_spec(1:sph_OUT1%nfield_sph_spec) = 1
!
      call set_dipole_fitting_name(sph_IN1, sph_OUT1%num_labels,        &
     &    sph_OUT1%num_time_labels, sph_OUT1%ene_sph_spec_name)
!
      write(*,*) 'Save fitted dipole ratio data  ',                     &
     &          trim(fit_dat1%fit_ratio_file_name)
      open(id_file_fitted, file=fit_dat1%fit_ratio_file_name)
      call select_output_sph_pwr_head                                   &
     &   (.FALSE., id_file_fitted, vol_ave_off, sph_OUT1, zbuf_o)
!
      nri_tmp = sph_OUT1%nri_sph
      sph_OUT1%nri_sph = 1
      sph_OUT1%nri_dat = 1
      call alloc_sph_spectr_data(izero, sph_OUT1)
      call alloc_layer_mean_data_IO                                     &
     &   (sph_OUT1%ntot_sph_spec, sph_OUT1%nri_sph, l_mean_OUT1)
!
!
      icou = 0
      ist_true = -1
      write(*,'(a6,i12,a30,i12)',advance="NO")                          &
     &       'step= ', sph_IN1%i_step,                                  &
     &       ' averaging finished. Count=  ', icou
      do
        call sel_gz_read_layer_spectr_mtr                               &
     &      (FPz_f1, id_file_rms, flag_gzip1,                           &
     &       sph_IN1%nri_sph, sph_IN1%ltr_sph, sph_IN1%ntot_sph_spec,   &
     &       sph_IN1%i_step, sph_IN1%time, sph_IN1%kr_sph,              &
     &       sph_IN1%r_sph, sph_IN1%i_mode, l_spec_IN1%spec_r_IO,       &
     &       zbuf_s, ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN1%time .ge. fit_dat1%start_time) then
          icou = icou + 1
          sph_OUT1%time =   sph_IN1%time
          sph_OUT1%i_step = sph_IN1%i_step
          call cal_dipole_fitting_ratio                                 &
     &      (sph_IN1, l_spec_IN1%spec_r_IO, sph_OUT1%ntot_sph_spec,     &
     &        sph_OUT1%kr_sph(1), sph_OUT1%r_sph(1),                    &
     &        l_mean_OUT1%sq_r_IO(1,1), fit_dat1)
!
          call sel_gz_write_layer_mean_mtr(.FALSE., id_file_fitted,     &
     &        sph_OUT1%i_step, sph_OUT1%time,                           &
     &        sph_OUT1%nri_sph, sph_OUT1%kr_sph, sph_OUT1%r_sph,        &
     &        sph_OUT1%ntot_sph_spec, l_mean_OUT1%sq_r_IO(1,1), zbuf_o)
        end if
!
        write(*,'(60a1,a6,i12,a30,i12)',advance="NO") (char(8),i=1,60), &
     &       'step= ', sph_IN1%i_step,                                  &
     &       ' averaging finished. Count=   ', icou
        if (sph_IN1%time .ge. fit_dat1%end_time) then
          true_end = sph_IN1%time
          exit
        end if
      end do
!
   99 continue
      write(*,*)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_file_rms, flag_gzip1, zbuf_s)
      close(id_file_fitted)
!
      call dealloc_layer_spectr_data_IO(l_spec_IN1)
      call dealloc_sph_espec_data(sph_IN1)
      call dealloc_sph_espec_name(sph_IN1)
      call dealloc_layer_mean_data_IO(l_mean_OUT1)
      call dealloc_sph_espec_data(sph_OUT1)
      call dealloc_sph_espec_name(sph_OUT1)
!
      end program sph_dipole_fitting
