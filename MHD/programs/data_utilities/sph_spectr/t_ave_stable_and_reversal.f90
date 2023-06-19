!>@file   t_ave_stable_and_reversal.f90
!!        program t_ave_stable_and_reversal
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
!!
!!      control block for pickup spherical harmonics
!!
!!  begin time_averaging_stable_rev
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    stable_limit_g10_ctl     7e-3
!!
!!    volume_average_file_name         'sph_ave_volume'
!!    volume_mean_square_file_name     'sph_pwr_volume_s'
!!    gauss_coefs_file_name           'sph_spectr/gauss_coefs'
!!  end time_averaging_stable_rev
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      program t_ave_stable_and_reversal
!
      use m_precision
      use m_constants
!
      use t_ctl_data_tave_stable_rev
      use t_ctl_param_sph_series_util
      use set_parallel_file_name
!
      implicit none
!
      integer(kind = kint), parameter :: id_file_rms =      34
      logical, parameter :: spectr_off =       .FALSE.
      logical, parameter :: volume_on =        .TRUE.
!>      Structure for control data
      type(tave_stable_reverse_ctl), save :: tave_svsr_ctl
!
      character(len=kchara) :: vave_file_name
      character(len=kchara) :: vpwr_file_name
      character(len=kchara) :: gauss_file_name
      real(kind = kreal) :: start_time, end_time, border_g10
!
      integer :: i
!
      call read_ctl_file_tave_stable_rev(0, tave_svsr_ctl)
      if(tave_svsr_ctl%i_tave_stable_reverse .ne. 1) then
        stop 'control file is broken'
      end if
!
      call set_control_ave_gauss(tave_svsr_ctl, start_time, end_time,   &
     &    vave_file_name, vpwr_file_name, gauss_file_name,              &
     &    border_g10)
      call reset_ctl_tave_stable_rev(tave_svsr_ctl)
!
!
      call s_time_average_vol_stable_rev(.TRUE., start_time, end_time,  &
     &    vave_file_name, vpwr_file_name, gauss_file_name, border_g10)
!
      stop
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_control_ave_gauss                                  &
     &         (tave_svsr_ctl, start_time, end_time,                    &
     &          vave_file_name, vpwr_file_name, gauss_file_name,        &
     &          border_g10)
!
      use t_ctl_param_sph_series_util
!
      type(tave_stable_reverse_ctl), intent(in) :: tave_svsr_ctl
      character(len=kchara), intent(inout) :: vave_file_name
      character(len=kchara), intent(inout) :: vpwr_file_name
      character(len=kchara), intent(inout) :: gauss_file_name
      real(kind = kreal), intent(inout) :: start_time, end_time
      real(kind = kreal), intent(inout) :: border_g10
!
!
      if(tave_svsr_ctl%volume_average_file_ctl%iflag .eq. 0) then
        write(*,*) 'Set File prefix for volume average data file'
        stop
      end if
      vave_file_name = tave_svsr_ctl%volume_average_file_ctl%charavalue
!
      if(tave_svsr_ctl%volume_power_file_ctl%iflag .eq. 0) then
        write(*,*) 'Set File prefix for volume mean square data file'
        stop
      end if
      vpwr_file_name = tave_svsr_ctl%volume_power_file_ctl%charavalue
!
      if(tave_svsr_ctl%gauss_coefs_file_ctl%iflag .eq. 0) then
        write(*,*) 'Set File prefix for Gauss coefficients'
        stop
      end if
      gauss_file_name = tave_svsr_ctl%gauss_coefs_file_ctl%charavalue
!
      if(tave_svsr_ctl%start_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set start time'
        stop
      end if
      start_time = tave_svsr_ctl%start_time_ctl%realvalue
!
      if(tave_svsr_ctl%end_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set end time'
        stop
      end if
      end_time = tave_svsr_ctl%end_time_ctl%realvalue
!
      if(tave_svsr_ctl%stable_limit_g10_ctl%iflag .eq. 0) then
        write(*,*) 'Set threthold for g10'
        stop
      end if
      border_g10 = tave_svsr_ctl%stable_limit_g10_ctl%realvalue
!
      end subroutine set_control_ave_gauss
!
!   --------------------------------------------------------------------
!
      subroutine s_time_average_vol_stable_rev                          &
     &         (flag_log, start_time, end_time,                         &
     &          vave_file_name, vpwr_file_name, gauss_file_name,        &
     &          border_g10)
!
      use t_buffer_4_gzip
      use t_read_sph_spectra
      use t_sph_volume_mean_series
!
      use count_monitor_time_series
      use gz_gauss_coefs_monitor_IO
      use gz_open_sph_vol_mntr_file
      use select_gz_stream_file_IO
      use write_gauss_coefs_4_monitor
      use sph_monitor_data_text
      use skip_comment_f
!
      logical, intent(in) :: flag_log
      character(len=kchara), intent(in) :: vave_file_name
      character(len=kchara), intent(in) :: vpwr_file_name
      character(len=kchara), intent(in) :: gauss_file_name
      real(kind = kreal), intent(in) :: start_time, end_time, border_g10
!
      real(kind = kreal), allocatable :: ave_gauss(:,:)
      real(kind = kreal), allocatable :: rms_gauss(:,:)
      real(kind = kreal), allocatable :: sdev_gauss(:,:)
      integer(kind = kint), allocatable :: iflag_sta(:)
      integer(kind = kint), allocatable :: iflag_rev(:)
!
      real(kind = kreal), allocatable :: tave_vol_ave(:,:)
      real(kind = kreal), allocatable :: rms_vol_ave(:,:)
      real(kind = kreal), allocatable :: sdev_vol_ave(:,:)
      real(kind = kreal), allocatable :: tave_vol_pwr(:,:)
      real(kind = kreal), allocatable :: rms_vol_pwr(:,:)
      real(kind = kreal), allocatable :: sdev_vol_pwr(:,:)
!
      character(len=kchara) :: tave_pick_gauss_fname
      character(len=kchara) :: trms_pick_gauss_fname
      character(len=kchara) :: sdev_pick_gauss_fname
!
      character(len=kchara) :: file_name, directory, extension
      character(len=kchara) :: fname_no_dir, fname_tmp
!
!      integer(kind = kint) :: icou
      real(kind = kreal) :: true_start, true_end, g10_mid
!>      Structure for gauss coeffciients
      type(picked_gauss_coefs_IO), save :: gauss_IO_a
      type(sph_spectr_head_labels), save :: sph_lbl_IN_a, sph_lbl_IN_p
      type(read_sph_spectr_data), save :: sph_IN_a, sph_IN_p, sph_OUT1
      type(sph_volume_mean_series), save :: vm_srs_a, vm_srs_p
      real(kind = kreal), allocatable :: spectr_OUT(:)
!
      type(buffer_4_gzip), save :: zbuf_s
!
      integer(kind = kint) :: imode_g1(-1:1)
      character(len=kchara) :: hd_g10 = 'g1_0'
      character(len=kchara) :: hd_g11 = 'g1_1'
      character(len=kchara) :: hd_h11 = 'h1_1'
!
      write(tave_pick_gauss_fname,'(a6,a)')                             &
     &                           't_ave_',  trim(gauss_file_name)
      write(trms_pick_gauss_fname,'(a6,a)')                             &
     &                           't_rms_',  trim(gauss_file_name)
      write(sdev_pick_gauss_fname,'(a8,a)')                             &
     &                           't_sigma_', trim(gauss_file_name)
!
!       Load gauss coefficients data
      call load_gauss_coefs_time_series                                 &
     &   (flag_log, gauss_file_name, start_time, end_time,              &
     &    true_start, true_end, gauss_IO_a)
!
      imode_g1(-1:1) = 0
      do i = 1, gauss_IO_a%num_mode
        if(cmp_no_case(gauss_IO_a%gauss_coef_name(i), hd_g10))          &
     &                                               imode_g1( 0) = i
        if(cmp_no_case(gauss_IO_a%gauss_coef_name(i), hd_g11))          &
     &                                               imode_g1( 1) = i
        if(cmp_no_case(gauss_IO_a%gauss_coef_name(i), hd_h11))          &
     &                                               imode_g1(-1) = i
      end do
      write(*,*) 'imode_g1', imode_g1( 0), imode_g1( 1), imode_g1(-1)
!
      allocate(ave_gauss(gauss_IO_a%num_mode,2))
      allocate(rms_gauss(gauss_IO_a%num_mode,2))
      allocate(sdev_gauss(gauss_IO_a%num_mode,2))
      allocate(iflag_sta(gauss_IO_a%n_step))
      allocate(iflag_rev(gauss_IO_a%n_step))
!
      iflag_sta = 0
      iflag_rev = 0
      do i = 1, gauss_IO_a%n_step-1
        g10_mid = half * (gauss_IO_a%d_gauss(imode_g1( 0),i)            &
     &                  + gauss_IO_a%d_gauss(imode_g1( 0),i+1))
        if(abs(g10_mid) .le. border_g10) iflag_rev(i) = 1
      end do
      iflag_sta(1:gauss_IO_a%n_step)                                    &
     &                  = 1 - iflag_rev(1:gauss_IO_a%n_step)
!
      call cal_time_ave_picked_sph_spectr                               &
     &   (gauss_IO_a%n_step, gauss_IO_a%d_time, iflag_sta,              &
     &    gauss_IO_a%num_mode, gauss_IO_a%d_gauss(1,1),                 &
     &    ave_gauss(1,1), rms_gauss(1,1), sdev_gauss(1,1))
      call cal_time_ave_picked_sph_spectr                               &
     &   (gauss_IO_a%n_step, gauss_IO_a%d_time, iflag_rev,              &
     &    gauss_IO_a%num_mode, gauss_IO_a%d_gauss(1,2),                 &
     &    ave_gauss(1,2), rms_gauss(1,2), sdev_gauss(1,2))
!
!
!      do icou = 1, gauss_IO_a%num_mode
!        write(*,*) icou, ave_gauss(icou,1), rms_gauss(icou,1),         &
!     &       sdev_gauss(icou,1), trim(gauss_IO_a%gauss_coef_name(icou))
!      end do
!      do icou = 1, gauss_IO_a%num_mode
!        write(*,*) icou, ave_gauss(icou,2), rms_gauss(icou,2),         &
!     &       sdev_gauss(icou,2), trim(gauss_IO_a%gauss_coef_name(icou))
!      end do
!
      call load_sph_volume_mean_file                                    &
     &   (vave_file_name, start_time, end_time, true_start, true_end,   &
     &    sph_lbl_IN_a, sph_IN_a, vm_srs_a)

      allocate(tave_vol_ave(sph_IN_a%ntot_sph_spec,2))
      allocate(rms_vol_ave(sph_IN_a%ntot_sph_spec,2))
      allocate(sdev_vol_ave(sph_IN_a%ntot_sph_spec,2))
      call cal_time_ave_picked_sph_spectr                               &
     &   (vm_srs_a%n_step, vm_srs_a%d_time, iflag_sta,                  &
     &    sph_IN_a%ntot_sph_spec, vm_srs_a%vmean_series(1,1),           &
     &    tave_vol_ave(1,1), rms_vol_ave(1,1), sdev_vol_ave(1,1))
      call cal_time_ave_picked_sph_spectr                               &
     &   (vm_srs_a%n_step, vm_srs_a%d_time, iflag_rev,                  &
     &    sph_IN_a%ntot_sph_spec, vm_srs_a%vmean_series(1,1),           &
     &    tave_vol_ave(1,2), rms_vol_ave(1,2), sdev_vol_ave(1,2))
      call dealloc_sph_volume_mean_series(vm_srs_a)
      call dealloc_sph_espec_data(sph_IN_a)
      call dealloc_sph_espec_name(sph_IN_a)
!
!
      call load_sph_volume_mean_file                                    &
     &   (vpwr_file_name, start_time, end_time, true_start, true_end,   &
     &    sph_lbl_IN_p, sph_IN_p, vm_srs_p)
!
      allocate(tave_vol_pwr(sph_IN_p%ntot_sph_spec,2))
      allocate(rms_vol_pwr(sph_IN_p%ntot_sph_spec,2))
      allocate(sdev_vol_pwr(sph_IN_p%ntot_sph_spec,2))
      call cal_time_ave_picked_sph_spectr                               &
     &   (vm_srs_p%n_step, vm_srs_p%d_time, iflag_sta,                  &
     &    sph_IN_p%ntot_sph_spec, vm_srs_p%vmean_series(1,1),           &
     &    tave_vol_pwr(1,1), rms_vol_pwr(1,1), sdev_vol_pwr(1,1))
      call cal_time_ave_picked_sph_spectr                               &
     &   (vm_srs_p%n_step, vm_srs_p%d_time, iflag_rev,                  &
     &    sph_IN_p%ntot_sph_spec,  vm_srs_p%vmean_series(1,1),          &
     &    tave_vol_pwr(1,2), rms_vol_pwr(1,2), sdev_vol_pwr(1,2))
      call dealloc_sph_volume_mean_series(vm_srs_p)
!
!
      call copy_read_ene_head_params(sph_IN_p, sph_OUT1)
      sph_OUT1%nfield_sph_spec = sph_OUT1%nfield_sph_spec + 1
      sph_OUT1%ntot_sph_spec =   sph_OUT1%ntot_sph_spec + 3
!      sph_OUT1%num_time_labels = sph_OUT1%num_time_labels
!
      call alloc_sph_espec_name(sph_OUT1)
!
      call alloc_sph_spectr_data(izero, sph_OUT1)
      call copy_read_ene_name_params                                    &
     &   (sph_IN_p%nfield_sph_spec, sph_IN_p%ntot_sph_spec,             &
     &    sph_OUT1%num_time_labels, sph_IN_p, sph_OUT1)
      i = sph_OUT1%num_time_labels + sph_IN_p%ntot_sph_spec
      sph_OUT1%ncomp_sph_spec(sph_IN_p%nfield_sph_spec+1) = 3
      sph_OUT1%ene_sph_spec_name(i+1) = hd_g10
      sph_OUT1%ene_sph_spec_name(i+2) = hd_g11
      sph_OUT1%ene_sph_spec_name(i+3) = hd_h11
!
      allocate(spectr_OUT(sph_OUT1%ntot_sph_spec))
!$omp parallel workshare
      spectr_OUT(1:sph_OUT1%ntot_sph_spec) = 0.0d0
!$omp end parallel workshare
!
      call split_extrension(vpwr_file_name, file_name, extension)
      if(extension .eq. 'gz') then
        call split_extrension(file_name, fname_tmp, extension)
      else
        fname_tmp = file_name
      end if
      call split_directory(fname_tmp, directory, fname_no_dir)
      write(fname_tmp, '(a17,a,a4)')                                    &
     &    't_ave_stable_rev_', trim(fname_no_dir), '.dat'
      file_name = append_directory(directory, fname_tmp)
!
      write(*,*) 'average file_name: ', trim(file_name)
      open(id_file_rms, file=file_name,                                 &
     &     status='replace', FORM='UNFORMATTED', ACCESS='STREAM')
      write(id_file_rms,'(a)')                                          &
     &          '# Step  0: average of volume mean square in stable'
      write(id_file_rms,'(a)')                                          &
     &          '# Step  1: average of volume mean square in reverse'
      write(id_file_rms,'(a)')                                          &
     &          '# Step  2: R.M.S. of volume mean square in stable'
      write(id_file_rms,'(a)')                                          &
     &          '# Step  3: R.M.S. of volume mean square in reverse'
      write(id_file_rms,'(a)')                                          &
     &          '# Step  4: s.dev. of volume mean square in stable'
      write(id_file_rms,'(a)')                                          &
     &          '# Step  5: s.dev. of volume mean square in reverse'
!
      write(id_file_rms,'(a)')                                          &
     &          '# Step  6: average of volume average in stable'
      write(id_file_rms,'(a)')                                          &
     &          '# Step  7: average of volume average in reverse'
      write(id_file_rms,'(a)')                                          &
     &          '# Step  8: R.M.S. of volume average in stable'
      write(id_file_rms,'(a)')                                          &
     &          '# Step  9: R.M.S. of volume average in reverse'
      write(id_file_rms,'(a)')                                          &
     &          '# Step 10: s.dev. of volume average in stable'
      write(id_file_rms,'(a)')                                          &
     &          '# Step 11: s.dev. of volume average in reverse'
      write(id_file_rms,'(a,1p2e16.8e3)')                               &
     &          '# Start and End time: ', true_start, true_end
!
      call write_sph_pwr_vol_head(.FALSE., id_file_rms, sph_pwr_labels, &
     &                            sph_OUT1, zbuf_s)
!
      sph_OUT1%time = true_end
      sph_OUT1%i_step = 0
      call copy_moniter_aves_to_IO                                      &
     &   (sph_IN_p%ntot_sph_spec, tave_vol_pwr(1,1),                    &
     &    gauss_IO_a%num_mode, ave_gauss(1,1), imode_g1, spectr_OUT)
      call sel_gz_write_text_stream(.FALSE., id_file_rms,               &
     &    volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,          &
     &    sph_OUT1%ntot_sph_spec, spectr_OUT(1)), zbuf_s)
!
      sph_OUT1%i_step = 1
      call copy_moniter_aves_to_IO                                      &
     &   (sph_IN_p%ntot_sph_spec, tave_vol_pwr(1,2),                    &
     &    gauss_IO_a%num_mode, ave_gauss(1,2), imode_g1, spectr_OUT)
      call sel_gz_write_text_stream(.FALSE., id_file_rms,               &
     &    volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,          &
     &    sph_OUT1%ntot_sph_spec, spectr_OUT(1)), zbuf_s)
!
!
      sph_OUT1%i_step = 2
      call copy_moniter_aves_to_IO                                      &
     &   (sph_IN_p%ntot_sph_spec, rms_vol_pwr(1,1),                     &
     &    gauss_IO_a%num_mode, rms_gauss(1,1), imode_g1, spectr_OUT)
      call sel_gz_write_text_stream(.FALSE., id_file_rms,               &
     &    volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,          &
     &    sph_OUT1%ntot_sph_spec, spectr_OUT(1)), zbuf_s)
!
      sph_OUT1%i_step = 3
      call copy_moniter_aves_to_IO                                      &
     &   (sph_IN_p%ntot_sph_spec, rms_vol_pwr(1,2),                     &
     &    gauss_IO_a%num_mode, rms_gauss(1,2), imode_g1, spectr_OUT)
      call sel_gz_write_text_stream(.FALSE., id_file_rms,               &
     &    volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,          &
     &    sph_OUT1%ntot_sph_spec, spectr_OUT(1)), zbuf_s)
!
!
      sph_OUT1%i_step = 4
      call copy_moniter_aves_to_IO                                      &
     &   (sph_IN_p%ntot_sph_spec, sdev_vol_pwr(1,1),                    &
     &    gauss_IO_a%num_mode, sdev_gauss(1,1), imode_g1, spectr_OUT)
      call sel_gz_write_text_stream(.FALSE., id_file_rms,               &
     &    volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,          &
     &    sph_OUT1%ntot_sph_spec, spectr_OUT(1)), zbuf_s)
!
      sph_OUT1%i_step = 5
      call copy_moniter_aves_to_IO                                      &
     &   (sph_IN_p%ntot_sph_spec, sdev_vol_pwr(1,2),                    &
     &    gauss_IO_a%num_mode, sdev_gauss(1,2), imode_g1, spectr_OUT)
      call sel_gz_write_text_stream(.FALSE., id_file_rms,               &
     &    volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,          &
     &    sph_OUT1%ntot_sph_spec, spectr_OUT(1)), zbuf_s)
!
!
      sph_OUT1%i_step = 6
      call copy_moniter_aves_to_IO                                      &
     &   (sph_IN_p%ntot_sph_spec, tave_vol_ave(1,1),                    &
     &    gauss_IO_a%num_mode, ave_gauss(1,1), imode_g1, spectr_OUT)
      call sel_gz_write_text_stream(.FALSE., id_file_rms,               &
     &    volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,          &
     &    sph_OUT1%ntot_sph_spec, spectr_OUT(1)), zbuf_s)
!
      sph_OUT1%i_step = 7
      call copy_moniter_aves_to_IO                                      &
     &   (sph_IN_p%ntot_sph_spec, tave_vol_ave(1,2),                    &
     &    gauss_IO_a%num_mode, ave_gauss(1,2), imode_g1, spectr_OUT)
      call sel_gz_write_text_stream(.FALSE., id_file_rms,               &
     &    volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,          &
     &    sph_OUT1%ntot_sph_spec, spectr_OUT(1)), zbuf_s)
!
!
      sph_OUT1%i_step = 8
      call copy_moniter_aves_to_IO                                      &
     &   (sph_IN_p%ntot_sph_spec, rms_vol_ave(1,1),                     &
     &    gauss_IO_a%num_mode, rms_gauss(1,1), imode_g1, spectr_OUT)
      call sel_gz_write_text_stream(.FALSE., id_file_rms,               &
     &    volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,          &
     &    sph_OUT1%ntot_sph_spec, spectr_OUT(1)), zbuf_s)
!
      sph_OUT1%i_step = 9
      call copy_moniter_aves_to_IO                                      &
     &   (sph_IN_p%ntot_sph_spec, rms_vol_ave(1,2),                     &
     &    gauss_IO_a%num_mode, rms_gauss(1,2), imode_g1, spectr_OUT)
      call sel_gz_write_text_stream(.FALSE., id_file_rms,               &
     &    volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,          &
     &    sph_OUT1%ntot_sph_spec, spectr_OUT(1)), zbuf_s)
!
!
      sph_OUT1%i_step = 10
      call copy_moniter_aves_to_IO                                      &
     &   (sph_IN_p%ntot_sph_spec, sdev_vol_ave(1,1),                    &
     &    gauss_IO_a%num_mode, sdev_gauss(1,1), imode_g1, spectr_OUT)
      call sel_gz_write_text_stream(.FALSE., id_file_rms,               &
     &    volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,          &
     &    sph_OUT1%ntot_sph_spec, spectr_OUT(1)), zbuf_s)
!
      sph_OUT1%i_step = 11
      call copy_moniter_aves_to_IO                                      &
     &   (sph_IN_p%ntot_sph_spec, sdev_vol_ave(1,2),                    &
     &    gauss_IO_a%num_mode, sdev_gauss(1,2), imode_g1, spectr_OUT)
      call sel_gz_write_text_stream(.FALSE., id_file_rms,               &
     &    volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,          &
     &    sph_OUT1%ntot_sph_spec, spectr_OUT(1)), zbuf_s)
!
      close(id_file_rms)
      deallocate(spectr_OUT)
!
      end subroutine s_time_average_vol_stable_rev
!
! -------------------------------------------------------------------
!
      subroutine copy_moniter_aves_to_IO(ntot_sph_spec, time_vol_ave,   &
     &          ntot_gauss, tave_gauss, imode_g1, spectr_OUT)
!
      use t_read_sph_spectra
!
      integer(kind = kint), intent(in) :: imode_g1(-1:1)
      integer(kind = kint), intent(in) :: ntot_gauss, ntot_sph_spec
      real(kind = kreal), intent(in) :: tave_gauss(ntot_gauss)
      real(kind = kreal), intent(in) :: time_vol_ave(ntot_sph_spec)
!
      real(kind = kreal), intent(inout) :: spectr_OUT(ntot_sph_spec+3)
!
      spectr_OUT(1:ntot_sph_spec) = time_vol_ave(1:ntot_sph_spec)
      spectr_OUT(ntot_sph_spec+1) = tave_gauss(imode_g1( 0))
      spectr_OUT(ntot_sph_spec+2) = tave_gauss(imode_g1( 1))
      spectr_OUT(ntot_sph_spec+3) = tave_gauss(imode_g1(-1))
!
      end subroutine copy_moniter_aves_to_IO
!
! -------------------------------------------------------------------
!
      end program t_ave_stable_and_reversal
