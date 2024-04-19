!>@file   t_ave_spectr_stable_and_rev.f90
!!        program t_ave_spectr_stable_and_rev
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
!!    volume_pwr_spectr_file_name     'sph_pwr_volume_m'
!!    gauss_coefs_file_name           'sph_spectr/gauss_coefs'
!!  end time_averaging_stable_rev
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      program t_ave_spectr_stable_and_rev
!
      use m_precision
      use m_constants
!
      use t_ctl_data_tave_stable_rev
      use t_ctl_param_sph_series_util
      use t_tave_stable_and_reversal
      use set_parallel_file_name
!
      implicit none
!
      integer(kind = kint), parameter :: id_file_rms =      34
      logical, parameter :: spectr_off =       .FALSE.
      logical, parameter :: volume_on =        .TRUE.
!>      Structure for control data
      type(tave_stable_reverse_ctl), save :: tave_svsr_ctl
!>      Structure for time averaging parametger
      type(tave_stable_and_reversal) :: tave_st_rev_p1
!
!
      call read_ctl_file_tave_stable_rev(0, tave_svsr_ctl)
!
      if(tave_svsr_ctl%i_tave_stable_reverse .ne. 1) then
        stop 'control file is broken'
      end if
!
      call set_control_stable_rev(tave_svsr_ctl, tave_st_rev_p1)
      call dealloc_ctl_tave_stable_rev(tave_svsr_ctl)
!
!
      call s_time_average_spec_stable_rev(.TRUE., tave_st_rev_p1)
!
      stop
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine s_time_average_spec_stable_rev                         &
     &         (flag_log, tave_st_rev_param)
!
      use t_buffer_4_gzip
      use t_read_sph_spectra
      use t_sph_volume_spectr_series
      use t_sph_volume_mean_series
!
      use count_monitor_time_series
      use gz_gauss_coefs_monitor_IO
      use gz_open_sph_vol_mntr_file
      use write_gauss_coefs_4_monitor
      use select_gz_stream_file_IO
      use gz_volume_spectr_monitor_IO
      use skip_comment_f
!
      logical, intent(in) :: flag_log
      type(tave_stable_and_reversal), intent(in) :: tave_st_rev_param
!
      real(kind = kreal), allocatable :: ave_gauss(:,:)
      real(kind = kreal), allocatable :: rms_gauss(:,:)
      real(kind = kreal), allocatable :: sdev_gauss(:,:)
      integer(kind = kint), allocatable :: iflag_sta(:)
      integer(kind = kint), allocatable :: iflag_rev(:)
!
      integer(kind = kint) :: num_ave_data
      real(kind = kreal), allocatable :: tave_vol_pwr(:,:)
      real(kind = kreal), allocatable :: rms_vol_pwr(:,:)
      real(kind = kreal), allocatable :: sdev_vol_pwr(:,:)
!
      character(len=kchara) :: file_name, directory
      character(len=kchara) :: fname_no_dir, fname_tmp
!
!      integer(kind = kint) :: icou
      real(kind = kreal) :: true_start, true_end, g10_mid
!>      Structure for gauss coeffciients
      type(picked_gauss_coefs_IO), save :: gauss_IO_a
      type(sph_spectr_head_labels), save :: sph_lbl_IN_p
      type(read_sph_spectr_data), save :: sph_IN_p, sph_OUT1
      type(sph_volume_spectr_series), save :: vs_srs_p
      real(kind = kreal), allocatable :: spectr_OUT(:,:)
!
      type(sph_spectr_head_labels), save :: sph_lbl_g
      type(read_sph_spectr_data), save :: sph_IN_g
      type(sph_volume_mean_series), save :: g_series
!
      character(len = 50+1) :: comment_1
      character(len = 51+1) :: comment_2
      character(len = 46+1) :: comment_3
      character(len = 47+1) :: comment_4
      character(len = 49+1) :: comment_5
      character(len = 50+1) :: comment_6
      character(len = 22+2*16+1) :: comment_7
!
      type(buffer_4_gzip), save :: zbuf_s
!
      integer(kind = kint) :: imode_g1(-1:1)
      character(len=kchara) :: hd_g10 = 'g1_0'
      character(len=kchara) :: hd_g11 = 'g1_1'
      character(len=kchara) :: hd_h11 = 'h1_1'
      character(len=2+23+25+25+1) :: comment_111
!
      integer(kind = kint) :: i, ifile
!
!       Load gauss coefficients data
      if(tave_st_rev_param%flag_old_gauss) then
        call load_gauss_coefs_time_series                               &
     &     (flag_log, tave_st_rev_param%gauss_file_name,                &
     &      tave_st_rev_param%start_time, tave_st_rev_param%end_time,   &
     &      true_start, true_end, gauss_IO_a)
        call dup_gauss_series_to_spectr(gauss_IO_a, sph_IN_g, g_series)
      else
        call load_sph_volume_mean_file                                  &
     &     (tave_st_rev_param%gauss_file_name,                          &
     &      tave_st_rev_param%start_time, tave_st_rev_param%end_time,   &
     &      true_start, true_end, sph_lbl_g, sph_IN_g, g_series)
        write(comment_111,'(2a,a23,1p2E25.15e3,a1)') '#', char(10),     &
     &             '# Start and End time:  ', true_start, true_end,     &
     &             char(10)
      end if
!
      imode_g1(-1:1) = 0
      do i = 1, sph_IN_g%nfield_sph_spec
        if(cmp_no_case(sph_IN_g%ene_sph_spec_name(i), hd_g10))          &
     &                                               imode_g1( 0) = i
        if(cmp_no_case(sph_IN_g%ene_sph_spec_name(i), hd_g11))          &
     &                                               imode_g1( 1) = i
        if(cmp_no_case(sph_IN_g%ene_sph_spec_name(i), hd_h11))          &
     &                                               imode_g1(-1) = i
      end do
      write(*,*) 'imode_g1', imode_g1( 0), imode_g1( 1), imode_g1(-1)
!
      allocate(ave_gauss(sph_IN_g%nfield_sph_spec,2))
      allocate(rms_gauss(sph_IN_g%nfield_sph_spec,2))
      allocate(sdev_gauss(sph_IN_g%nfield_sph_spec,2))
      allocate(iflag_sta(g_series%n_step))
      allocate(iflag_rev(g_series%n_step))
!
      iflag_sta = 0
      iflag_rev = 0
      do i = 1, g_series%n_step-1
        g10_mid = half * (g_series%vmean_series(imode_g1( 0),i)         &
     &                  + g_series%vmean_series(imode_g1( 0),i+1))
        if(abs(g10_mid) .le. tave_st_rev_param%border_g10)              &
     &                                        iflag_rev(i) = 1
      end do
      iflag_sta(1:g_series%n_step) = 1 - iflag_rev(1:g_series%n_step)
!
      call cal_time_ave_picked_sph_spectr                               &
     &   (g_series%n_step, g_series%d_time, iflag_sta,                  &
     &    sph_IN_g%nfield_sph_spec, g_series%vmean_series(1,1),         &
     &    ave_gauss(1,1), rms_gauss(1,1), sdev_gauss(1,1))
      call cal_time_ave_picked_sph_spectr                               &
     &   (g_series%n_step, g_series%d_time, iflag_rev,                  &
     &    sph_IN_g%nfield_sph_spec, g_series%vmean_series(1,2),         &
     &    ave_gauss(1,2), rms_gauss(1,2), sdev_gauss(1,2))
!
!
!      do icou = 1, sph_IN_g%nfield_sph_spec
!        write(*,*) icou, ave_gauss(icou,1), rms_gauss(icou,1),         &
!     &       sdev_gauss(icou,1), trim(sph_IN_g%ene_sph_spec_name(icou))
!      end do
!      do icou = 1, sph_IN_g%nfield_sph_spec
!        write(*,*) icou, ave_gauss(icou,2), rms_gauss(icou,2),         &
!     &       sdev_gauss(icou,2), trim(sph_IN_g%ene_sph_spec_name(icou))
!      end do
!
      do ifile = 1, tave_st_rev_param%num_vol_spectr_file
        write(file_name, '(a,a4)')                                      &
     &    trim(tave_st_rev_param%vol_spectr_file_prefix(ifile)), '.dat'
        write(*,*) 'load_sph_volume_spec_file: ', trim(file_name)
        call load_sph_volume_spec_file(file_name,                       &
     &      tave_st_rev_param%start_time, tave_st_rev_param%end_time,   &
     &      true_start, true_end, sph_lbl_IN_p, sph_IN_p, vs_srs_p)
!
        num_ave_data = sph_IN_p%ntot_sph_spec * (sph_IN_p%ltr_sph + 1)
        allocate(tave_vol_pwr(num_ave_data,2))
        allocate(rms_vol_pwr(num_ave_data,2))
        allocate(sdev_vol_pwr(num_ave_data,2))
!
        call cal_time_ave_picked_sph_spectr                             &
     &     (vs_srs_p%n_step, vs_srs_p%d_time, iflag_sta,                &
     &      num_ave_data,  vs_srs_p%vspec_series(1,0,1),                &
     &      tave_vol_pwr(1,1), rms_vol_pwr(1,1), sdev_vol_pwr(1,1))
        call cal_time_ave_picked_sph_spectr                             &
     &     (vs_srs_p%n_step, vs_srs_p%d_time, iflag_rev,                &
     &      num_ave_data,  vs_srs_p%vspec_series(1,0,1),                &
     &      tave_vol_pwr(1,2), rms_vol_pwr(1,2), sdev_vol_pwr(1,2))
        call dealloc_sph_volume_spec_series(vs_srs_p)
!
!
        call copy_read_ene_head_params(sph_IN_p, sph_OUT1)
        sph_OUT1%nfield_sph_spec = sph_OUT1%nfield_sph_spec + 1
        sph_OUT1%ntot_sph_spec =   sph_OUT1%ntot_sph_spec + 3
!        sph_OUT1%num_time_labels = sph_OUT1%num_time_labels
!
        call alloc_sph_espec_name(sph_OUT1)
        call alloc_sph_spectr_data(sph_IN_p%ltr_sph, sph_OUT1)
        call copy_read_ene_name_params                                  &
     &     (sph_IN_p%nfield_sph_spec, sph_IN_p%ntot_sph_spec,           &
     &      sph_OUT1%num_time_labels, sph_IN_p, sph_OUT1)
        i = sph_OUT1%num_time_labels + sph_IN_p%ntot_sph_spec
        sph_OUT1%ncomp_sph_spec(sph_IN_p%nfield_sph_spec+1) = 3
        sph_OUT1%ene_sph_spec_name(i+1) = hd_g10
        sph_OUT1%ene_sph_spec_name(i+2) = hd_g11
        sph_OUT1%ene_sph_spec_name(i+3) = hd_h11
!
        allocate(spectr_OUT(sph_OUT1%ntot_sph_spec,0:sph_OUT1%ltr_sph))
!$omp parallel workshare
        spectr_OUT(1:sph_OUT1%ntot_sph_spec,0:sph_OUT1%ltr_sph) = 0.0d0
!$omp end parallel workshare
!
        call split_directory                                            &
     &     (tave_st_rev_param%vol_spectr_file_prefix(ifile),            &
     &      directory, fname_no_dir)
        write(fname_tmp, '(a17,a,a4)')                                  &
     &      't_ave_stable_rev_', trim(fname_no_dir), '.dat'
        file_name = append_directory(directory, fname_tmp)
!
        write(*,*) 'average file_name: ', trim(file_name)
        open(id_file_rms, file=file_name, status='replace',             &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
!
        write(comment_1,'(a50,a1)')                                     &
     &  '# Step  0: average of volume mean square in stable', char(10)
        write(comment_2,'(a51,a1)')                                     &
     &  '# Step  1: average of volume mean square in reverse', char(10)
        write(comment_3,'(a46,a1)')                                     &
     &  '# Step  2: average of volume average in stable', char(10)
        write(comment_4,'(a47,a1)')                                     &
     &  '# Step  3: average of volume average in reverse', char(10)
        write(comment_5,'(a49,a1)')                                     &
     &  '# Step  4: R.M.S. of volume mean square in stable', char(10)
        write(comment_6,'(a50,a1)')                                     &
     &  '# Step  5: R.M.S. of volume mean square in reverse', char(10)
        write(comment_7,'(a22,1p2e16.8e3,a1)')                          &
     &  '# Start and End time: ', true_start, true_end, char(10)
!
        call sel_gz_write_text_stream(.FALSE., id_file_rms,             &
     &                                comment_1, zbuf_s)
        call sel_gz_write_text_stream(.FALSE., id_file_rms,             &
     &                                comment_2, zbuf_s)
        call sel_gz_write_text_stream(.FALSE., id_file_rms,             &
     &                                comment_3, zbuf_s)
        call sel_gz_write_text_stream(.FALSE., id_file_rms,             &
     &                                comment_4, zbuf_s)
        call sel_gz_write_text_stream(.FALSE., id_file_rms,             &
     &                                comment_5, zbuf_s)
        call sel_gz_write_text_stream(.FALSE., id_file_rms,             &
     &                                comment_6, zbuf_s)
        call sel_gz_write_text_stream(.FALSE., id_file_rms,             &
     &                                comment_7, zbuf_s)
!
        call write_sph_pwr_vol_head(.FALSE., id_file_rms,               &
     &                              sph_pwr_labels, sph_OUT1, zbuf_s)
!
!$omp parallel do
        do i = 0, sph_OUT1%ltr_sph
          sph_OUT1%i_mode(i) = i
        end do
!$omp end parallel do
        sph_OUT1%time = true_end
        sph_OUT1%i_step = 0
        call copy_moniter_spectr_to_IO                                  &
     &     (sph_IN_p%ntot_sph_spec, sph_IN_p%ltr_sph, tave_vol_pwr(1,1),&
     &      sph_IN_g%nfield_sph_spec, ave_gauss(1,1), imode_g1,         &
     &      spectr_OUT(1,0))
        call sel_gz_write_volume_spectr_mtr(.FALSE., id_file_rms,       &
     &      sph_OUT1%i_step, sph_OUT1%time, sph_OUT1%ltr_sph,           &
     &      sph_OUT1%ntot_sph_spec, spectr_OUT(1,0), zbuf_s)
!
        sph_OUT1%i_step = 1
        call copy_moniter_spectr_to_IO                                  &
     &     (sph_IN_p%ntot_sph_spec, sph_IN_p%ltr_sph, tave_vol_pwr(1,2),&
     &      sph_IN_g%nfield_sph_spec, ave_gauss(1,2), imode_g1,         &
     &      spectr_OUT(1,0))
        call sel_gz_write_volume_spectr_mtr(.FALSE., id_file_rms,       &
     &      sph_OUT1%i_step, sph_OUT1%time, sph_OUT1%ltr_sph,           &
     &      sph_OUT1%ntot_sph_spec, spectr_OUT(1,0), zbuf_s)
!
!
        sph_OUT1%i_step = 2
        call copy_moniter_spectr_to_IO                                  &
     &     (sph_IN_p%ntot_sph_spec, sph_IN_p%ltr_sph, rms_vol_pwr(1,1), &
     &      sph_IN_g%nfield_sph_spec, rms_gauss(1,1), imode_g1,         &
     &      spectr_OUT(1,0))
        call sel_gz_write_volume_spectr_mtr(.FALSE., id_file_rms,       &
     &      sph_OUT1%i_step, sph_OUT1%time, sph_OUT1%ltr_sph,           &
     &      sph_OUT1%ntot_sph_spec, spectr_OUT(1,0), zbuf_s)
!
        sph_OUT1%i_step = 3
        call copy_moniter_spectr_to_IO                                  &
     &     (sph_IN_p%ntot_sph_spec, sph_IN_p%ltr_sph, rms_vol_pwr(1,2), &
     &      sph_IN_g%nfield_sph_spec, rms_gauss(1,2), imode_g1,         &
     &      spectr_OUT(1,0))
        call sel_gz_write_volume_spectr_mtr(.FALSE., id_file_rms,       &
     &      sph_OUT1%i_step, sph_OUT1%time, sph_OUT1%ltr_sph,           &
     &      sph_OUT1%ntot_sph_spec, spectr_OUT(1,0), zbuf_s)
!
!
        sph_OUT1%i_step = 4
        call copy_moniter_spectr_to_IO                                  &
     &     (sph_IN_p%ntot_sph_spec, sph_IN_p%ltr_sph, sdev_vol_pwr(1,1),&
     &      sph_IN_g%nfield_sph_spec, sdev_gauss(1,1), imode_g1,        &
     &      spectr_OUT(1,0))
        call sel_gz_write_volume_spectr_mtr(.FALSE., id_file_rms,       &
     &      sph_OUT1%i_step, sph_OUT1%time, sph_OUT1%ltr_sph,           &
     &      sph_OUT1%ntot_sph_spec, spectr_OUT(1,0), zbuf_s)
!
        sph_OUT1%i_step = 5
        call copy_moniter_spectr_to_IO                                  &
     &     (sph_IN_p%ntot_sph_spec, sph_IN_p%ltr_sph, sdev_vol_pwr(1,2),&
     &      sph_IN_g%nfield_sph_spec, sdev_gauss(1,2), imode_g1,        &
     &      spectr_OUT(1,0))
        call sel_gz_write_volume_spectr_mtr(.FALSE., id_file_rms,       &
     &      sph_OUT1%i_step, sph_OUT1%time, sph_OUT1%ltr_sph,           &
     &      sph_OUT1%ntot_sph_spec, spectr_OUT(1,0), zbuf_s)
        close(id_file_rms)
!
        deallocate(sdev_vol_pwr)
        deallocate(rms_vol_pwr)
        deallocate(tave_vol_pwr)
!
        deallocate(spectr_OUT)
!
        call dealloc_sph_espec_data(sph_OUT1)
        call dealloc_sph_espec_name(sph_OUT1)
        call dealloc_sph_espec_data(sph_IN_p)
        call dealloc_sph_espec_name(sph_IN_p)
      end do
!
      end subroutine s_time_average_spec_stable_rev
!
! -------------------------------------------------------------------
!
      subroutine copy_moniter_spectr_to_IO                              &
     &         (ntot_sph_spec, ltr, time_vol_spectr,                    &
     &          ntot_gauss, tave_gauss, imode_g1, spectr_OUT)
!
      use t_read_sph_spectra
!
      integer(kind = kint), intent(in) :: imode_g1(-1:1)
      integer(kind = kint), intent(in) :: ntot_gauss
      integer(kind = kint), intent(in) :: ntot_sph_spec, ltr
      real(kind = kreal), intent(in) :: tave_gauss(ntot_gauss)
      real(kind = kreal), intent(in)                                    &
     &                   :: time_vol_spectr(ntot_sph_spec,0:ltr)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_OUT(ntot_sph_spec+3,0:ltr)
!
!
      spectr_OUT(1:ntot_sph_spec,0:ltr)                                 &
     &            = time_vol_spectr(1:ntot_sph_spec,0:ltr)
      spectr_OUT(ntot_sph_spec+1,0:ltr) = zero
      spectr_OUT(ntot_sph_spec+2,0:ltr) = zero
      spectr_OUT(ntot_sph_spec+3,0:ltr) = zero
      spectr_OUT(ntot_sph_spec+1,1) = tave_gauss(imode_g1( 0))
      spectr_OUT(ntot_sph_spec+2,1) = tave_gauss(imode_g1( 1))
      spectr_OUT(ntot_sph_spec+3,1) = tave_gauss(imode_g1(-1))
!
      end subroutine copy_moniter_spectr_to_IO
!
! -------------------------------------------------------------------
!
      end program t_ave_spectr_stable_and_rev
