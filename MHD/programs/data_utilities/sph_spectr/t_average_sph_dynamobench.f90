!>@file   t_average_sph_dynamobench.f90
!!        program t_average_sph_dynamobench
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
!!      control file: control_sph_time_average
!!
!!  begin time_averaging_sph_monitor
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    begin monitor_data_list_ctl
!!      dynamo_benchmark_file_prefix   'dynamobench'
!!    end monitor_data_list_ctl
!!  end time_averaging_sph_monitor
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      program t_average_sph_dynamobench
!
      use m_precision
      use m_constants
!
      use t_ctl_data_tave_sph_monitor
      use t_ctl_param_sph_series_util
      use t_time_ave_sph_volume_mean
      use set_parallel_file_name
!
      implicit none
!
!>        Control file name
      character(len = kchara), parameter                                &
     &           :: fname_ctl_tave_sph_mtr = 'control_sph_time_average'
!
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
      type(sph_spectr_file_param), save :: spec_evo_p1
      character(len = kchara) :: dynamobench_prefix
      character(len = kchara) :: dynamobench_fname
!
      call read_control_file_sph_monitor(0, fname_ctl_tave_sph_mtr,     &
     &                                   tave_sph_ctl1)
      call set_spec_series_time_param(tave_sph_ctl1, spec_evo_p1)
!
      if(tave_sph_ctl1%monitor_list_ctl%dynamobench_file_prefix%iflag   &
     &     .le. 0) then
        write(*,*) 'Set dynamo benchmark file prefix at ',              &
     &             'dynamo_benchmark_file_prefix.'
        stop
      end if
      dynamobench_prefix = tave_sph_ctl1%monitor_list_ctl%dynamobench_file_prefix%charavalue
      dynamobench_fname = add_dat_extension(dynamobench_prefix)
!
      call dealloc_ctl_tave_sph_monitor(tave_sph_ctl1)
!
      call time_ave_sdev_sph_dynamobench(dynamobench_fname,             &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time)
!
      call dealloc_spec_series_file_param(spec_evo_p1)
      stop
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine time_ave_sdev_sph_dynamobench(fname_org,               &
     &                                         start_time, end_time)
!
      use t_sph_volume_mean_series
      use count_monitor_time_series
      use sph_monitor_data_text
      use set_parallel_file_name
      use sph_volume_monitor_snap_IO
!'
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(in) :: start_time, end_time
!
      type(sph_spectr_head_labels), save ::   sph_lbl_IN1
      type(read_sph_spectr_data), save ::     sph_IN1
      type(sph_volume_mean_series), save ::   vm_srs1
      type(time_average_volume_mean), save :: tave_vm1
      real(kind = kreal) :: true_start, true_end
!
      character(len=2+23+25+25+1) :: comment_1
!
      integer(kind = kint) :: i
      character(len = kchara) :: ave_fname, fname_tmp
      character(len = kchara) :: directory, fname_no_dir
!
!
      call load_sph_volume_mean_file(fname_org, start_time, end_time,   &
     &    true_start, true_end, sph_lbl_IN1, sph_IN1, vm_srs1)
      write(comment_1,'(2a,a23,1p2E25.15e3,a1)') '#', char(10),         &
     &             '# Start and End time:  ', true_start, true_end,     &
     &             char(10)
!
      call check_dynamobench_drift(sph_IN1, vm_srs1%n_step, vm_srs1%d_time,        &
     &    vm_srs1%ntot_comp, vm_srs1%vmean_series)
!
      call alloc_t_average_volume_mean                                  &
     &   (vm_srs1%n_step, vm_srs1%ntot_comp, tave_vm1)
      call cal_time_ave_picked_sph_spectr                               &
     &   (vm_srs1%n_step, vm_srs1%d_time, tave_vm1%iflag_all,           &
     &    tave_vm1%ncomp_tave, vm_srs1%vmean_series,                    &
     &    tave_vm1%ave_mean, tave_vm1%rms_mean, tave_vm1%sdev_mean)
!
      write(*,'(a)') 'Dynamo benchmarks'
      write(*,'(a)') 'Average, R.M.S., standard_deviation, Item_name'
      do i = 1, tave_vm1%ncomp_tave
        write(*,'(1p3E25.15e3, 2a)')                                    &
     &       tave_vm1%ave_mean(i), tave_vm1%rms_mean(i),                &
     &       tave_vm1%sdev_mean(i), ':   ',                             &
     &       trim(sph_IN1%ene_sph_spec_name(i+sph_IN1%num_time_labels))
      end do
!
      call split_directory(fname_org, directory, fname_no_dir)
      write(fname_tmp, '(a12,a)') 't_ave_sigma_', trim(fname_no_dir)
      ave_fname = append_directory(directory, fname_tmp)
!
      call write_sph_vol_mean_tave_sdev(.FALSE., ave_fname,             &
     &    comment_1, sph_lbl_IN1, sph_IN1, tave_vm1%ncomp_tave,         &
     &    tave_vm1%ave_mean, tave_vm1%rms_mean, tave_vm1%sdev_mean)
!
      call dealloc_t_average_volume_mean(tave_vm1)
      call dealloc_sph_volume_mean_series(vm_srs1)
      call dealloc_sph_espec_name(sph_IN1)
      call dealloc_sph_espec_data(sph_IN1)
!
      end subroutine time_ave_sdev_sph_dynamobench
!
!   --------------------------------------------------------------------
!
      subroutine check_dynamobench_drift(sph_IN, num_step, time,        &
     &          num_comps, d_series)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      integer(kind = kint), intent(in) :: num_step, num_comps
      real(kind = kreal), intent(in) :: time(num_step)
!
      real(kind = kreal), intent(inout) :: d_series(num_comps,num_step)
!
      integer(kind = kint) :: icou, nd
      real(kind = kreal) :: acou, total_t
      character(len=kchara) :: tmpchara
      character(len=1) ::      lastchara
      character(len=kchara), parameter:: ref_omaga = 'omega_'
      character(len=kchara), parameter:: ref_ave_p = 'Average_drift_vr'
      integer(kind = kint) :: last, m_fold
      integer(kind = kint), allocatable :: iflag_chack(:)
      real(kind = kreal) :: total, delta, dt, ave
      real(kind = kreal), parameter :: pi = four * atan(one)
!
      allocate(iflag_chack(num_comps))
      iflag_chack(1:num_comps) = 0
!
      do nd = 1, num_comps
        icou = nd+sph_IN%num_time_labels
        last = len_trim(sph_IN%ene_sph_spec_name(icou))
        write(tmpchara, '(a6)') sph_IN%ene_sph_spec_name(icou)(1:6)
        write(lastchara,'(a1)')                                         &
     &                    sph_IN%ene_sph_spec_name(icou)(last:last)
        if(cmp_no_case(tmpchara, ref_omaga)) then
          read(lastchara,*) m_fold
          iflag_chack(nd) = 1
        end if
      end do
      do nd = 1, num_comps
        if(iflag_chack(nd) .eq. 1) cycle
!
        icou = nd+sph_IN%num_time_labels
        if(cmp_no_case(sph_IN%ene_sph_spec_name(icou), ref_ave_p))      &
     &       iflag_chack(nd) = 2
      end do
!
      do nd = 1, num_comps
        icou = nd + sph_IN%num_time_labels
        write(*,*) nd, trim(sph_IN%ene_sph_spec_name(icou)),            &
     &            iflag_chack(nd)
      end do
!
      do nd = 1, num_comps
        if(iflag_chack(nd) .eq. 0) cycle
!
        icou = nd + sph_IN%num_time_labels
        write(*,*) 'Fix drift freqency for ',                           &
     &            trim(sph_IN%ene_sph_spec_name(icou))
        write(*,*) 'count, time, original, Fixed'
        do icou = 2, num_step-1
          if(    (d_series(nd,icou-1) * d_series(nd,icou) .lt. zero)    &
     &     .and. (d_series(nd,icou) * d_series(nd,icou+1) .lt. zero))   &
     &                                                             then
            dt = time(icou) - time(icou-1)
            delta = d_series(nd,icou) * dt
            if(abs(delta+pi) .le. pi) delta = delta + pi
            if(abs(delta-pi) .le. pi) delta = delta - pi
            delta = delta / dt
!
            ave = half * (d_series(nd,icou-1) + d_series(nd,icou+1))
            if(abs(delta - ave) .gt. ave) delta = ave
!
            write(*,*) icou, time(icou), d_series(nd,icou), delta
!
            d_series(nd,icou) = delta
          end if
        end do
      end do
!
      end subroutine check_dynamobench_drift
!
! -------------------------------------------------------------------
!
      end program t_average_sph_dynamobench
