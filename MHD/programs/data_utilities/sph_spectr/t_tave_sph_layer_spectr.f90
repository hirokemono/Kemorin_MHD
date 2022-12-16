!>@file   t_tave_sph_layer_spectr.f90
!!        module t_tave_sph_layer_spectr
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine time_ave_sdev_sph_layer_spec                         &
!!     &         (fname_org, start_time, end_time)
!!        character(len = kchara), intent(in) :: fname_org
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!      subroutine read_time_ave_sph_layer_spec(fname_org, sph_lbl_IN,  &
!!     &          sph_IN, tave_spectr, trms_spectr, sdev_spectr)
!!        character(len = kchara), intent(in) :: fname_org
!!        type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: tave_spectr(sph_IN%ntot_sph_spec,         &
!!     &                                0:sph_IN%ltr_sph,sph_IN%nri_sph)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: trms_spectr(sph_IN%ntot_sph_spec,         &
!!     &                                0:sph_IN%ltr_sph,sph_IN%nri_sph)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: sdev_spectr(sph_IN%ntot_sph_spec,         &
!!     &                                0:sph_IN%ltr_sph,sph_IN%nri_sph)
!!@endverbatim
!
      module t_tave_sph_layer_spectr
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
      use t_buffer_4_gzip
!
      implicit none
!
!
      integer(kind = kint), parameter :: id_file_rms = 34
      integer(kind = kint), parameter :: id_read_rms = 45
!
      type layer_spectr_ave_sigma_work
        real(kind = kreal), allocatable :: read_spec(:,:,:)
!
        real(kind = kreal), allocatable :: ave_spec_l(:,:,:)
        real(kind = kreal), allocatable :: rms_spec_l(:,:,:)
        real(kind = kreal), allocatable :: sigma_spec_l(:,:,:)
        real(kind = kreal), allocatable :: ave_pre_l(:,:,:)
        real(kind = kreal), allocatable :: rms_pre_l(:,:,:)
        real(kind = kreal), allocatable :: sigma_pre_l(:,:,:)
      end type layer_spectr_ave_sigma_work
!
      logical, parameter, private :: flag_current_format = .FALSE.
!
      type(read_sph_spectr_data), save, private :: sph_IN1
      type(layer_spectr_ave_sigma_work), save, private :: WK_tave1
      type(sph_spectr_head_labels), save, private :: sph_lbl_IN1
!
      private :: id_file_rms
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine time_ave_sdev_sph_layer_spec                           &
     &         (fname_org, start_time, end_time)
!
      use set_parallel_file_name
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use count_monitor_time_series
      use gz_open_sph_monitor_file
      use gz_layer_spectr_monitor_IO
      use sph_layer_monitor_snap_IO
!
!
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(in) :: start_time, end_time
!
      type(read_sph_spectr_data), save :: sph_IN2
      real(kind = kreal) :: true_start, true_end
      integer(kind = kint) :: n_line, icou_skip
!
      character, pointer :: FPz_f1
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1
!
      character(len = kchara) :: tave_file_name, trms_file_name
      character(len = kchara) :: sdev_file_name
      character(len = kchara) :: directory, fname_no_dir, fname_tmp
      character(len=2+23+25+25+1) :: comment_1
!
!
      write(comment_1,'(2a,a23,1p2E25.15e3,a1)') '#', char(10),         &
     &             '# Start and End time:  ', true_start, true_end,     &
     &             char(10)
!
      call split_directory(fname_org, directory, fname_no_dir)
!
      write(fname_tmp, '(a6,a)') 't_ave_', trim(fname_no_dir)
      tave_file_name = append_directory(directory, fname_tmp)
      write(fname_tmp, '(a6,a)') 't_rms_', trim(fname_no_dir)
      trms_file_name = append_directory(directory, fname_tmp)
      write(fname_tmp, '(a7,a)') 't_sdev_', trim(fname_no_dir)
      sdev_file_name = append_directory(directory, fname_tmp)
!
!
      call sel_open_read_gz_stream_file(FPz_f1, id_read_rms,            &
     &                                    fname_org, flag_gzip1, zbuf1)
!
      sph_IN1%num_time_labels = 5
      call gz_read_sph_pwr_layer_head(FPz_f1, id_read_rms, flag_gzip1,  &
     &                                sph_lbl_IN1, sph_IN1, zbuf1)
!
      call alloc_sph_espec_name(sph_IN1)
      call sel_read_sph_spectr_name(FPz_f1, id_read_rms, flag_gzip1,    &
     &    sph_IN1%nfield_sph_spec, sph_IN1%num_labels,                  &
     &    sph_IN1%ncomp_sph_spec, sph_IN1%ene_sph_spec_name, zbuf1)
!
      sph_IN1%nri_dat = sph_IN1%nri_sph
      call alloc_sph_spectr_data(sph_IN1%ltr_sph, sph_IN1)
!
      n_line = (sph_IN1%ltr_sph+1) * sph_IN1%nri_sph
      call s_count_monitor_time_start                                   &
     &   (.TRUE., FPz_f1, id_read_rms, flag_gzip1, n_line,              &
     &    start_time, icou_skip, zbuf1)
!
!
      call alloc_tave_sph_data(sph_IN1, WK_tave1)
      call sel_redwind_gz_stream_file(FPz_f1, id_read_rms, flag_gzip1)
!
      sph_IN2%num_time_labels = sph_IN1%num_time_labels
      call gz_read_sph_pwr_layer_head(FPz_f1, id_read_rms, flag_gzip1,  &
     &                                sph_lbl_IN1, sph_IN2, zbuf1)
      call sel_read_sph_spectr_name(FPz_f1, id_read_rms, flag_gzip1,    &
     &    sph_IN1%nfield_sph_spec, sph_IN1%num_labels,                  &
     &    sph_IN1%ncomp_sph_spec, sph_IN1%ene_sph_spec_name, zbuf1)
!
      call s_skip_monitor_time_series(.TRUE., FPz_f1, id_read_rms,      &
     &    flag_gzip1, n_line, icou_skip, zbuf1)
      call sph_layer_spectr_average                                     &
     &   (FPz_f1, id_read_rms, flag_gzip1, flag_current_format,         &
     &    start_time, end_time, true_start, true_end,                   &
     &    sph_IN1, WK_tave1, zbuf1)
!
!
      call sel_redwind_gz_stream_file(FPz_f1, id_read_rms, flag_gzip1)
!
      sph_IN2%num_time_labels = sph_IN1%num_time_labels
      call gz_read_sph_pwr_layer_head(FPz_f1, id_read_rms, flag_gzip1,  &
     &                                sph_lbl_IN1, sph_IN2, zbuf1)
      call sel_read_sph_spectr_name(FPz_f1, id_read_rms, flag_gzip1,    &
     &    sph_IN1%nfield_sph_spec, sph_IN1%num_labels,                  &
     &    sph_IN1%ncomp_sph_spec, sph_IN1%ene_sph_spec_name, zbuf1)
!
      call s_skip_monitor_time_series(.TRUE., FPz_f1, id_read_rms,      &
     &    flag_gzip1, n_line, icou_skip, zbuf1)
      call sph_layer_spectr_std_deviation                               &
     &   (FPz_f1, id_read_rms, flag_gzip1, flag_current_format,         &
     &    start_time, end_time, sph_IN1, WK_tave1, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_read_rms, flag_gzip1, zbuf1)
!
!  Output average
      call write_sph_layer_spectr_snapshot(.FALSE., tave_file_name,     &
     &    comment_1, sph_IN1, WK_tave1%ave_spec_l(1,0,1))
!  Output RMS
      call write_sph_layer_spectr_snapshot(.FALSE., trms_file_name,     &
     &    comment_1, sph_IN1, WK_tave1%rms_spec_l(1,0,1))
!  Output Standard deviation
      call write_sph_layer_spectr_snapshot(.FALSE., trms_file_name,     &
     &    comment_1, sph_IN1, WK_tave1%sigma_spec_l(1,0,1))
!
      write(*,'(a,1p2e25.15e3)') 'Start and end time:     ',            &
     &                          true_start, true_end
!      call check_time_ave_sph_layer_spec(sph_IN1, WK_tave1)
!
      call dealloc_tave_sph_data(WK_tave1)
      call dealloc_sph_espec_data(sph_IN1)
      call dealloc_sph_espec_name(sph_IN1)
!
      end subroutine time_ave_sdev_sph_layer_spec
!
!   --------------------------------------------------------------------
!
      subroutine read_time_ave_sph_layer_spec(fname_org, sph_lbl_IN,    &
     &          sph_IN, tave_spectr, trms_spectr, sdev_spectr)
!
      use set_parallel_file_name
      use sph_layer_monitor_snap_IO
!
      character(len = kchara), intent(in) :: fname_org
!
      type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      real(kind = kreal), intent(inout)                                 &
     &                   :: tave_spectr(sph_IN%ntot_sph_spec,           &
     &                                0:sph_IN%ltr_sph,sph_IN%nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: trms_spectr(sph_IN%ntot_sph_spec,           &
     &                                0:sph_IN%ltr_sph,sph_IN%nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: sdev_spectr(sph_IN%ntot_sph_spec,           &
     &                                0:sph_IN%ltr_sph,sph_IN%nri_sph)
!
      character(len = kchara) :: tave_file_name, trms_file_name
      character(len = kchara) :: sdev_file_name
      character(len = kchara) :: directory, fname_no_dir, fname_tmp
!
!
      call split_directory(fname_org, directory, fname_no_dir)
!
      write(fname_tmp, '(a6,a)') 't_ave_', trim(fname_no_dir)
      tave_file_name = append_directory(directory, fname_tmp)
      write(fname_tmp, '(a6,a)') 't_rms_', trim(fname_no_dir)
      trms_file_name = append_directory(directory, fname_tmp)
      write(fname_tmp, '(a7,a)') 't_sdev_', trim(fname_no_dir)
      sdev_file_name = append_directory(directory, fname_tmp)
!
      call read_sph_layer_spectr_snapshot(tave_file_name, sph_lbl_IN,   &
     &                                    sph_IN, tave_spectr(1,0,1))
      call read_sph_layer_spectr_snapshot(trms_file_name, sph_lbl_IN,   &
     &                                    sph_IN, trms_spectr(1,0,1))
      call read_sph_layer_spectr_snapshot(sdev_file_name, sph_lbl_IN,   &
     &                                    sph_IN, sdev_spectr(1,0,1))
!
      end subroutine read_time_ave_sph_layer_spec
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine sph_layer_spectr_average                               &
     &         (FPz_f, id_read, flag_gzip, flag_old_fmt,                &
     &          start_time, end_time, true_start, true_end,             &
     &          sph_IN, WK_tave, zbuf_rd)
!
      use gz_spl_sph_spectr_data_IO
      use cal_tave_sph_ene_spectr
!
      integer(kind = kint), intent(in) :: id_read
      logical, intent(in) :: flag_gzip, flag_old_fmt
      real(kind = kreal), intent(in) :: start_time, end_time
      real(kind = kreal), intent(inout) :: true_start, true_end
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(layer_spectr_ave_sigma_work), intent(inout) :: WK_tave
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip) :: zbuf_rd
!
      real(kind = kreal) :: prev_time
      integer(kind = kint) :: icou, ierr, ist_true, i, ncomp
!
!
      ncomp = sph_IN%ntot_sph_spec * sph_IN%nri_sph*(sph_IN%ltr_sph+1)
!
      icou = 0
      ist_true = -1
      prev_time = sph_IN%time
        write(*,'(a6,i12,a8,f12.6,a15,i12)',advance="NO")               &
     &       'step= ', sph_IN%i_step, ', time= ', sph_IN%time,          &
     &       ', Load Count:  ', icou
      do
        call sel_gz_input_sph_layer_spec                                &
     &     (FPz_f, id_read, flag_gzip, flag_old_fmt,                    &
     &      sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,       &
     &      sph_IN%i_step, sph_IN%time, sph_IN%kr_sph,                  &
     &      sph_IN%r_sph, sph_IN%i_mode, WK_tave%read_spec(1,0,1),      &
     &      zbuf_rd, ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. start_time) then
          icou = icou + 1
          if (ist_true .eq. -1) then
            ist_true =   sph_IN%i_step
            true_start = sph_IN%time
!
            call copy_ene_spectr_2_pre(sph_IN%time, prev_time,          &
     &          ncomp, WK_tave%read_spec(1,0,1),                        &
     &          WK_tave%ave_spec_l(1,0,1), WK_tave%ave_pre_l(1,0,1),    &
     &          WK_tave%rms_spec_l(1,0,1), WK_tave%rms_pre_l(1,0,1))
          else
            call add_average_ene_spectr(sph_IN%time, prev_time,         &
     &          ncomp, WK_tave%read_spec(1,0,1),                        &
     &          WK_tave%ave_spec_l(1,0,1), WK_tave%ave_pre_l(1,0,1),    &
     &          WK_tave%rms_spec_l(1,0,1), WK_tave%rms_pre_l(1,0,1))
!
          end if
        end if
!
        write(*,'(65a1,a6,i12,a8,f12.6,a15,i12)',advance="NO")          &
     &       (char(8),i=1,65),                                          &
     &       'step= ', sph_IN%i_step, ', time= ', sph_IN%time,          &
     &       ', Load Count:  ', icou
        if (sph_IN%time .ge. end_time) then
          true_end = sph_IN%time
          exit
        end if
      end do
!
   99 continue
      write(*,*)
!
      call divide_average_ene_spectr(sph_IN%time, true_start, ncomp,    &
     &    WK_tave%ave_spec_l, WK_tave%rms_spec_l)
!
      end subroutine sph_layer_spectr_average
!
!   --------------------------------------------------------------------
!
      subroutine sph_layer_spectr_std_deviation                         &
     &         (FPz_f, id_read, flag_gzip, flag_old_fmt,                &
     &          start_time, end_time, sph_IN, WK_tave, zbuf_rd)
!
      use gz_spl_sph_spectr_data_IO
      use cal_tave_sph_ene_spectr
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_read
      logical, intent(in) :: flag_gzip, flag_old_fmt
      real(kind = kreal), intent(in) :: start_time, end_time
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(layer_spectr_ave_sigma_work), intent(inout) :: WK_tave
      type(buffer_4_gzip), intent(inout) :: zbuf_rd
!
      real(kind = kreal) :: true_start, prev_time
      integer(kind = kint) :: icou, ierr, ist_true, i, ncomp
!
!  Evaluate standard deviation
!
      ncomp = sph_IN%ntot_sph_spec * sph_IN%nri_sph*(sph_IN%ltr_sph+1)
!
      icou = 0
      ist_true = -1
      prev_time = sph_IN%time
      WK_tave%sigma_spec_l = 0.0d0
      write(*,'(a6,i12,a8,f12.6,a15,i12)',advance="NO")                 &
     &       'step= ', sph_IN%i_step, ', time= ', sph_IN%time,          &
     &       ', Load Count:  ', icou
      do
        call sel_gz_input_sph_layer_spec                                &
     &     (FPz_f, id_read, flag_gzip, flag_old_fmt,                    &
     &      sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,       &
     &      sph_IN%i_step, sph_IN%time, sph_IN%kr_sph,                  &
     &      sph_IN%r_sph, sph_IN%i_mode, WK_tave%read_spec(1,0,1),      &
     &      zbuf_rd, ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. start_time) then
          icou = icou + 1
          if (ist_true .eq. -1) then
            ist_true = sph_IN%i_step
            true_start = sph_IN%time
            call copy_deviation_ene_2_pre                               &
     &         (sph_IN%time, prev_time, ncomp,                          &
     &         WK_tave%read_spec(1,0,1), WK_tave%ave_spec_l(1,0,1),     &
     &         WK_tave%sigma_spec_l(1,0,1), WK_tave%sigma_pre_l(1,0,1))
          else
            call add_deviation_ene_spectr                               &
     &        (sph_IN%time, prev_time, ncomp,                           &
     &         WK_tave%read_spec(1,0,1), WK_tave%ave_spec_l(1,0,1),     &
     &         WK_tave%sigma_spec_l(1,0,1), WK_tave%sigma_pre_l(1,0,1))
          end if
        end if
!
        write(*,'(65a1,a6,i12,a8,f12.6,a15,i12)',advance="NO")          &
     &       (char(8),i=1,65),                                          &
     &       'step= ', sph_IN%i_step, ', time= ', sph_IN%time,          &
     &       ', Load Count:  ', icou
        if (sph_IN%time .ge. end_time) exit
      end do
   99 continue
      write(*,*)
!
      call divide_deviation_ene_spectr(sph_IN%time, true_start,         &
     &                                 ncomp, WK_tave%sigma_spec_l)
!
      end subroutine sph_layer_spectr_std_deviation
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_tave_sph_data(sph_IN, WK_tave)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(layer_spectr_ave_sigma_work), intent(inout) :: WK_tave
      integer(kind = kint) :: ltr, ncomp
!
!
      ncomp = sph_IN%ntot_sph_spec
      ltr =   sph_IN%ltr_sph
      allocate( WK_tave%read_spec(ncomp,0:ltr,sph_IN%nri_sph))
      allocate( WK_tave%ave_spec_l(ncomp,0:ltr,sph_IN%nri_sph))
      allocate( WK_tave%rms_spec_l(ncomp,0:ltr,sph_IN%nri_sph))
      allocate( WK_tave%sigma_spec_l(ncomp,0:ltr,sph_IN%nri_sph))
      allocate( WK_tave%ave_pre_l(ncomp,0:ltr,sph_IN%nri_sph))
      allocate( WK_tave%rms_pre_l(ncomp,0:ltr,sph_IN%nri_sph))
      allocate( WK_tave%sigma_pre_l(ncomp,0:ltr,sph_IN%nri_sph))
!
      if(ncomp .le. 0) return
!$omp parallel workshare
      WK_tave%read_spec =  0.0d0
      WK_tave%ave_spec_l =  0.0d0
      WK_tave%rms_spec_l =  0.0d0
      WK_tave%sigma_spec_l =  0.0d0
      WK_tave%ave_pre_l = 0.0d0
      WK_tave%rms_pre_l = 0.0d0
      WK_tave%sigma_pre_l = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_tave_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_tave_sph_data(WK_tave)
!
      type(layer_spectr_ave_sigma_work), intent(inout) :: WK_tave
!
      deallocate(WK_tave%read_spec)
      deallocate(WK_tave%ave_spec_l,   WK_tave%ave_pre_l)
      deallocate(WK_tave%rms_spec_l,   WK_tave%rms_pre_l)
      deallocate(WK_tave%sigma_spec_l, WK_tave%sigma_pre_l)
!
      end subroutine dealloc_tave_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine check_time_ave_sph_layer_spec(sph_IN, WK_tave)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(layer_spectr_ave_sigma_work), intent(in) :: WK_tave
!
      integer(kind = kint) :: i, k, l, num_tlabel
!
!
      num_tlabel = sph_IN%num_time_labels
      do i = 1, sph_IN%ntot_sph_spec
        write(*,'(a)') trim(sph_IN%ene_sph_spec_name(i+num_tlabel))
        write(*,'(a)') 'Mode, Time_average, Standard_deviation, R.M.S.'
        do k = 1, sph_IN%nri_sph
          do l = 0, sph_IN%ltr_sph
            write(*,'(i16,1p3e23.15e3,2a)')                             &
     &        sph_IN%kr_sph(k),  sph_IN%r_sph(k),  sph_IN%i_mode(l),    &
     &        WK_tave%ave_spec_l(i,l,k),  WK_tave%sigma_spec_l(i,l,k),  &
     &        WK_tave%rms_spec_l(i,l,k)
          end do
        end do
      end do
!
      end subroutine check_time_ave_sph_layer_spec
!
!   --------------------------------------------------------------------
!
      end module t_tave_sph_layer_spectr
