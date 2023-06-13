!>@file   compare_sph_mean_spectr.f90
!!        program compare_sph_mean_spectr
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!>@brief Compare volume mean square data from reference
!!@n
!!@n      Reference data: reference/sph_pwr_volumne.dat
!!@n      Compared data:          ./sph_pwr_volumne.dat
!!
!
      program compare_sph_mean_spectr
!
      use m_precision
      use skip_comment_f
      use t_buffer_4_gzip
      use t_read_sph_spectra
      use t_sph_spectr_head_labels
!
      use compare_sph_monitor_header
      use sel_gz_input_sph_mtr_head
      use select_gz_stream_file_IO
      use gz_spl_sph_spectr_data_IO
      use gz_volume_spectr_monitor_IO
      use set_parallel_file_name
      use skip_comment_f
!
      implicit none
!
      character(len = kchara) :: fhead_rms_vol, fname_rms_vol
      character(len = kchara) :: fhead_rms_ref, fname_rms_ref
!
      integer(kind = kint), parameter :: id_file1 = 34, id_file2 = 36
      character, pointer :: FPz_f1, FPz_f2
      type(buffer_4_gzip) :: zbuf1, zbuf2
      type(read_sph_spectr_data) :: sph_IN1, sph_IN2
      type(sph_spectr_head_labels) :: sph_lbl_IN1, sph_lbl_IN2
      logical :: flag_gzip1, flag_gzip2
      logical :: flag_miss1, flag_miss2
      character(len = kchara) :: file_name
!
      logical :: error
      integer(kind = kint) :: ierr1, ierr2, l
      real(kind = kreal) :: diff
      real(kind = kreal), allocatable :: spectr_IN1(:,:)
      real(kind = kreal), allocatable :: spectr_IN2(:,:)
      integer(kind = kint) :: icomp, icomp2
!
!
      if(iargc_kemo() .le. 1) then
        write(*,*) 'sph_vol_spectr_check ',                             &
     &             'REFERENCE_FILE_PREFIX COMPARED_FILE_PREFIX'
        stop
      end if
      call getarg_k(1, fhead_rms_ref)
      call getarg_k(2, fhead_rms_vol)
      fname_rms_ref = add_dat_extension(fhead_rms_ref)
      fname_rms_vol = add_dat_extension(fhead_rms_vol)
!
!
      call sel_open_check_gz_stream_file(FPz_f1, id_file1,              &
     &   fname_rms_vol, flag_gzip1, flag_miss1, file_name, zbuf1)
      call sel_open_check_gz_stream_file(FPz_f2, id_file2,              &
     &   fname_rms_ref, flag_gzip2, flag_miss2, file_name, zbuf2)
!
      call read_sph_volume_spectr_head(FPz_f1, id_file1, flag_gzip1,    &
     &                                 sph_lbl_IN1, sph_IN1, zbuf1)
      call read_sph_volume_spectr_head(FPz_f2, id_file2, flag_gzip2,    &
     &                                 sph_lbl_IN2, sph_IN2, zbuf2)
      error = .not. cmp_sph_volume_monitor_heads                        &
     &            (sph_lbl_IN1, sph_IN1, sph_lbl_IN2, sph_IN2)
!
      if(error) then
        write(*,*) 'time sequence data header does not match'
        stop 1
      end if
!
      call alloc_sph_spectr_data(sph_IN1%ltr_sph, sph_IN1)
      call alloc_sph_spectr_data(sph_IN2%ltr_sph, sph_IN2)
      allocate(spectr_IN1(sph_IN1%ntot_sph_spec,0:sph_IN1%ltr_sph))
      allocate(spectr_IN2(sph_IN2%ntot_sph_spec,0:sph_IN2%ltr_sph))
!
      do
        call sel_gz_read_volume_spectr_mtr(FPz_f1, id_file1,            &
     &      flag_gzip1, sph_IN1%ltr_sph, sph_IN1%ntot_sph_spec,         &
     &      sph_IN1%i_step, sph_IN1%time, sph_IN1%i_mode,               &
     &      spectr_IN1(1,0), zbuf1, ierr1)
        call sel_gz_read_volume_spectr_mtr(FPz_f2, id_file2,            &
     &      flag_gzip2, sph_IN2%ltr_sph, sph_IN2%ntot_sph_spec,         &
     &      sph_IN2%i_step, sph_IN2%time, sph_IN2%i_mode,               &
     &      spectr_IN1(1,0), zbuf2, ierr2)
!
        if(ierr1*ierr2 .gt. 0) exit
        if(ierr1+ierr1 .gt. 0 .and. ierr1*ierr2 .eq. 0) then
          error = .TRUE.
          exit
        end if
!
        error = .FALSE.
        do l = 0, sph_IN1%ltr_sph
          do icomp = 1, sph_IN1%ntot_sph_spec
            diff = compare_data(spectr_IN1(icomp,l),                    &
     &                          spectr_IN2(icomp,l))
            if(abs(diff) .gt. 1.d-9) then
              icomp2 = icomp + sph_IN1%num_time_labels
              write(*,*) 'Error in ', l,                                &
     &             trim(sph_IN1%ene_sph_spec_name(icomp2)),             &
     &             ': ', spectr_IN1(icomp,l), spectr_IN2(icomp,l), diff
              error = .TRUE.
              exit
            end if
          end do
        end do
      end do
!
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_file1, flag_gzip1, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f2, id_file2, flag_gzip2, zbuf2)
!
      if(error) then
        write(*,*) 'time sequence data does not match'
        stop 1
      end if
!
      stop 0
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine getarg_k(i, argc)
!
      integer, intent(in) :: i
      character(len=*), intent(out) :: argc
!
      call getarg(0, argc)
      if(argc == "") then
        call getarg(i + 1, argc)
      else
        call getarg(i, argc)
      end if
      end subroutine getarg_k
!
!   --------------------------------------------------------------------
!
      integer function iargc_kemo() result(oresult)
!
      integer :: iargc
      character(len=8) :: argc
      oresult = iargc()
      call getarg(0, argc)
      if(argc == "") then
        oresult = oresult - 1
      end if
      end function iargc_kemo
!
!   --------------------------------------------------------------------
!
      end program compare_sph_mean_spectr
