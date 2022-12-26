!>@file   m_part_sum_sph_ene_spectr.f90
!!        module m_part_sum_sph_ene_spectr
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief TAke sum of a part of spectrum data
!!
!!@verbatim
!!      subroutine sph_part_volume_spectr_sum                           &
!!     &         (fname_org, spec_evo_p, sph_IN)
!!      subroutine sph_part_layer_spectr_sum                            &
!!     &         (fname_org, spec_evo_p, sph_IN)
!!        character(len = kchara), intent(in) :: fname_org
!!        type(sph_spectr_file_param), intent(in) :: spec_evo_p
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!@endverbatim
!
      module m_part_sum_sph_ene_spectr
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
!
      implicit none
!
!
      integer(kind = kint), parameter :: id_file_rms =      34
      integer(kind = kint), parameter :: id_file_rms_l =    44
!
      type(read_sph_spectr_data), save :: sph_OUT1
!
      private :: sph_OUT1
      private :: id_file_rms, id_file_rms_l
      private :: part_sum_ene_spectr
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sph_part_volume_spectr_sum                             &
     &         (fname_org, spec_evo_p, sph_IN)
!
      use t_buffer_4_gzip
      use t_ctl_param_sph_series_util
      use sph_monitor_data_text
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_volume_spectr_monitor_IO
      use gz_spl_sph_spectr_data_IO
      use gz_open_sph_monitor_file
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: fname_org
      type(sph_spectr_file_param), intent(in) :: spec_evo_p
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      real(kind = kreal), allocatable :: spectr_IN(:,:)
      real(kind = kreal), allocatable :: mean_OUT(:)
!
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1, zbuf_s
      character, pointer :: FPz_f1
      type(sph_spectr_head_labels) :: sph_lbl_IN_ps
      character(len = kchara) :: input_prefix, input_extension
      character(len = kchara) :: file_name, fname_tmp
      integer(kind = kint) :: i, icou, ierr, ist_true
!
!
      call split_extrension(fname_org, input_prefix, input_extension)
      file_name = add_dat_extension(input_prefix)
      call sel_open_read_gz_stream_file(FPz_f1, id_file_rms_l,          &
     &                                    fname_org, flag_gzip1, zbuf1)
      call read_sph_volume_spectr_head(FPz_f1, id_file_rms_l,           &
     &    flag_gzip1, sph_lbl_IN_ps, sph_IN, zbuf1)
      call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
      call check_sph_spectr_name(sph_IN)
!
      call copy_read_ene_params_4_sum(sph_IN, sph_OUT1)
!
      allocate(spectr_IN(sph_IN%ntot_sph_spec,0:sph_IN%ltr_sph))
      allocate(mean_OUT(sph_OUT1%ntot_sph_spec))
!
!$omp parallel workshare
      spectr_IN(1:sph_IN%ntot_sph_spec,0:sph_IN%ltr_sph) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
      mean_OUT(1:sph_OUT1%ntot_sph_spec) = 0.0d0
!$omp end parallel workshare
!
      write(fname_tmp, '(a5,a)') 'part_', trim(input_prefix)
      file_name = add_int_suffix(spec_evo_p%lst, fname_tmp)
      fname_tmp = add_int_suffix(spec_evo_p%led, file_name)
      file_name = add_dat_extension(fname_tmp)
      open(id_file_rms, file=file_name,                                 &
     &     status='replace', FORM='UNFORMATTED', ACCESS='STREAM')
      call write_sph_pwr_vol_head                                       &
     &   (.FALSE., id_file_rms, sph_pwr_labels, sph_OUT1, zbuf_s)
!
      icou = 0
      ist_true = -1
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' averaging finished. Count=  ', icou
      do
        call sel_gz_read_volume_spectr_mtr(FPz_f1, id_file_rms_l,       &
     &      flag_gzip1, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,           &
     &      sph_IN%i_step, sph_IN%time, sph_IN%i_mode,                  &
     &      spectr_IN(1,0), zbuf1, ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. spec_evo_p%start_time) then
          sph_OUT1%time =   sph_IN%time
          sph_OUT1%i_step = sph_IN%i_step
          call part_sum_ene_spectr(spec_evo_p%lst, spec_evo_p%led,      &
     &        ione, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,               &
     &        spectr_IN(1,0), mean_OUT(1))
          icou = icou + 1
!
          call sel_gz_write_text_stream(.FALSE., id_file_rms_l,         &
     &        volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,      &
     &        sph_OUT1%ntot_sph_spec, mean_OUT(1)), zbuf_s)
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' averaging finished. Count=   ', icou
        if (sph_IN%time .ge. spec_evo_p%end_time) exit
      end do
!
   99 continue
      write(*,*)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_file_rms_l, flag_gzip1, zbuf1)
      close(id_file_rms)
!
!
      deallocate(spectr_IN, mean_OUT)
      call dealloc_sph_espec_data(sph_IN)
      call dealloc_sph_espec_name(sph_IN)
      call dealloc_sph_espec_data(sph_OUT1)
      call dealloc_sph_espec_name(sph_OUT1)
!
      end subroutine sph_part_volume_spectr_sum
!
!   --------------------------------------------------------------------
!
      subroutine sph_part_layer_spectr_sum                              &
     &         (fname_org, spec_evo_p, sph_IN)
!
      use t_buffer_4_gzip
      use t_ctl_param_sph_series_util
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_layer_spectr_monitor_IO
      use gz_layer_mean_monitor_IO
      use gz_spl_sph_spectr_data_IO
      use gz_open_sph_monitor_file
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: fname_org
      type(sph_spectr_file_param), intent(in) :: spec_evo_p
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      real(kind = kreal), allocatable :: spectr_IN(:,:,:)
      real(kind = kreal), allocatable :: mean_OUT(:,:)
!
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1, zbuf_s
      character, pointer :: FPz_f1
      type(sph_spectr_head_labels) :: sph_lbl_IN_ps
      character(len = kchara) :: input_prefix, input_extension
      character(len = kchara) :: file_name, fname_tmp
      integer(kind = kint) :: i, icou, ierr, ist_true
!
!
      call split_extrension(fname_org, input_prefix, input_extension)
      file_name = add_dat_extension(input_prefix)
      call sel_open_read_gz_stream_file(FPz_f1, id_file_rms_l,          &
     &                                    fname_org, flag_gzip1, zbuf1)
      call read_sph_layer_spectr_head(FPz_f1, id_file_rms_l,            &
     &    flag_gzip1, spec_evo_p%flag_old_fmt, sph_lbl_IN_ps, sph_IN,   &
     &    zbuf1)
      call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
      call check_sph_spectr_name(sph_IN)
!
      call copy_read_ene_params_4_sum(sph_IN, sph_OUT1)
!
      allocate(spectr_IN(sph_IN%ntot_sph_spec,                          &
     &                   0:sph_IN%ltr_sph,sph_IN%nri_sph))
      allocate(mean_OUT(sph_OUT1%ntot_sph_spec,sph_OUT1%nri_sph))
!$omp parallel workshare
      spectr_IN(1:sph_IN%ntot_sph_spec,                                 &
     &          0:sph_IN%ltr_sph,1:sph_IN%nri_sph) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
      mean_OUT(1:sph_OUT1%ntot_sph_spec,1:sph_OUT1%nri_sph) = 0.0d0
!$omp end parallel workshare
!
      write(fname_tmp, '(a5,a)') 'part_', trim(input_prefix)
      file_name = add_int_suffix(spec_evo_p%lst, fname_tmp)
      fname_tmp = add_int_suffix(spec_evo_p%led, file_name)
      file_name = add_dat_extension(fname_tmp)
      open(id_file_rms, file=file_name,                                 &
     &     status='replace', FORM='UNFORMATTED', ACCESS='STREAM')
      call write_sph_pwr_layer_head                                     &
     &   (.FALSE., id_file_rms, sph_pwr_labels, sph_OUT1, zbuf_s)
!
      icou = 0
      ist_true = -1
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' averaging finished. Count=  ', icou
      do
        call sel_gz_input_sph_layer_spec(FPz_f1, id_file_rms_l,         &
     &      flag_gzip1, spec_evo_p%flag_old_fmt,                        &
     &      sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,       &
     &      sph_IN%i_step, sph_IN%time, sph_IN%kr_sph,                  &
     &      sph_IN%r_sph, sph_IN%i_mode, spectr_IN(1,0,1),              &
     &      zbuf1, ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. spec_evo_p%start_time) then
          sph_OUT1%time =   sph_IN%time
          sph_OUT1%i_step = sph_IN%i_step
          call part_sum_ene_spectr(spec_evo_p%lst, spec_evo_p%led,      &
     &        sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,     &
     &        spectr_IN, mean_OUT)
          icou = icou + 1
!
          call sel_gz_write_layer_mean_mtr(.FALSE., id_file_rms,        &
     &        sph_OUT1%i_step, sph_OUT1%time, sph_OUT1%nri_sph,         &
     &        sph_OUT1%kr_sph, sph_OUT1%r_sph, sph_OUT1%ntot_sph_spec,  &
     &        mean_OUT(1,1), zbuf_s)
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' averaging finished. Count=   ', icou
        if (sph_IN%time .ge. spec_evo_p%end_time) exit
      end do
!
   99 continue
      write(*,*)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_file_rms_l, flag_gzip1, zbuf1)
      close(id_file_rms)
!
!
      deallocate(spectr_IN, mean_OUT)
      call dealloc_sph_espec_data(sph_IN)
      call dealloc_sph_espec_name(sph_IN)
      call dealloc_sph_espec_data(sph_OUT1)
      call dealloc_sph_espec_name(sph_OUT1)
!
      end subroutine sph_part_layer_spectr_sum
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine part_sum_ene_spectr(lst, led, nri_sph, ltr_sph, ncomp, &
     &          spectr_l, sum_spec)
!
      integer(kind = kint), intent(in) :: lst, led
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout) :: sum_spec(ncomp,nri_sph)
!
      integer(kind = kint) :: kr, lth
!
!
!$omp parallel workshare
        sum_spec(1:ncomp,1:nri_sph) = 0.0d0
!$omp end parallel workshare
!
      do kr = 1, nri_sph
        do lth = lst, led
!$omp parallel workshare
            sum_spec(1:ncomp,kr) = sum_spec(1:ncomp,kr)                 &
     &                              + spectr_l(1:ncomp,lth,kr)
!$omp end parallel workshare
        end do
      end do
!
      end subroutine part_sum_ene_spectr
!
!   --------------------------------------------------------------------
!
      end module m_part_sum_sph_ene_spectr
