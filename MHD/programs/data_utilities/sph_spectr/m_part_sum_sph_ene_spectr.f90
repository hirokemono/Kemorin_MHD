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
!!      subroutine sph_part_pwr_spectr_sum                              &
!!     &         (fname_org, flag_vol_ave, spec_evo_p, sph_IN)
!!        character(len = kchara), intent(in) :: fname_org
!!        logical, intent(in) :: flag_vol_ave
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
      subroutine sph_part_pwr_spectr_sum                                &
     &         (fname_org, flag_vol_ave, spec_evo_p, sph_IN)
!
      use t_buffer_4_gzip
      use t_ctl_param_sph_series_util
      use select_gz_stream_file_IO
      use write_sph_monitor_data
      use gz_spl_sph_spectr_head_IO
      use gz_spl_sph_spectr_data_IO
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: fname_org
      logical, intent(in) :: flag_vol_ave
      type(sph_spectr_file_param), intent(in) :: spec_evo_p
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1
      character, pointer :: FPz_f1
      character(len = kchara) :: input_prefix, input_extension
      character(len = kchara) :: file_name, fname_tmp
      integer(kind = kint) :: i, icou, ierr, ist_true
!
!
      call split_extrension(fname_org, input_prefix, input_extension)
      file_name = add_dat_extension(input_prefix)
      call sel_open_read_gz_stream_file(FPz_f1, id_file_rms_l,          &
     &                                    fname_org, flag_gzip1, zbuf1)
      call select_input_sph_series_head(FPz_f1, id_file_rms_l,          &
     &    flag_gzip1, spec_evo_p%flag_old_fmt, spectr_on, flag_vol_ave, &
     &    sph_IN, zbuf1)
      call check_sph_spectr_name(sph_IN)
!
      call copy_read_ene_params_4_sum(sph_IN, sph_OUT1)
!
      write(fname_tmp, '(a5,a)') 'part_', trim(input_prefix)
      file_name = add_int_suffix(spec_evo_p%lst, fname_tmp)
      fname_tmp = add_int_suffix(spec_evo_p%led, file_name)
      file_name = add_dat_extension(fname_tmp)
      open(id_file_rms, file=file_name)
      call select_output_sph_pwr_head                                   &
     &   (id_file_rms, flag_vol_ave, sph_OUT1)
!
      icou = 0
      ist_true = -1
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' averaging finished. Count=  ', icou
      do
        call sel_gz_input_sph_series_data                               &
     &     (FPz_f1, id_file_rms_l, flag_gzip1,                          &
     &      spec_evo_p%flag_old_fmt, spectr_on, flag_vol_ave,           &
     &      sph_IN, zbuf1, ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. spec_evo_p%start_time) then
          sph_OUT1%time =   sph_IN%time
          sph_OUT1%i_step = sph_IN%i_step
          call part_sum_ene_spectr(spec_evo_p%lst, spec_evo_p%led,      &
     &        sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,     &
     &        sph_IN%spectr_IO, sph_OUT1%spectr_IO)
          icou = icou + 1
!
          call select_output_sph_series_data                            &
     &       (id_file_rms, spectr_off, flag_vol_ave, sph_OUT1)
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
      call dealloc_sph_espec_data(sph_IN)
      call dealloc_sph_espec_data(sph_OUT1)
!
      end subroutine sph_part_pwr_spectr_sum
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
      real(kind = kreal), intent(inout) :: sum_spec(ncomp,0:0,nri_sph)
!
      integer(kind = kint) :: kr, lth
!
!
!$omp parallel workshare
        sum_spec(1:ncomp,0:0,1:nri_sph) = 0.0d0
!$omp end parallel workshare
!
      do kr = 1, nri_sph
        do lth = lst, led
!$omp parallel workshare
            sum_spec(1:ncomp,0,kr) = sum_spec(1:ncomp,0,kr)             &
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
