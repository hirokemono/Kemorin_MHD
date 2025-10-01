!>@file   m_sph_dynamic_elsasser.f90
!!        module m_sph_dynamic_elsasser
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Obtain lengh scale from spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine sph_dynamic_elsasser_by_spectr                       &
!!     &         (input_header, sph_IN)
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!@endverbatim
!
      module m_sph_dynamic_elsasser
!
      use m_precision
      use m_constants
      use m_phys_constants
      use t_read_sph_spectra
      use t_buffer_4_gzip
      use t_dyn_elsasser_address
      use t_ctl_param_dyn_elsasser
!
      implicit none
!
      integer(kind = kint), parameter :: id_file_rms_l =    35
      integer(kind = kint), parameter :: id_file_rms_m =    37
      integer(kind = kint), parameter :: id_file_lscale =   44
!
      type(read_sph_spectr_data), save :: sph_IN_l, sph_IN_m
      type(read_sph_spectr_data), save :: sph_OUT1
!
      type(dyn_elsasser_address), save :: iels1
!
      logical, parameter, private :: vol_ave_on = .TRUE.
!
      private :: sph_OUT1, sph_IN_l, sph_IN_m
      private :: id_file_rms_l, id_file_rms_m, id_file_lscale
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sph_dynamic_elsasser_by_spectr(els_dat)
!
      use t_buffer_4_gzip
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_volume_spectr_monitor_IO
      use gz_open_sph_vol_mntr_file
      use sph_monitor_data_text
      use set_parallel_file_name
      use cal_dyn_elsasser_by_spectr
!
      type(sph_dyn_elsasser_data), intent(inout) :: els_dat
!
      logical :: flag_gzip_l, flag_gzip_m
      type(buffer_4_gzip) :: zbuf_l, zbuf_m, zbuf_s
      type(sph_spectr_head_labels) :: sph_lbl_IN_l, sph_lbl_IN_m
      character, pointer :: FPz_l, FPz_m
!
      real(kind = kreal), allocatable :: spectr_l(:,:)
      real(kind = kreal), allocatable :: spectr_m(:,:)
      real(kind = kreal), allocatable :: elsassers(:)
!
      integer(kind = kint) :: i, icou, ierr, ist_true
      integer(kind = kint) :: nri_tmp
!
!
      call sel_open_read_gz_stream_file(FPz_l, id_file_rms_l,           &
     &    els_dat%vol_l_spectr_file_name, flag_gzip_l, zbuf_l)
      call read_sph_volume_spectr_head(FPz_l, id_file_rms_l,            &
     &    flag_gzip_l, sph_lbl_IN_l, sph_IN_l, zbuf_l)
!
      sph_IN_l%nri_dat = 1
      call alloc_sph_spectr_data(sph_IN_l%ltr_sph, sph_IN_l)
      call check_sph_spectr_name(sph_IN_l)
!
      call sel_open_read_gz_stream_file(FPz_m, id_file_rms_m,           &
     &    els_dat%vol_m_spectr_file_name, flag_gzip_m, zbuf_m)
      call read_sph_volume_spectr_head(FPz_m, id_file_rms_m,            &
     &    flag_gzip_m, sph_lbl_IN_m, sph_IN_m, zbuf_m)
!
      sph_IN_m%nri_dat = 1
      call alloc_sph_spectr_data(sph_IN_m%ltr_sph, sph_IN_m)
      call check_sph_spectr_name(sph_IN_m)
!
      call set_spectr_address_4_dyn_els(sph_IN_l, els_dat)
!
      call copy_read_ene_head_params(sph_IN_l, sph_OUT1)
      sph_OUT1%num_time_labels = sph_OUT1%num_time_labels - 1
!
      call set_dynamic_elsasser_address(els_dat, iels1,                 &
     &    sph_OUT1%nfield_sph_spec, sph_OUT1%ntot_sph_spec)
!      call check_dynamic_elsasser_address(els_dat, iels1)
!
      sph_OUT1%num_labels                                               &
     &        = sph_OUT1%ntot_sph_spec + sph_OUT1%num_time_labels
      call alloc_sph_espec_name(sph_OUT1)
      call set_dynamic_elsasser_name(sph_IN_l, iels1, els_dat,          &
     &    sph_OUT1%num_labels, sph_OUT1%nfield_sph_spec,                &
     &    sph_OUT1%num_time_labels, sph_OUT1%ncomp_sph_spec,            &
     &    sph_OUT1%ene_sph_spec_name)
!
!
      allocate(spectr_l(sph_IN_l%ntot_sph_spec, 0:sph_IN_l%ltr_sph))
      allocate(spectr_m(sph_IN_m%ntot_sph_spec, 0:sph_IN_m%ltr_sph))
      allocate(elsassers(sph_OUT1%ntot_sph_spec))
!
!$omp parallel workshare
      spectr_l(1:sph_IN_l%ntot_sph_spec, 0:sph_IN_l%ltr_sph) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
      spectr_m(1:sph_IN_m%ntot_sph_spec, 0:sph_IN_m%ltr_sph) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
      elsassers(1:sph_OUT1%ntot_sph_spec) = 0.0d0
!$omp end parallel workshare
!
!
      write(*,*) 'Save Elsasser number into  ',                         &
     &          trim(els_dat%elsasser_file_name)
      open(id_file_lscale, file=els_dat%elsasser_file_name,             &
     &     status='replace', FORM='UNFORMATTED', ACCESS='STREAM')
      call write_sph_pwr_vol_head(.FALSE., id_file_lscale,              &
     &                            sph_pwr_labels, sph_OUT1, zbuf_s)
!
      nri_tmp = sph_OUT1%nri_sph
      sph_OUT1%nri_sph = 1
      sph_OUT1%nri_dat = 1
      call alloc_sph_spectr_data(izero, sph_OUT1)
!
      icou = 0
      ist_true = -1
        write(*,'(a5,i12,a7,1pE23.15,a19,i12)',advance="NO")            &
     &       'step= ', sph_IN_l%i_step, ' time= ', sph_IN_l%time,       &
     &       ' evaluated Count=  ', icou
      do
        call sel_gz_read_volume_spectr_mtr(FPz_l, id_file_rms_l,        &
     &      flag_gzip_l, sph_IN_l%ltr_sph, sph_IN_l%ntot_sph_spec,      &
     &      sph_IN_l%i_step, sph_IN_l%time, sph_IN_l%i_mode,            &
     &      spectr_l(1,0), zbuf_l, ierr)
        call sel_gz_read_volume_spectr_mtr(FPz_l, id_file_rms_m,        &
     &      flag_gzip_m, sph_IN_m%ltr_sph, sph_IN_m%ntot_sph_spec,      &
     &      sph_IN_m%i_step, sph_IN_m%time, sph_IN_m%i_mode,            &
     &      spectr_m(1,0), zbuf_m, ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN_l%time .ge. els_dat%start_time) then
          icou = icou + 1
          sph_OUT1%time =   sph_IN_l%time
          sph_OUT1%i_step = sph_IN_l%i_step
          call cal_dynamic_elsasser_by_spectr(iels1, els_dat,           &
     &        sph_IN_l%ltr_sph, sph_IN_l%ntot_sph_spec, spectr_l(1,0),  &
     &        sph_IN_m%ltr_sph, sph_IN_m%ntot_sph_spec, spectr_m(1,0),  &
     &        sph_OUT1%ntot_sph_spec, elsassers(1))
!
          call sel_gz_write_text_stream(.FALSE., id_file_lscale,        &
     &        volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,      &
     &        sph_OUT1%ntot_sph_spec, elsassers), zbuf_s)
        end if
!
        write(*,'(78a1,a5,i12,a7,1pE23.15,a19,i12)',advance="NO")       &
     &       (char(8),i=1,78),                                          &
     &       'step= ', sph_IN_l%i_step, ' time= ', sph_IN_l%time,       &
     &       ' evaluated Count=  ', icou
        if (sph_IN_l%time .ge. els_dat%end_time) exit
      end do
!
   99 continue
      write(*,*)
      call sel_close_read_gz_stream_file(FPz_l, id_file_rms_l,          &
     &                                flag_gzip_l, zbuf_l)
      call sel_close_read_gz_stream_file(FPz_m, id_file_rms_m,          &
     &                                flag_gzip_m, zbuf_m)
      close(id_file_lscale)
!
!
      deallocate(elsassers, spectr_m, spectr_l)
      call dealloc_sph_espec_data(sph_IN_l)
      call dealloc_sph_espec_name(sph_IN_l)
      call dealloc_sph_espec_data(sph_IN_m)
      call dealloc_sph_espec_name(sph_IN_m)
      call dealloc_sph_espec_data(sph_OUT1)
      call dealloc_sph_espec_name(sph_OUT1)
!
      end subroutine sph_dynamic_elsasser_by_spectr
!
!   --------------------------------------------------------------------
!
      end module m_sph_dynamic_elsasser
