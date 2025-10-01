!>@file   m_maxmode_sph_ene_spectr.f90
!!        module m_maxmode_sph_ene_spectr
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Find max and minimum amplitude
!!
!!@verbatim
!!      subroutine sph_maximum_volume_spectr                            &
!!     &         (fname_org, spec_evo_p, sph_IN)
!!      subroutine sph_maximum_layer_spectr                             &
!!     &         (fname_org, spec_evo_p, sph_IN)
!!        character(len = kchara), intent(in) :: fname_org
!!        type(sph_spectr_file_param), intent(in) :: spec_evo_p
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!@endverbatim
!
      module m_maxmode_sph_ene_spectr
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
!
      implicit none
!
!
      integer(kind = kint), parameter :: id_file_rms_l =    35
      integer(kind = kint), parameter :: id_file_maxval =   44
      integer(kind = kint), parameter :: id_file_maxloc =   45
!
      type(read_sph_spectr_data), save :: sph_OUT1
!
      real(kind = kreal), allocatable :: max_spectr(:,:)
      real(kind = kreal), allocatable :: max_degree(:,:)
!
!
      private :: sph_OUT1, max_spectr, max_degree
      private :: id_file_rms_l, id_file_maxval, id_file_maxloc
      private :: find_dominant_scale_sph
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sph_maximum_volume_spectr                              &
     &         (fname_org, spec_evo_p, sph_IN)
!
      use t_buffer_4_gzip
      use t_ctl_param_sph_series_util
      use sph_monitor_data_text
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_open_sph_vol_mntr_file
      use gz_volume_spectr_monitor_IO
      use gz_spl_sph_spectr_data_IO
!
      character(len = kchara), intent(in) :: fname_org
      type(sph_spectr_file_param), intent(in) :: spec_evo_p
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      real(kind = kreal), allocatable :: spectr_IN(:,:)
!
      character(len = kchara) :: file_name
      integer(kind = kint) :: i, icou, ierr, ist_true
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1, zbuf_s
      character, pointer :: FPz_f1
!
      type(sph_spectr_head_labels) :: sph_lbl_IN_m
!
!
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_file_rms_l, fname_org, flag_gzip1, zbuf1)
      call read_sph_volume_spectr_head(FPz_f1, id_file_rms_l,           &
     &    flag_gzip1, sph_lbl_IN_m, sph_IN, zbuf1)
      call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
!
      call check_sph_spectr_name(sph_IN)
!
      call copy_read_ene_params_4_sum(sph_IN, sph_OUT1)
!
      allocate(spectr_IN(sph_IN%ntot_sph_spec,0:sph_IN%ltr_sph))
!$omp parallel workshare
      spectr_IN(1:sph_IN%ntot_sph_spec,0:sph_IN%ltr_sph) = 0.0d0
!$omp end parallel workshare
!
      write(file_name, '(a7,a)') 'maxval_', trim(fname_org)
      open(id_file_maxval, file=file_name,                              &
     &     status='replace', FORM='UNFORMATTED', ACCESS='STREAM')
      call write_sph_pwr_vol_head                                       &
     &   (.FALSE., id_file_maxval, sph_pwr_labels, sph_OUT1, zbuf_s)
!
      write(file_name, '(a7,a)') 'maxloc_', trim(fname_org)
      open(id_file_maxloc, file=file_name,                              &
     &     status='replace', FORM='UNFORMATTED', ACCESS='STREAM')
      call write_sph_pwr_vol_head                                       &
     &   (.FALSE., id_file_maxloc, sph_pwr_labels, sph_OUT1, zbuf_s)
!
      call allocate_max_sph_data(sph_IN)
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
          call find_dominant_scale_sph                                  &
     &       (ione, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,               &
     &        spectr_IN(1,0), max_spectr, max_degree)
          icou = icou + 1
!
          call sel_gz_write_text_stream(.FALSE., id_file_maxval,        &
     &        volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,      &
     &        sph_OUT1%ntot_sph_spec, max_spectr(1,1)), zbuf_s)
          call sel_gz_write_text_stream(.FALSE., id_file_maxloc,        &
     &        volume_pwr_data_text(sph_OUT1%i_step, sph_OUT1%time,      &
     &        sph_OUT1%ntot_sph_spec, max_degree(1,1)), zbuf_s)
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
      close(id_file_rms_l)
      close(id_file_maxval)
      close(id_file_maxloc)
!
!
      call deallocate_max_sph_data
      call dealloc_sph_espec_data(sph_IN)
      call dealloc_sph_espec_name(sph_IN)
      call dealloc_sph_espec_data(sph_OUT1)
      call dealloc_sph_espec_name(sph_OUT1)
!
      end subroutine sph_maximum_volume_spectr
!
!   --------------------------------------------------------------------
!
      subroutine sph_maximum_layer_spectr                               &
     &         (fname_org, spec_evo_p, sph_IN)
!
      use t_buffer_4_gzip
      use t_ctl_param_sph_series_util
      use gz_layer_mean_monitor_IO
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_open_sph_layer_mntr_file
      use gz_spl_sph_spectr_data_IO
!
      character(len = kchara), intent(in) :: fname_org
      type(sph_spectr_file_param), intent(in) :: spec_evo_p
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      real(kind = kreal), allocatable :: spectr_IN(:,:,:)
!
      character(len = kchara) :: file_name
      integer(kind = kint) :: i, icou, ierr, ist_true
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1, zbuf_s
      character, pointer :: FPz_f1
!
      type(sph_spectr_head_labels) :: sph_lbl_IN_m
!
!
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_file_rms_l, fname_org, flag_gzip1, zbuf1)
      call read_sph_layer_spectr_head                                   &
     &   (FPz_f1, id_file_rms_l, flag_gzip1, spec_evo_p%flag_old_fmt,   &
     &    sph_lbl_IN_m, sph_IN, zbuf1)
      call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
!
      call check_sph_spectr_name(sph_IN)
!
      call copy_read_ene_params_4_sum(sph_IN, sph_OUT1)
!
      allocate(spectr_IN(sph_IN%ntot_sph_spec,                          &
     &                   0:sph_IN%ltr_sph,sph_IN%nri_sph))
!$omp parallel workshare
      spectr_IN(1:sph_IN%ntot_sph_spec,                                 &
     &          0:sph_IN%ltr_sph,1:sph_IN%nri_sph) = 0.0d0
!$omp end parallel workshare
!
      write(file_name, '(a7,a)') 'maxval_', trim(fname_org)
      open(id_file_maxval, file=file_name,                              &
     &     status='replace', FORM='UNFORMATTED', ACCESS='STREAM')
      call write_sph_pwr_layer_head                                     &
     &   (.FALSE., id_file_maxval, sph_pwr_labels, sph_OUT1, zbuf_s)
!
      write(file_name, '(a7,a)') 'maxloc_', trim(fname_org)
      open(id_file_maxloc, file=file_name,                              &
     &     status='replace', FORM='UNFORMATTED', ACCESS='STREAM')
      call write_sph_pwr_layer_head                                     &
     &   (.FALSE., id_file_maxloc, sph_pwr_labels, sph_OUT1, zbuf_s)
!
      call allocate_max_sph_data(sph_IN)
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
     &      sph_IN%r_sph, sph_IN%i_mode, spectr_IN(1,0,1), zbuf1, ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. spec_evo_p%start_time) then
          sph_OUT1%time =   sph_IN%time
          sph_OUT1%i_step = sph_IN%i_step
          call find_dominant_scale_sph                                  &
     &       (sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,     &
     &        spectr_IN, max_spectr, max_degree)
          icou = icou + 1
!
          call sel_gz_write_layer_mean_mtr(.FALSE., id_file_maxval,     &
     &        sph_OUT1%i_step, sph_OUT1%time, sph_OUT1%nri_sph,         &
     &        sph_OUT1%kr_sph, sph_OUT1%r_sph, sph_OUT1%ntot_sph_spec,  &
     &        max_spectr(1,1), zbuf_s)
          call sel_gz_write_layer_mean_mtr(.FALSE., id_file_maxloc,     &
     &        sph_OUT1%i_step, sph_OUT1%time, sph_OUT1%nri_sph,         &
     &        sph_OUT1%kr_sph, sph_OUT1%r_sph, sph_OUT1%ntot_sph_spec,  &
     &        max_degree(1,1), zbuf_s)
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
      close(id_file_rms_l)
      close(id_file_maxval)
      close(id_file_maxloc)
!
!
      call deallocate_max_sph_data
      call dealloc_sph_espec_data(sph_IN)
      call dealloc_sph_espec_name(sph_IN)
      call dealloc_sph_espec_data(sph_OUT1)
      call dealloc_sph_espec_name(sph_OUT1)
!
      end subroutine sph_maximum_layer_spectr
!
!   --------------------------------------------------------------------
!
      subroutine find_dominant_scale_sph(nri_sph, ltr_sph, ncomp,       &
     &          spectr_l, max_data, max_mode)
!
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout) :: max_data(ncomp,nri_sph)
      real(kind = kreal), intent(inout) :: max_mode(ncomp,nri_sph)
!
      integer(kind = kint) :: kr, nd, lth
!
!
!$omp parallel private(kr,lth,nd)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp
          max_data(nd,kr) = spectr_l(nd,0,kr)
          max_mode(nd,kr) = 0
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      do lth = 1, ltr_sph
!$omp parallel private(kr,nd)
        do kr = 1, nri_sph
!$omp do
          do nd = 1, ncomp
            if(spectr_l(nd,lth,kr) .gt. max_data(nd,kr)) then
              max_data(nd,kr) = spectr_l(nd,lth,kr)
              max_mode(nd,kr) = lth
            end if
          end do
!$omp end do nowait
        end do
!$omp end parallel
      end do
!
      end subroutine find_dominant_scale_sph
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine allocate_max_sph_data(sph_IN)
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      allocate( max_spectr(sph_IN%ntot_sph_spec,sph_IN%nri_sph) )
      allocate( max_degree(sph_IN%ntot_sph_spec,sph_IN%nri_sph) )
!$omp parallel workshare
      max_spectr = 0.0d0
      max_degree = 0.0d0
!$omp end parallel workshare
!
      end subroutine allocate_max_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_max_sph_data
!
      deallocate(max_spectr, max_degree)
!
      end subroutine deallocate_max_sph_data
!
!   --------------------------------------------------------------------
!
      end module m_maxmode_sph_ene_spectr
