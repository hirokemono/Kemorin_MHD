!>@file   m_sph_uli_lengh_scale.f90
!!        module m_sph_uli_lengh_scale
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Obtain lengh scale from spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine sph_volume_uli_lscale_by_spec                        &
!!     &         (fname_org, spec_evo_p, sph_IN)
!!      subroutine sph_layer_uli_lscale_by_spec                         &
!!     &         (fname_org, spec_evo_p, sph_IN)
!!        logical, intent(in) :: flag_vol_ave
!!        type(sph_spectr_file_param), intent(in) :: spec_evo_p
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!@endverbatim
!
      module m_sph_uli_lengh_scale
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
!
      implicit none
!
!
      integer(kind = kint), parameter :: id_file_rms_l =    35
      integer(kind = kint), parameter :: id_file_rms_m =    37
      integer(kind = kint), parameter :: id_file_lscale =   44
!
      type(read_sph_spectr_data), save :: sph_OUT1
!
      real(kind = kreal), allocatable :: total_msq(:,:)
      real(kind = kreal), allocatable :: spec_times_l(:,:)
!
!
      private :: sph_OUT1, total_msq, spec_times_l
      private :: id_file_rms_l, id_file_rms_m, id_file_lscale
      private :: cal_uli_length_scale_sph
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sph_volume_uli_lscale_by_spec                          &
     &         (fname_org, spec_evo_p, sph_IN)
!
      use t_buffer_4_gzip
      use t_ctl_param_sph_series_util
      use select_gz_stream_file_IO
      use write_sph_monitor_data
      use sel_gz_input_sph_mtr_head
      use gz_volume_spectr_monitor_IO
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: fname_org
      type(sph_spectr_file_param), intent(in) :: spec_evo_p
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      character(len = kchara) :: file_name
      integer(kind = kint) :: i, icou, ierr, ist_true
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1, zbuf_s
      character, pointer :: FPz_f1
      type(sph_spectr_head_labels) :: sph_lbl_IN_u
      real(kind = kreal), allocatable :: spectr_l(:,:)
      real(kind = kreal), allocatable :: scale_uli(:)
!
!
      call sel_open_read_gz_stream_file(FPz_f1, id_file_rms_l,          &
     &                                    fname_org, flag_gzip1, zbuf1)
      call s_select_input_sph_series_head                               &
     &   (FPz_f1, id_file_rms_l, flag_gzip1,                            &
     &    spec_evo_p%flag_old_fmt, spectr_on, .TRUE.,                   &
     &    sph_lbl_IN_u, sph_IN, zbuf1)
!
      sph_IN%nri_dat = 1
      call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
      call check_sph_spectr_name(sph_IN)
!
      call copy_read_ene_params_4_sum(sph_IN, sph_OUT1)
!
      allocate(spectr_l(sph_IN%ntot_sph_spec,0:sph_IN%ltr_sph))
      allocate(scale_uli(sph_IN%ntot_sph_spec))
!$omp parallel workshare
      spectr_l(1:sph_IN%ntot_sph_spec,0:sph_IN%ltr_sph) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
      scale_uli(1:sph_IN%ntot_sph_spec) = 0.0d0
!$omp end parallel workshare
!
      write(file_name, '(a7,a)') 'lscale_', trim(fname_org)
      open(id_file_lscale, file=file_name)
      call select_output_sph_pwr_head                                   &
     &   (.FALSE., id_file_lscale, .TRUE., sph_OUT1, zbuf_s)
!
      call allocate_lscale_espec_data(sph_IN)
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
     &      spectr_l(1,0), zbuf1, ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. spec_evo_p%start_time) then
          sph_OUT1%time =   sph_IN%time
          sph_OUT1%i_step = sph_IN%i_step
          call cal_uli_length_scale_sph                                 &
     &       (ione, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,               &
     &        spectr_l(1,0), total_msq, spec_times_l, scale_uli(1))
          icou = icou + 1
!
          call write_vol_sph_data(id_file_lscale, sph_OUT1,             &
     &                            scale_uli(1))
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' evaluation finished. Count=  ', icou
        if (sph_IN%time .ge. spec_evo_p%end_time) exit
      end do
!
   99 continue
      write(*,*)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_file_rms_l, flag_gzip1, zbuf1)
      close(id_file_lscale)
!
!
      deallocate(scale_uli, spectr_l)
      call deallocate_lscale_espec_data
      call dealloc_sph_espec_data(sph_IN)
      call dealloc_sph_espec_name(sph_IN)
      call dealloc_sph_espec_data(sph_OUT1)
      call dealloc_sph_espec_name(sph_OUT1)
!
      end subroutine sph_volume_uli_lscale_by_spec
!
!   --------------------------------------------------------------------
!
      subroutine sph_layer_uli_lscale_by_spec                           &
     &         (fname_org, spec_evo_p, sph_IN)
!
      use t_buffer_4_gzip
      use t_ctl_param_sph_series_util
      use select_gz_stream_file_IO
      use write_sph_monitor_data
      use sel_gz_input_sph_mtr_head
      use gz_spl_sph_spectr_data_IO
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: fname_org
      type(sph_spectr_file_param), intent(in) :: spec_evo_p
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      character(len = kchara) :: file_name
      integer(kind = kint) :: i, icou, ierr, ist_true
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1, zbuf_s
      character, pointer :: FPz_f1
      type(sph_spectr_head_labels) :: sph_lbl_IN_u
      real(kind = kreal), allocatable :: scale_uli(:,:)
      real(kind = kreal), allocatable :: spectr_l(:,:,:)
!
!
      call sel_open_read_gz_stream_file(FPz_f1, id_file_rms_l,          &
     &                                    fname_org, flag_gzip1, zbuf1)
      call s_select_input_sph_series_head                               &
     &   (FPz_f1, id_file_rms_l, flag_gzip1,                            &
     &    spec_evo_p%flag_old_fmt, spectr_on, .FALSE.,                  &
     &    sph_lbl_IN_u, sph_IN, zbuf1)
!
      sph_IN%nri_dat = 1
      call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
      call check_sph_spectr_name(sph_IN)
!
      call copy_read_ene_params_4_sum(sph_IN, sph_OUT1)
!
      allocate(spectr_l(sph_IN%ntot_sph_spec,                           &
     &                  0:sph_IN%ltr_sph,1:sph_IN%nri_sph))
      allocate(scale_uli(sph_IN%ntot_sph_spec,1:sph_IN%nri_sph))
!$omp parallel workshare
      spectr_l(1:sph_IN%ntot_sph_spec,                                  &
     &         0:sph_IN%ltr_sph,1:sph_IN%nri_sph) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
      scale_uli(1:sph_IN%ntot_sph_spec,1:sph_IN%nri_sph) = 0.0d0
!$omp end parallel workshare
!
      write(file_name, '(a7,a)') 'lscale_', trim(fname_org)
      open(id_file_lscale, file=file_name)
      call select_output_sph_pwr_head                                   &
     &   (.FALSE., id_file_lscale, .FALSE., sph_OUT1, zbuf_s)
!
      call allocate_lscale_espec_data(sph_IN)
!
      icou = 0
      ist_true = -1
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' averaging finished. Count=  ', icou
      do
        call sel_gz_input_sph_layer_spec                                &
     &     (FPz_f1, id_file_rms_l, flag_gzip1, spec_evo_p%flag_old_fmt, &
     &      sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,       &
     &      sph_IN%i_step, sph_IN%time, sph_IN%kr_sph,                  &
     &      sph_IN%r_sph, sph_IN%i_mode, spectr_l, zbuf1, ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. spec_evo_p%start_time) then
          sph_OUT1%time =   sph_IN%time
          sph_OUT1%i_step = sph_IN%i_step
          call cal_uli_length_scale_sph                                 &
     &       (sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,     &
     &        spectr_l(1,0,1), total_msq, spec_times_l, scale_uli(1,1))
          icou = icou + 1
!
          call write_layer_sph_data(id_file_lscale, sph_OUT1,           &
     &                              scale_uli(1,1))
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' evaluation finished. Count=  ', icou
        if (sph_IN%time .ge. spec_evo_p%end_time) exit
      end do
!
   99 continue
      write(*,*)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_file_rms_l, flag_gzip1, zbuf1)
      close(id_file_lscale)
!
!
      deallocate(scale_uli, spectr_l)
      call deallocate_lscale_espec_data
      call dealloc_sph_espec_data(sph_IN)
      call dealloc_sph_espec_name(sph_IN)
      call dealloc_sph_espec_data(sph_OUT1)
      call dealloc_sph_espec_name(sph_OUT1)
!
      end subroutine sph_layer_uli_lscale_by_spec
!
!   --------------------------------------------------------------------
!
      subroutine cal_uli_length_scale_sph(nri_sph, ltr_sph, ncomp,      &
     &          spectr_l, total_msq, spec_times_l, scale_uli)
!
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout)                                 &
     &                    :: total_msq(ncomp,nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                    :: spec_times_l(ncomp,nri_sph)
      real(kind = kreal), intent(inout) :: scale_uli(ncomp,nri_sph)
!
      integer(kind = kint) :: kr, nd, lth
!
!
!$omp parallel private(kr,nd)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp
          total_msq(nd,kr) =    spectr_l(nd,0,kr)
          spec_times_l(nd,kr) = 0.0d0
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
            total_msq(nd,kr) = total_msq(nd,kr)                         &
     &                           + spectr_l(nd,lth,kr)
            spec_times_l(nd,kr) = spec_times_l(nd,kr)                   &
     &                           + spectr_l(nd,lth,kr) * dble(lth)
          end do
!$omp end do nowait
        end do
!$omp end parallel
      end do
!
!$omp parallel private(kr,nd)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp
          if(total_msq(nd,kr) .le. 0.0d0) then
            scale_uli(nd,kr) = 0.0d0
          else
            scale_uli(nd,kr) = spec_times_l(nd,kr) / total_msq(nd,kr)
          end if
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine cal_uli_length_scale_sph
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine allocate_lscale_espec_data(sph_IN)
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      allocate( total_msq(sph_IN%ntot_sph_spec,sph_IN%nri_sph) )
      allocate( spec_times_l(sph_IN%ntot_sph_spec,sph_IN%nri_sph) )
      total_msq = 0.0d0
      spec_times_l = 0.0d0
!
      end subroutine allocate_lscale_espec_data
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_lscale_espec_data
!
      deallocate(total_msq, spec_times_l)
!
      end subroutine deallocate_lscale_espec_data
!
!   --------------------------------------------------------------------
!
      end module m_sph_uli_lengh_scale
