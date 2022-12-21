!>@file   append_sph_layer_mean.f90
!!        program append_sph_layer_mean
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2020
!!
!!
!> @brief Append mean square data file
!!
!!@verbatim
!!      subroutine append_sph_layer_mean_file                           &
!!     &         (append_file_name, target_file_name)
!!        character(len=kchara), intent(in) :: append_file_name
!!        character(len=kchara), intent(in) :: target_file_name
!!@endverbatim
      module append_sph_layer_mean
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_read_sph_spectra
      use t_pick_copy_monitor_data
      use t_buffer_4_gzip
!
      implicit none
!
      private :: pick_copy_sph_lmean_to_end
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine append_sph_layer_mean_file                             &
     &         (append_file_name, target_file_name)
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_spl_sph_spectr_data_IO
!
      implicit none
!
      character(len=kchara), intent(in) :: append_file_name
      character(len=kchara), intent(in) :: target_file_name
!
!
      type(read_sph_spectr_data), save :: sph_IN1
      type(read_sph_spectr_data), save :: sph_OUT1
      type(sph_spectr_head_labels), save :: sph_lbl_IN1
      type(sph_spectr_head_labels), save :: sph_lbl_OUT1
!
      integer(kind = kint), parameter :: id_read_file = 15
      integer(kind = kint), parameter :: id_write_file = 16
!
      integer(kind = kint) :: istep_start
      real(kind = kreal) :: start_time
!
      integer(kind = kint) :: ntot_pick
!
      logical :: flag_gzip1
      type(buffer_4_gzip), save :: zbuf1
      character, pointer, save  :: FPz_f1
      type(monitor_field_pickup_table), save :: comp_tbl1
!
!
      write(*,*) 'Open data file to append.'
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_read_file, append_file_name, flag_gzip1, zbuf1)
      call read_sph_layer_mean_head(FPz_f1, id_read_file, flag_gzip1,   &
     &    (.FALSE.), sph_lbl_IN1, sph_IN1, zbuf1)
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f1, id_read_file, flag_gzip1, zbuf1)
      read(zbuf1%fixbuf(1),*) istep_start, start_time
!
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_read_file, flag_gzip1, zbuf1)
!
!
      write(*,*) 'Open target file', ': ', trim(target_file_name)
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_write_file, target_file_name, flag_gzip1, zbuf1)
      call read_sph_layer_mean_head(FPz_f1, id_write_file, flag_gzip1,  &
     &    (.FALSE.), sph_lbl_OUT1, sph_OUT1, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_write_file, flag_gzip1, zbuf1)
!
!
      if(sph_IN1%nri_sph .ne. sph_OUT1%nri_sph) then
        write(*,*) '# of radial layer does not match',                  &
     &      sph_IN1%nri_sph, sph_OUT1%nri_sph
        stop
      end if
      if(sph_IN1%ltr_sph .ne. sph_OUT1%ltr_sph) then
        write(*,*) '# of truncation does not match',                    &
     &      sph_IN1%ltr_sph, sph_OUT1%ltr_sph
        stop
      end if
!
      call init_pick_copy_sph_pwr_list                                  &
     &   (sph_IN1%ntot_sph_spec, sph_OUT1%ntot_sph_spec,                &
     &    sph_IN1%ene_sph_spec_name(sph_IN1%num_time_labels+1),         &
     &    sph_OUT1%ene_sph_spec_name(sph_OUT1%num_time_labels+1),       &
     &    comp_tbl1)
      call dealloc_sph_espec_name(sph_IN1)
!
      ntot_pick = sph_OUT1%nri_sph
      write(*,*) 'ntot_pick', ntot_pick,                                &
     &         sph_OUT1%nri_sph, sph_OUT1%ltr_sph
!
      if(comp_tbl1%fast_flag .eqv. .FALSE.) then
        sph_OUT1%nri_dat = sph_OUT1%nri_sph
        call alloc_sph_spectr_data(izero, sph_OUT1)
      end if
!
      call open_bwd_serch_to_append(target_file_name, id_write_file,    &
     &    istep_start, start_time, ntot_pick)
!
      write(*,*) 'Open file to append again'
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_read_file, append_file_name, flag_gzip1, zbuf1)
      call read_sph_layer_mean_head(FPz_f1, id_read_file, flag_gzip1,   &
     &    (.FALSE.), sph_lbl_IN1, sph_IN1, zbuf1)
      call alloc_sph_spectr_data(izero, sph_IN1)
!
      if(comp_tbl1%fast_flag) then
        write(*,*) 'Copy data as text'
        call copy_sph_monitor_to_end(FPz_f1, id_read_file, flag_gzip1,  &
     &      id_write_file, ntot_pick, zbuf1)
      else
        write(*,*) 'Read and select data'
        call pick_copy_sph_lmean_to_end                                 &
     &     (FPz_f1, id_read_file, id_write_file, flag_gzip1,            &
     &      comp_tbl1, sph_IN1, sph_OUT1, zbuf1)
        call dealloc_monitor_fld_pickup_tbl(comp_tbl1)
      end if
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_read_file, flag_gzip1, zbuf1)
!
      close(id_write_file)
!
      call dealloc_sph_espec_data(sph_IN1)
      call dealloc_sph_espec_name(sph_IN1)
      if(comp_tbl1%fast_flag .eqv. .FALSE.) then
        call dealloc_sph_espec_data(sph_OUT1)
      end if
      call dealloc_sph_espec_name(sph_OUT1)
!
      end subroutine append_sph_layer_mean_file
!
! -----------------------------------------------------------------------
!
      subroutine pick_copy_sph_lmean_to_end                             &
     &         (FPz_f, id_read_file, id_write_file, flag_gzip,          &
     &          comp_tbl, sph_IN, sph_OUT, zbuf)
!
      use sel_gz_input_sph_mtr_head
      use gz_spl_sph_spectr_data_IO
      use write_sph_monitor_data
 !
      character, pointer, intent(in)  :: FPz_f
      integer(kind = kint), intent(in) :: id_read_file
      integer(kind = kint), intent(in) :: id_write_file
      logical, intent(in) :: flag_gzip
      type(monitor_field_pickup_table), intent(in) :: comp_tbl
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      real(kind = kreal), allocatable :: spectr_IN(:,:)
      real(kind = kreal), allocatable :: spectr_OUT(:,:)
      integer(kind = kint) :: ierr, n_mode
!
!
      allocate(spectr_IN(sph_IN%ntot_sph_spec,sph_IN%nri_sph))
      allocate(spectr_OUT(sph_OUT%ntot_sph_spec,sph_OUT%nri_sph))
!
!$omp parallel workshare
      spectr_IN(1:sph_IN%ntot_sph_spec,1:sph_IN%nri_sph) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
      spectr_OUT(1:sph_OUT%ntot_sph_spec,1:sph_OUT%nri_sph) = 0.0d0
!$omp end parallel workshare
!
      do
        call sel_gz_input_sph_layer_mean                                &
     &     (FPz_f, id_read_file, flag_gzip, flag_current_fmt,           &
     &      sph_IN%nri_sph, sph_IN%ntot_sph_spec, sph_IN%i_step,        &
     &      sph_IN%time, sph_IN%kr_sph, sph_IN%r_sph,                   &
     &      spectr_IN(1,1), zbuf, ierr)
        if(ierr .gt. 0) exit
!
        n_mode = sph_IN%nri_sph
        sph_OUT%i_step = sph_IN%i_step
        sph_OUT%time = sph_IN%time
        sph_OUT%kr_sph(1:sph_IN%nri_sph)                                &
     &               = sph_IN%kr_sph(1:sph_IN%nri_sph)
        sph_OUT%r_sph(1:sph_IN%nri_sph)                                 &
     &               = sph_IN%r_sph(1:sph_IN%nri_sph)
        call pick_copy_monitor_data                                     &
     &     (comp_tbl, sph_IN%ntot_sph_spec, sph_OUT%ntot_sph_spec,      &
     &      n_mode, spectr_IN(1,1), spectr_OUT(1,1))
        call write_layer_sph_data(id_write_file, sph_OUT,               &
     &                            spectr_OUT(1,1))
      end do
      deallocate(spectr_IN, spectr_OUT)
!
      end subroutine pick_copy_sph_lmean_to_end
!
! -----------------------------------------------------------------------
!
      end module append_sph_layer_mean
