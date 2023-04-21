!>@file   t_sph_layer_spec_series.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time series of layerd spectr data 
!!
!!@verbatim
!!      subroutine load_sph_layer_spec_file(fname_org, flag_old_fmt,    &
!!     &          start_time, end_time, true_start, true_end,           &
!!     &          sph_lbl_IN, sph_IN, rs_srs)
!!      subroutine dealloc_sph_layer_spec_series(rs_srs)
!!        character(len = kchara), intent(in) :: fname_org
!!        logical, intent(in) :: flag_old_fmt
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!        real(kind = kreal), intent(inout) :: true_start, true_end
!!        type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        type(sph_layer_spectr_series), intent(inout) :: rs_srs
!!
!!      subroutine alloc_sph_layer_spec_series(n_step, nri, ltr, ncomp, &
!!     &                                        rs_srs)
!!        integer(kind = kint), intent(in) :: n_step, nri, ncomp
!!        type(sph_layer_spectr_series), intent(inout) :: rs_srs
!!      subroutine copy_sph_volume_spec_series(icou, ltr_in, ntot_in,   &
!!     &          i_step, time, spectr_IO, rs_srs)
!!        integer(kind = kint), intent(in) :: icou, ltr_in, ntot_in
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        real(kind = kreal), intent(in) :: spectr_IO(ntot_in)
!!        type(sph_layer_spectr_series), intent(inout) :: rs_srs
!!@endverbatim
      module t_sph_layer_spec_series
!
      use m_precision
      use m_constants
!
      use t_read_sph_spectra
      use t_buffer_4_gzip
!
      implicit none
!
!
      type sph_layer_spectr_series
!>        Number of time series
        integer(kind = kint) :: n_step =    0
!>        Number of time series
        integer(kind = kint) :: ntot_comp = 0
!>        Number of Radial points
        integer(kind = kint) :: nri_srs =   0
!>        Number of truncation
        integer(kind = kint) :: ltr_srs =   0
!>        Number of data for each step
        integer(kind = kint), allocatable :: i_step(:)
!>        time
        real(kind = kreal), allocatable :: d_time(:)
!>        spectr time series
        real(kind = kreal), allocatable :: rspec_series(:,:,:,:)
!>        spectr snap shot to loading
        real(kind = kreal), allocatable :: rspec_snap(:,:,:)
      end type sph_layer_spectr_series
!
      integer(kind = kint), parameter, private :: id_file_rms = 34
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine load_sph_layer_spec_file(fname_org, flag_old_fmt,      &
     &          start_time, end_time, true_start, true_end,             &
     &          sph_lbl_IN, sph_IN, rs_srs)
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_spl_sph_spectr_data_IO
      use set_parallel_file_name
      use count_monitor_time_series
!
      character(len = kchara), intent(in) :: fname_org
      logical, intent(in) :: flag_old_fmt
      real(kind = kreal), intent(in) :: start_time, end_time
      real(kind = kreal), intent(inout) :: true_start, true_end
!
      type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(sph_layer_spectr_series), intent(inout) :: rs_srs
!
      real(kind = kreal) :: prev_time
      integer(kind = kint) :: icou, ierr, ist_true, i, num
      integer(kind = kint) :: num_count, icou_skip
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1
      character, pointer :: FPz_f1
!
!
      call sel_open_read_gz_stream_file(FPz_f1, id_file_rms,            &
     &                                  fname_org, flag_gzip1, zbuf1)
      call read_sph_layer_spectr_head(FPz_f1, id_file_rms, flag_gzip1,  &
     &    flag_old_fmt, sph_lbl_IN, sph_IN, zbuf1)
!
      num = sph_IN%nri_sph * (sph_IN%ltr_sph + 1)
      call s_count_monitor_time_series                                  &
     &   (.TRUE., FPz_f1, id_file_rms, flag_gzip1, num,                 &
     &    start_time, end_time, true_start, true_end,                   &
     &    num_count, icou_skip, zbuf1)
      call dealloc_sph_espec_name(sph_IN)
!
      call sel_redwind_gz_stream_file(FPz_f1, id_file_rms, flag_gzip1)
      call read_sph_layer_mean_head(FPz_f1, id_file_rms, flag_gzip1,    &
     &    flag_old_fmt, sph_lbl_IN, sph_IN, zbuf1)
      call s_skip_monitor_time_series                                   &
     &   (.TRUE., FPz_f1, id_file_rms, flag_gzip1, num,                 &
     &    icou_skip, zbuf1)
!
!
      call alloc_sph_spectr_data(izero, sph_IN)
      call alloc_sph_layer_spec_series                                  &
     &   (num_count, sph_IN%nri_sph, sph_IN%ltr_sph,                    &
     &    sph_IN%ntot_sph_spec, rs_srs)
!
      icou = 0
      ist_true = -1
      prev_time = sph_IN%time
      write(*,'(a6,i12,a8,f12.6,a15,i12)',advance="NO")                 &
     &       'step= ', sph_IN%i_step, ', time= ', sph_IN%time,          &
     &       ', Load Count:  ', icou
      do
        call sel_gz_input_sph_layer_spec                                &
     &     (FPz_f1, id_file_rms, flag_gzip1, flag_old_fmt,              &
     &      sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,       &
     &      sph_IN%i_step, sph_IN%time, sph_IN%kr_sph,                  &
     &      sph_IN%r_sph, sph_IN%i_mode, rs_srs%rspec_snap(1,0,1),      &
     &      zbuf1, ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. start_time) then
          icou = icou + 1
          call copy_sph_layer_spec_series                               &
     &       (icou, sph_IN%i_step, sph_IN%time, rs_srs)
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
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_file_rms, flag_gzip1, zbuf1)
!
      end subroutine load_sph_layer_spec_file
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_layer_spec_series(rs_srs)
!
      type(sph_layer_spectr_series), intent(inout) :: rs_srs
!
!
      deallocate(rs_srs%i_step, rs_srs%d_time)
      deallocate(rs_srs%rspec_series, rs_srs%rspec_snap)
!
      end subroutine dealloc_sph_layer_spec_series
!
! --------------------------------------------------------------------
! --------------------------------------------------------------------
!
      subroutine alloc_sph_layer_spec_series(n_step, nri, ltr, ncomp,   &
     &                                       rs_srs)
!
      integer(kind = kint), intent(in) :: n_step, nri, ltr, ncomp
      type(sph_layer_spectr_series), intent(inout) :: rs_srs
!
!
      rs_srs%ntot_comp = ncomp
      rs_srs%nri_srs = nri
      rs_srs%ltr_srs = ltr
      rs_srs%n_step = n_step
      allocate(rs_srs%i_step(rs_srs%n_step))
      allocate(rs_srs%d_time(rs_srs%n_step))
      allocate(rs_srs%rspec_series(ncomp,0:ltr,nri,rs_srs%n_step))
      allocate(rs_srs%rspec_snap(ncomp,0:ltr,nri))
!
!$omp parallel workshare
      rs_srs%i_step(1:rs_srs%n_step) = izero
      rs_srs%d_time(1:rs_srs%n_step) = zero
!$omp end parallel workshare
!$omp parallel workshare
      rs_srs%rspec_series(1:ncomp,0:ltr,1:nri,1:rs_srs%n_step) =  zero
!$omp end parallel workshare
!$omp parallel workshare
      rs_srs%rspec_snap(1:ncomp,0:ltr,1:nri) =  zero
!$omp end parallel workshare
!
      end subroutine alloc_sph_layer_spec_series
!
!   --------------------------------------------------------------------
!
      subroutine copy_sph_layer_spec_series(icou, i_step, time, rs_srs)
!
      integer(kind = kint), intent(in) :: icou
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      type(sph_layer_spectr_series), intent(inout) :: rs_srs
!
      integer(kind = kint) :: ncomp, ltr, nri
!
      ncomp = rs_srs%ntot_comp
      ltr =   rs_srs%ltr_srs
      nri =   rs_srs%nri_srs
      rs_srs%i_step(icou) = i_step
      rs_srs%d_time(icou) = time
!$omp parallel workshare
      rs_srs%rspec_series(1:ncomp,0:ltr,1:nri,icou)                     &
     &                = rs_srs%rspec_snap(1:ncomp,0:ltr,1:nri)
!$omp end parallel workshare
!
      end subroutine copy_sph_layer_spec_series
!
!   --------------------------------------------------------------------
!
      end module t_sph_layer_spec_series
