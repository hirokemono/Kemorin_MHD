!>@file   t_picked_sph_spectr_data_IO.f90
!!@brief  module t_picked_sph_spectr_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine alloc_pick_sph_series(n_step, picked_IO)
!!      subroutine dealloc_pick_sph_monitor_IO(picked_IO)
!!      subroutine write_tave_sph_spec_monitor                          &
!!     &         (file_name, i_step, time, true_start, picked_IO)
!!        integer(kind = kint), intent(in) :: n_step
!!        type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!!
!!      subroutine read_pick_series_head(id_pick, picked_IO)
!!      subroutine read_sph_spec_monitor                                &
!!     &         (id_pick, i_step, time, picked_IO, ierr)
!!      subroutine copy_to_pick_sph_series                              &
!!     &         (icou, i_step, time, picked_IO)
!!        integer(kind = kint), intent(in) :: icou
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!!@endverbatim
!!
!!@n @param  id_rank   Process ID
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module t_picked_sph_spectr_data_IO
!
      use m_precision
      use m_constants
      use t_pickup_sph_spectr_data
!
      implicit  none
!
!>      File ID for spectrum monitor file
      integer(kind = kint), parameter :: id_pick_mode = 22
!
!
      type picked_spectrum_data_IO
!>        Number of radial layer for monitoring spectrum
        integer(kind = kint) :: num_layer = 0
!>        Number of modes of  monitoring spectrum to be evaluated
        integer(kind = kint) :: num_mode =  0
!>        Number of modes of monitoring spectrum in each process
        integer(kind = kint) :: ntot_pick_spectr = 0
!>        Number of components
        integer(kind = kint) :: ntot_comp = 0
!>        Number of data for each step
        integer(kind = kint) :: ntot_data = 0
!
!>        Global spherical harmonics ID to evaluate  monitoring spectrum
        integer(kind = kint), allocatable :: idx_sph(:,:)
!>        radius
        real(kind = kreal), allocatable :: radius(:)
!>        snapshot of monitoring spectrum
        real(kind = kreal), allocatable :: d_pk(:)
!>        Name of  monitoring spectrum
        character(len=kchara), allocatable :: spectr_name(:)
!
!>        Number of time series
        integer(kind = kint) :: n_step = 0
!>        Number of data for each step
        integer(kind = kint), allocatable :: i_step(:)
!>        snapshot of monitoring spectrum
        real(kind = kreal), allocatable :: d_time(:)
!>        snapshot of monitoring spectrum
        real(kind = kreal), allocatable :: d_pick(:,:)
!
        integer(kind = kint), allocatable :: istep_tmp(:)
        real(kind = kreal), allocatable :: time_tmp(:)
        real(kind = kreal), allocatable :: d_tmp(:,:)
!
      end type picked_spectrum_data_IO
!
      private :: alloc_pick_sph_monitor_IO
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_series(n_step, picked_IO)
!
      integer(kind = kint), intent(in) :: n_step
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
!
      picked_IO%n_step = n_step
!
      allocate(picked_IO%i_step(picked_IO%n_step))
      allocate(picked_IO%d_time(picked_IO%n_step))
      allocate(picked_IO%d_pick(picked_IO%ntot_data,picked_IO%n_step))
!
      if(picked_IO%n_step .gt. 0) then
        picked_IO%i_step = -1
        picked_IO%d_time = 0.0d0
        picked_IO%d_pick = 0.0d0
      end if
!
      end subroutine alloc_pick_sph_series
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_sph_series(picked_IO)
!
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
!
      deallocate(picked_IO%i_step, picked_IO%d_time)
      deallocate(picked_IO%d_pick)
!
      end subroutine dealloc_pick_sph_series
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_sph_monitor_IO(picked_IO)
!
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
!
      deallocate( picked_IO%idx_sph)
      deallocate( picked_IO%radius)
      deallocate( picked_IO%d_pk)

      deallocate( picked_IO%spectr_name)
!
      end subroutine dealloc_pick_sph_monitor_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_tave_sph_spec_monitor                            &
     &         (file_name, i_step, time, true_start, picked_IO)
!
      use m_monitor_file_labels
      use write_field_labels
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time, true_start
!
      type(picked_spectrum_data_IO), intent(in) :: picked_IO
!
      integer(kind = kint) :: ipick, i_fld, ist
!
!
      if(picked_IO%num_mode .eq. izero) return
!
      open(id_pick_mode, file = file_name, form='formatted',            &
     &     position='append')
!
      write(*,'(a,3i16)') hd_pick_sph_head(), picked_IO%num_layer,      &
     &        picked_IO%num_mode, picked_IO%ntot_pick_spectr
      write(*,'(a,i16)') hd_pick_sph_num(), picked_IO%ntot_comp
!
      write(*,'(a)')  '#   Start and end time'
      write(*,'(1p2e25.12)')  true_Start, time
!
      write(id_pick_mode,'(a)')    '#'
      write(id_pick_mode,'(a)')    '# num_layers, num_spectr'
      write(id_pick_mode,'(3i16)') picked_IO%num_layer,                 &
     &           picked_IO%num_mode, picked_IO%ntot_pick_spectr
      write(id_pick_mode,'(a)')    '# number of component'
      write(id_pick_mode,'(i16)') picked_IO%ntot_comp
!
      write(id_pick_mode,'(a)',advance='NO')  't_step    time    '
      write(id_pick_mode,'(a)',advance='NO')  'radius_ID    radius    '
      write(id_pick_mode,'(a)',advance='NO')  'degree    order    '
!
      call write_multi_labels(id_pick_mode, picked_IO%ntot_comp,        &
     &    picked_IO%spectr_name)
!
      do ipick = 1, picked_IO%ntot_pick_spectr
          ist = (ipick-1) * picked_IO%ntot_comp
          write(id_pick_mode,'(i16,1pe25.14e3)', advance='NO')          &
     &               i_step, time
          write(id_pick_mode,'(i16,1pe25.14e3,2i16)', advance='NO')     &
     &            picked_IO%idx_sph(ipick,1), picked_IO%radius(ipick),  &
     &            picked_IO%idx_sph(ipick,3:4)
          do i_fld = 1, picked_IO%ntot_comp
            write(id_pick_mode,'(1pe25.14e3)', advance='NO')            &
     &            picked_IO%d_pk(ist+i_fld)
          end do
          write(id_pick_mode,'(a)') ''
      end do
!
      close(id_pick_mode)
!
      end subroutine write_tave_sph_spec_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_pick_series_head(id_pick, picked_IO)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_pick
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
      integer(kind = kint) :: i
      character(len=kchara) :: tmpchara
!
!
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*,err=89) picked_IO%num_layer,                      &
     &               picked_IO%num_mode, picked_IO%ntot_pick_spectr
      go to 10
!
  89  continue
         picked_IO%ntot_pick_spectr                                     &
     &      = picked_IO%num_mode * picked_IO%num_layer
  10  continue
!
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) picked_IO%ntot_comp
!
      call alloc_pick_sph_monitor_IO(picked_IO)
!
      read(id_pick,*) (tmpchara,i=1,6),                                 &
     &                 picked_IO%spectr_name(1:picked_IO%ntot_comp)
!
      end subroutine read_pick_series_head
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_spec_time                                     &
     &         (id_pick, picked_IO, i_step, time, ierr)
!
      type(picked_spectrum_data_IO), intent(in) :: picked_IO
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
!
      integer(kind = kint) :: ipick
!
!
      ierr = 0
      do ipick = 1, picked_IO%ntot_pick_spectr
        read(id_pick,*,err=99,end=99) i_step, time
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_sph_spec_time
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_spec_monitor                                  &
     &         (id_pick, i_step, time, picked_IO, ierr)
!
      use spherical_harmonics
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
!
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
      integer(kind = kint) :: l, m, ipick, ist
!
!
      ierr = 0
      do ipick = 1, picked_IO%ntot_pick_spectr
        ist = (ipick-1) * picked_IO%ntot_comp
        read(id_pick,*,err=99,end=99) i_step, time,                     &
     &     picked_IO%idx_sph(id_pick,1), picked_IO%radius(id_pick),     &
     &     l, m, picked_IO%d_pk(ist+1:ist+picked_IO%ntot_comp)
        picked_IO%idx_sph(ipick,2) = get_idx_by_full_degree_order(l,m)
        picked_IO%idx_sph(ipick,3) = l
        picked_IO%idx_sph(ipick,4) = m
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_sph_spec_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_monitor_IO(picked_IO)
!
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
      integer(kind = kint) :: num
!
!
      num = picked_IO%ntot_pick_spectr
      picked_IO%ntot_data                                               &
     &    = picked_IO%ntot_comp * picked_IO%ntot_pick_spectr
!
      allocate( picked_IO%idx_sph(picked_IO%ntot_pick_spectr,4) )
      allocate( picked_IO%radius(num) )
      allocate( picked_IO%d_pk(picked_IO%ntot_data) )

      allocate( picked_IO%spectr_name(picked_IO%ntot_comp) )
!
      if(num .gt. 0) then
        picked_IO%idx_sph = -1
        picked_IO%radius = 0.0d0
        picked_IO%d_pk = 0.0d0
      end if
!
      end subroutine alloc_pick_sph_monitor_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_to_pick_sph_series                                &
     &         (icou, i_step, time, picked_IO)
!
      integer(kind = kint), intent(in) :: icou
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
!
      picked_IO%i_step(icou) = i_step
      picked_IO%d_time(icou) = time
!$omp parallel workshare
      picked_IO%d_pick(1:picked_IO%ntot_data,icou)                      &
     &                     = picked_IO%d_pk(1:picked_IO%ntot_data)
!$omp end parallel workshare
!
      end subroutine copy_to_pick_sph_series
!
! -----------------------------------------------------------------------
!
      end module t_picked_sph_spectr_data_IO
