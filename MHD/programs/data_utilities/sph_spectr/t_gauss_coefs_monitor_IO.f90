!>@file   t_gauss_coefs_monitor_IO.f90
!!@brief  module t_gauss_coefs_monitor_IO
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays for gauss coefficients output
!!
!!@verbatim
!!      subroutine dealloc_gauss_coef_monitor(gauss_IO)
!!      subroutine write_gauss_coefs_4_monitor                          &
!!     &         (id_rank, i_step, time, gauss_IO)
!!
!!      subroutine open_gauss_coefs_read_monitor(id_pick, gauss_IO)
!!      subroutine read_gauss_coefs_4_monitor(id_pick, i_step, time,    &
!!     &          gauss_IO, ierr)
!!@endverbatim
!!
!!@n @param  id_rank   Process ID
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module t_gauss_coefs_monitor_IO
!
      use m_precision
      use m_constants
      use t_pickup_sph_spectr_data
!
      implicit  none
!
!>      File ID for Gauss coefficients IO
      integer(kind = kint), parameter :: id_gauss_coef = 23
!
!
      type picked_gauss_coefs_IO
!>        File name of GAuss coefficients monitoring file
        character(len = kchara)                                         &
     &               :: gauss_coef_file_name =  'picked_gauss.dat'
!>        Radius for the gauss coefficients
        real(kind = kreal) :: radius_gauss
!
!>        Number of modes of  monitoring spectrum to be evaluated
        integer(kind = kint) :: num_mode =  0
!>        monitoring spectrum
        real(kind = kreal), allocatable :: gauss_coef(:)
!>        Number of modes of Gauss coefficients to be evaluated
!>        Name of Gauss coefficients  (g_{l}^{m} or h_{l}^{m})
        character(len=kchara), allocatable :: gauss_coef_name(:)
!
!>        Number of time series
        integer(kind = kint) :: n_step = 0
!>        Number of data for each step
        integer(kind = kint), allocatable :: i_step(:)
!>        time
        real(kind = kreal), allocatable :: d_time(:)
!>        Gauss coefficient time series
        real(kind = kreal), allocatable :: d_gauss(:,:)
      end type picked_gauss_coefs_IO
!
      private :: open_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_gauss_coef_monitor(gauss_IO)
!
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
!
      deallocate(gauss_IO%gauss_coef_name, gauss_IO%gauss_coef)
!
      end subroutine dealloc_gauss_coef_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_gauss_coefs_4_monitor(gauss_IO)
!
      use m_monitor_file_labels
      use write_field_labels
!
      type(picked_gauss_coefs_IO), intent(in) :: gauss_IO
!
!
      open(id_gauss_coef, file = gauss_IO%gauss_coef_file_name,         &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_gauss_coef, file = gauss_IO%gauss_coef_file_name,         &
     &    form='formatted', status='replace')
!
!
      write(id_gauss_coef,'(a)')   hd_pick_gauss_head()
      write(id_gauss_coef,'(i16,1pe25.15e3)')                           &
     &     gauss_IO%num_mode, gauss_IO%radius_gauss
!
      write(id_gauss_coef,'(a)',advance='NO')  hd_time_label()
!
      call write_multi_labels(id_gauss_coef, gauss_IO%num_mode,         &
     &    gauss_IO%gauss_coef_name)
      write(id_gauss_coef,'(a)') ''
!
      end subroutine open_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_gauss_coefs_4_monitor                            &
     &         (id_rank, i_step, time, gauss_IO)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      type(picked_gauss_coefs_IO), intent(in) :: gauss_IO
!
      integer(kind = kint) :: inum
!
!
      if(gauss_IO%num_mode .eq. izero) return
      if(id_rank .gt. izero) return
!
      call open_gauss_coefs_4_monitor(gauss_IO)
!
      write(id_gauss_coef,'(i16,1pe23.14e3)', advance='NO')             &
     &       i_step, time
      do inum = 1, gauss_IO%num_mode
        write(id_gauss_coef,'(1pe23.14e3)', advance='NO')               &
     &       gauss_IO%gauss_coef(inum)
      end do
      write(id_gauss_coef,'(a)') ''
!
      close(id_gauss_coef)
!
      end subroutine write_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_gauss_coefs_time_series(file_name, gauss_IO)
!
      use count_monitor_time_series
!
      character(len=kchara), intent(in) :: file_name
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
      integer(kind = kint) :: i, ierr
      integer(kind = kint) :: i_start, i_end
      real(kind = kreal) :: start_time, end_time
!
!
      write(*,*) 'Open file: ', trim(file_name)
      open(id_gauss_coef, file = file_name, position='append')
      backspace(id_gauss_coef)
      call read_sph_spectr_time                                         &
     &   (id_gauss_coef, ione, i_end, end_time, ierr)
      rewind(id_gauss_coef)
!
      call read_gauss_coefs_header(id_gauss_coef, gauss_IO)
      call alloc_gauss_coef_monitor(gauss_IO)
      call read_gauss_coefs_labels(id_gauss_coef, gauss_IO)
      call read_sph_spectr_time                                         &
     &   (id_gauss_coef, ione, i_start, start_time, ierr)
      close(id_gauss_coef)
!
      write(*,*) 'Start step and time: ', i_start, start_time
      write(*,*) 'End step and time: ', i_end, end_time
!
      write(*,*) 'Saved Gauss coefficients at r = ',                    &
     &          gauss_IO%radius_gauss, ': '
      do i = 1, gauss_IO%num_mode
        write(*,*) i, trim(gauss_IO%gauss_coef_name(i))
      end do
!
      call dealloc_gauss_coef_monitor(gauss_IO)
!
      end subroutine check_gauss_coefs_time_series
!
! -----------------------------------------------------------------------
!
      subroutine load_gauss_coefs_time_series                           &
     &         (flag_log, file_name, start_time, end_time,              &
     &          true_start, true_end, gauss_IO)
!
      use count_monitor_time_series
!
      logical, intent(in) :: flag_log
      character(len=kchara), intent(in) :: file_name
      real(kind = kreal), intent(in) :: start_time, end_time
!
      real(kind = kreal), intent(inout) :: true_start, true_end
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
      integer(kind = kint) :: num_count
!
!
      write(*,*) 'Open file: ', trim(file_name)
      open(id_gauss_coef, file = file_name)
!
      call read_gauss_coefs_header(id_gauss_coef, gauss_IO)
      call alloc_gauss_coef_monitor(gauss_IO)
      call read_gauss_coefs_labels(id_gauss_coef, gauss_IO)
!
      call s_count_monitor_time_series(flag_log, id_gauss_coef, ione,   &
     &    start_time, end_time, true_start, true_end, num_count)
      rewind(id_gauss_coef)
!
      call read_gauss_coefs_header(id_gauss_coef, gauss_IO)
      call read_gauss_coefs_labels(id_gauss_coef, gauss_IO)
!
      call alloc_gauss_coefs_series(num_count, gauss_IO)
      call read_gauss_coefs_series(flag_log, id_gauss_coef,             &
     &                             start_time, end_time, gauss_IO)
      close(id_gauss_coef)
!
      write(*,*) 'Start step and time: ', true_start, true_end, num_count
!
      end subroutine load_gauss_coefs_time_series
!
! -----------------------------------------------------------------------
!
      subroutine alloc_gauss_coefs_series(n_step, gauss_IO)
!
      integer(kind = kint), intent(in) :: n_step
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
!
      gauss_IO%n_step = n_step
!
      allocate(gauss_IO%i_step(gauss_IO%n_step))
      allocate(gauss_IO%d_time(gauss_IO%n_step))
      allocate(gauss_IO%d_gauss(gauss_IO%num_mode,gauss_IO%n_step))
!
      if(gauss_IO%n_step .gt. 0) then
        gauss_IO%i_step = -1
        gauss_IO%d_time = 0.0d0
        gauss_IO%d_gauss = 0.0d0
      end if
!
      end subroutine alloc_gauss_coefs_series
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_gauss_coefs_series(gauss_IO)
!
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
      deallocate(gauss_IO%i_step, gauss_IO%d_time, gauss_IO%d_gauss)
!
      end subroutine dealloc_gauss_coefs_series
!
! -----------------------------------------------------------------------
!
      subroutine read_gauss_coefs_series                                &
     &         (flag_log, id_pick, start_time, end_time, gauss_IO)
!
      use count_monitor_time_series
!
      logical, intent(in) :: flag_log
      integer(kind = kint), intent(in) :: id_pick
      real(kind = kreal), intent(in) :: start_time, end_time
!
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
      integer(kind = kint) :: icou, i_step, ierr, i
      real(kind = kreal) :: time
!
      icou = 0
      do
        call read_gauss_coefs_4_monitor                                 &
     &     (id_pick, i_step, time, gauss_IO, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          icou = icou + 1
          call copy_to_gauss_coefs_series(icou, i_step, time, gauss_IO)
!
          if(flag_log) then
            write(*,'(69a1,a5,i12,a4,1pe16.8e3,a20,i12)',advance="NO")  &
     &          (char(8),i=1,69), 'step ', i_step,                      &
     &          ' at ', time, ' is read. count is  ', icou
          end if
        end if
!
        if(time .ge. end_time) exit
      end do
      if(flag_log) write(*,*)
!
      end subroutine read_gauss_coefs_series
!
! -----------------------------------------------------------------------
!
      subroutine copy_to_gauss_coefs_series                             &
     &         (icou, i_step, time, gauss_IO)
!
      integer(kind = kint), intent(in) :: icou
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
!
      gauss_IO%i_step(icou) = i_step
      gauss_IO%d_time(icou) = time
!$omp parallel workshare
      gauss_IO%d_gauss(1:gauss_IO%num_mode,icou)                        &
     &                 = gauss_IO%gauss_coef(1:gauss_IO%num_mode)
!$omp end parallel workshare
!
      end subroutine copy_to_gauss_coefs_series
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_gauss_coefs_header(id_pick, gauss_IO)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_pick
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
      character(len=255) :: tmpchara
!
!
      gauss_IO%radius_gauss = 2.82
      call skip_comment(tmpchara,id_pick)
      read(id_pick,*) gauss_IO%num_mode, gauss_IO%radius_gauss
!
      end subroutine read_gauss_coefs_header
!
! -----------------------------------------------------------------------
!
      subroutine read_gauss_coefs_labels(id_pick, gauss_IO)
!
      integer(kind = kint), intent(in) :: id_pick
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
      integer(kind = kint) :: i
      character(len=255) :: tmpchara
!
      read(id_pick,*) (tmpchara,i=1,2),                                 &
     &               gauss_IO%gauss_coef_name(1:gauss_IO%num_mode)
!
      end subroutine read_gauss_coefs_labels
!
! -----------------------------------------------------------------------
!
      subroutine read_gauss_coefs_4_monitor(id_pick, i_step, time,      &
     &          gauss_IO, ierr)
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(inout) :: i_step, ierr
!
      real(kind = kreal), intent(inout) :: time
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
!
      ierr = 0
      read(id_pick,*,err=99,end=99) i_step, time,                       &
     &       gauss_IO%gauss_coef(1:gauss_IO%num_mode)
!
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_gauss_coef_monitor(gauss_IO)
!
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
!
      allocate(gauss_IO%gauss_coef_name(gauss_IO%num_mode) )
      allocate(gauss_IO%gauss_coef(gauss_IO%num_mode) )
!
      if(gauss_IO%num_mode .le. 0) return
!$omp parallel workshare
      gauss_IO%gauss_coef(1:gauss_IO%num_mode) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_gauss_coef_monitor
!
! -----------------------------------------------------------------------
!
      end module t_gauss_coefs_monitor_IO
