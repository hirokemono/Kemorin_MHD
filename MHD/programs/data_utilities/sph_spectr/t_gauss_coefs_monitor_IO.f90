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
!>        File prefix for spectr monitoring file
        character(len = kchara) :: file_prefix =  'picked_gauss'
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
      end type picked_gauss_coefs_IO
!
      private :: open_gauss_coefs_4_monitor
      private :: alloc_gauss_coef_monitor
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
      use set_parallel_file_name
      use write_field_labels
!
      type(picked_gauss_coefs_IO), intent(in) :: gauss_IO
      character(len = kchara) :: file_name
!
!
      file_name = add_dat_extension(gauss_IO%file_prefix)
      open(id_gauss_coef, file = file_name,                             &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_gauss_coef, file = file_name,                             &
     &    form='formatted', status='replace')
!
!
      write(id_gauss_coef,'(a)')    'num_spectr, reference_radius'
      write(id_gauss_coef,'(i16,1pe25.15e3)')                           &
     &     gauss_IO%num_mode, gauss_IO%radius_gauss
!
      write(id_gauss_coef,'(a)',advance='NO')    't_step    time    '
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
      subroutine open_gauss_coefs_read_monitor(id_pick, gauss_IO)
!
      use m_phys_labels
      use skip_comment_f
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: id_pick
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
      integer(kind = kint) :: i
      character(len = kchara) :: file_name
      character(len=255) :: tmpchara
!
!
      file_name = add_dat_extension(gauss_IO%file_prefix)
      open(id_pick, file = file_name)
!
      gauss_IO%radius_gauss = 2.82
      call skip_comment(tmpchara,id_pick)
      read(id_pick,*) gauss_IO%num_mode, gauss_IO%radius_gauss
!
!
      call alloc_gauss_coef_monitor(gauss_IO)
      read(id_pick,*) (tmpchara,i=1,2),                                 &
     &               gauss_IO%gauss_coef_name(1:gauss_IO%num_mode)
!
      end subroutine open_gauss_coefs_read_monitor
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
      write(*,*) 'i_step', i_step, time
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
      if(gauss_IO%num_mode .le 0) return
!$omp parallel workshare
      gauss_IO%gauss_coef(1:gauss_IO%num_mode)
!$omp end parallel workshare
!
      end subroutine alloc_gauss_coef_monitor
!
! -----------------------------------------------------------------------
!
      end module t_gauss_coefs_monitor_IO
