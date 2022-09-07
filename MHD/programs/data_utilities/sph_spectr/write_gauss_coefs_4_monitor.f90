!>@file   write_gauss_coefs_4_monitor.f90
!!@brief  module write_gauss_coefs_4_monitor
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays for gauss coefficients output
!!
!!@verbatim
!!      subroutine s_write_gauss_coefs_4_monitor                        &
!!     &         (id_rank, file_name, i_step, time, gauss_IO)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        type(picked_gauss_coefs_IO), intent(in) :: gauss_IO
!!@endverbatim
      module write_gauss_coefs_4_monitor
!
      use m_precision
      use m_constants
      use t_gauss_coefs_monitor_IO
!
      implicit  none
!
!>      File ID for Gauss coefficients IO
      integer(kind = kint), parameter :: id_gauss_coef = 24
!
      private :: open_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_write_gauss_coefs_4_monitor                          &
     &         (id_rank, file_name, i_step, time, gauss_IO)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
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
      call open_gauss_coefs_4_monitor(file_name, id_gauss_coef,         &
     &                                gauss_IO)
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
      end subroutine s_write_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_gauss_coefs_4_monitor(file_name, id_pick,         &
     &                                      gauss_IO)
!
      use m_monitor_file_labels
      use write_field_labels
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: id_pick
      type(picked_gauss_coefs_IO), intent(in) :: gauss_IO
!
!
      open(id_pick, file = file_name,                                   &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_pick, file = file_name,                                   &
     &    form='formatted', status='replace')
!
!
      write(id_pick,'(a)')   hd_pick_gauss_head()
      write(id_pick,'(i16,1pe25.15e3)')                                 &
     &     gauss_IO%num_mode, gauss_IO%radius_gauss
!
      write(id_pick,'(a)',advance='NO')  hd_time_label()
!
      call write_multi_labels(id_pick, gauss_IO%num_mode,               &
     &    gauss_IO%gauss_coef_name)
      write(id_pick,'(a)') ''
!
      end subroutine open_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
!
      end module write_gauss_coefs_4_monitor
