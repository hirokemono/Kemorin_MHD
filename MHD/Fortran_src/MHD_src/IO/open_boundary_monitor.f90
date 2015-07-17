!
!      module open_boundary_monitor
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine s_open_boundary_monitor(sf_grp)
!      subroutine close_boundary_monitor(my_rank)
!
      module open_boundary_monitor
!
      use m_precision
!
      implicit none
!
      integer(kind=kint), parameter :: boundary_monitor_code = 22
      character(len=kchara), parameter                                  &
     &      :: boundary_file_name = 'boundary_monitor.dat'
!
      private :: boundary_monitor_code, boundary_file_name
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_open_boundary_monitor(sf_grp)
!
      use t_group_data
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind = kint) :: i
!
!
      open (boundary_monitor_code,file=boundary_file_name,              &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
  99  continue
         open (boundary_monitor_code,file=boundary_file_name,           &
     &       status='replace')
!
         write(boundary_monitor_code,'(a)', advance='NO')               &
     &        't_step, time, h_flux: '
         do i = 1, sf_grp%num_grp
           write(boundary_monitor_code,'(a,a2)', advance='NO')          &
     &       trim(sf_grp%grp_name(i)), ', '
         end do
!
         write(boundary_monitor_code,'(a)', advance='NO') 'stress_x: '
         do i = 1, sf_grp%num_grp
           write(boundary_monitor_code,'(a,a2)', advance='NO')          &
     &       trim(sf_grp%grp_name(i)), ', '
         end do
!
         write(boundary_monitor_code,'(a)', advance='NO') 'stress_y: '
         do i = 1, sf_grp%num_grp
           write(boundary_monitor_code,'(a,a2)', advance='NO')          &
     &       trim(sf_grp%grp_name(i)), ', '
         end do
!
         write(boundary_monitor_code,'(a)', advance='NO') 'stress_z: '
         do i = 1, sf_grp%num_grp
           write(boundary_monitor_code,'(a,a2)', advance='NO')          &
     &       trim(sf_grp%grp_name(i)), ', '
         end do
         write(boundary_monitor_code,'(a)') ''
!
      end subroutine s_open_boundary_monitor
!
! ----------------------------------------------------------------------
!
      subroutine close_boundary_monitor
!
!
      close(boundary_monitor_code)
!
      end subroutine close_boundary_monitor
!
! ----------------------------------------------------------------------
!
      end module open_boundary_monitor
