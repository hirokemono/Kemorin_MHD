!
!      module open_boundary_monitor
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine s_open_boundary_monitor(my_rank)
!      subroutine close_boundary_monitor(my_rank)
!
      module open_boundary_monitor
!
      use m_precision
!
      use m_file_control_parameter
!
      implicit none
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_open_boundary_monitor(my_rank)
!
      use m_surface_group
!
      integer(kind=kint), intent(in) :: my_rank
      integer(kind = kint) :: i
!
!
       if ( my_rank .gt. 0 ) return
         write(time_step_data_file,'(a18)') 'boundary_monitor.dat'
!
         open (boundary_monitor_code,file=time_step_data_file,          &
     &       status='replace')
!
         write(boundary_monitor_code,'(a)', advance='NO')               &
     &        't_step, time, h_flux: '
         do i = 1, num_surf
           write(boundary_monitor_code,'(a,a2)', advance='NO')          &
     &       trim(surf_name(i)), ', '
         end do
!
         write(boundary_monitor_code,'(a)', advance='NO') 'stress_x: '
         do i = 1, num_surf
           write(boundary_monitor_code,'(a,a2)', advance='NO')          &
     &       trim(surf_name(i)), ', '
         end do
!
         write(boundary_monitor_code,'(a)', advance='NO') 'stress_y: '
         do i = 1, num_surf
           write(boundary_monitor_code,'(a,a2)', advance='NO')          &
     &       trim(surf_name(i)), ', '
         end do
!
         write(boundary_monitor_code,'(a)', advance='NO') 'stress_z: '
         do i = 1, num_surf
           write(boundary_monitor_code,'(a,a2)', advance='NO')          &
     &       trim(surf_name(i)), ', '
         end do
         write(boundary_monitor_code,'(a)') ''
!
      end subroutine s_open_boundary_monitor
!
! ----------------------------------------------------------------------
!
      subroutine close_boundary_monitor(my_rank)
!
      integer(kind=kint), intent(in) :: my_rank
!
      if ( my_rank .gt. 0 ) return
      close(boundary_monitor_code)
!
      end subroutine close_boundary_monitor
!
! ----------------------------------------------------------------------
!
      end module open_boundary_monitor
