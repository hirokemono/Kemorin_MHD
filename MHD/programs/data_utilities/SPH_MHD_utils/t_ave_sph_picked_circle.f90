!>@file   t_ave_sph_picked_circle.f90
!!@brief  module t_ave_sph_picked_circle
!!
!!@author H. Matsui
!!@date Programmed on June., 2013
!
!>@brief main program to take average of circle data
!
!
      program t_ave_sph_picked_circle
!
      use m_precision
      use m_spheric_parameter
      use m_tave_field_on_circle
!
      implicit none
!
      integer(kind = kint) :: ist, ied, ierr
      integer(kind = kint) :: icou, istep
      real(kind = kreal) :: time
!
!
      write(*,*) 'imput start and end step number'
      read(*,*) ist, ied
!
!    Evaluate time average
!
      write(*,*) 'open_read_field_data_on_circle'
      call open_read_field_data_on_circle(sph_rtp1, sph_rj1)
      call allocate_tave_circle_field
!
      icou = 0
      do
        write(*,*) 'read_field_data_on_circle'
        call read_field_data_on_circle(istep, time, ierr)
        write(*,*) 'read_field_data_on_circle end', istep, time, icou
        if(ierr.gt.0) go to 99
!
        if (istep .ge. ist) then
          icou = icou + 1
          call sum_average_circle_field
        end if
!
        if (istep .ge. ied) exit
!
        write(*,*) 'step', istep, 'averagind finished. Count: ', icou
      end do
   99 continue
!
      call close_field_data_on_circle
      call deallocate_circle_field
      call divide_average_circle_field(icou)
!
!
!  Evaluate standard deviation
!
      call open_read_field_data_on_circle(sph_rtp1, sph_rj1)
!
      icou = 0
      do
        call read_field_data_on_circle(istep, time, ierr)
        write(*,*) 'read_field_data_on_circle end', istep, time, icou
        if(ierr.gt.0) go to 98
!
        if (istep .ge. ist) then
          icou = icou + 1
          call sum_deviation_circle_field
        end if
!
        if (istep .ge. ied) exit
!
        write(*,*) 'step', istep, 'deviation finished. Count: ', icou
      end do
   98 continue
!
      call close_field_data_on_circle
      call divide_deviation_circle_field(icou)
!
      call copy_average_circle_field
      call write_field_data_on_circle(istep, time)
!
      call copy_deviation_circle_field
      call write_field_data_on_circle(istep, time)
      call deallocate_circle_field
!
      stop 'Finished'
      end program t_ave_sph_picked_circle
