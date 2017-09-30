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
      use m_tave_field_on_circle
      use t_spheric_parameter
      use t_field_on_circle
      use field_on_circle_IO
!
      implicit none
!
      integer(kind = kint) :: ist, ied, ierr
      integer(kind = kint) :: icou, istep
      real(kind = kreal) :: time
!
      type(sph_grids), save :: sph_c
      type(circle_fld_maker), save :: cdat_a
!
!
      write(*,*) 'imput start and end step number'
      read(*,*) ist, ied
!
!    Evaluate time average
!
      write(*,*) 'open_read_field_data_on_circle'
      call open_read_field_data_on_circle(sph_c%sph_rtp, sph_c%sph_rj,  &
     &    cdat_a%circle, cdat_a%d_circle)
      call allocate_tave_circle_field(cdat_a%circle, cdat_a%d_circle)
!
      icou = 0
      do
        write(*,*) 'read_field_data_on_circle'
        call read_field_data_on_circle                                  &
     &     (istep, time, ierr, cdat_a%circle, cdat_a%d_circle)
        write(*,*) 'read_field_data_on_circle end', istep, time, icou
        if(ierr.gt.0) go to 99
!
        if (istep .ge. ist) then
          icou = icou + 1
          call sum_average_circle_field(cdat_a%circle, cdat_a%d_circle)
        end if
!
        if (istep .ge. ied) exit
!
        write(*,*) 'step', istep, 'averagind finished. Count: ', icou
      end do
   99 continue
!
      call close_field_data_on_circle
      call dealloc_circle_field(cdat_a%circle, cdat_a%d_circle)
      call divide_average_circle_field                                  &
     &   (icou, cdat_a%circle, cdat_a%d_circle)
!
!
!  Evaluate standard deviation
!
      call open_read_field_data_on_circle(sph_c%sph_rtp, sph_c%sph_rj,  &
     &    cdat_a%circle, cdat_a%d_circle)
!
      icou = 0
      do
        call read_field_data_on_circle                                  &
     &     (istep, time, ierr, cdat_a%circle, cdat_a%d_circle)
        write(*,*) 'read_field_data_on_circle end', istep, time, icou
        if(ierr.gt.0) go to 98
!
        if (istep .ge. ist) then
          icou = icou + 1
          call sum_deviation_circle_field                               &
     &       (cdat_a%circle, cdat_a%d_circle)
        end if
!
        if (istep .ge. ied) exit
!
        write(*,*) 'step', istep, 'deviation finished. Count: ', icou
      end do
   98 continue
!
      call close_field_data_on_circle
      call divide_deviation_circle_field                                &
     &   (icou, cdat_a%circle, cdat_a%d_circle)
!
      call copy_average_circle_field(cdat_a%circle, cdat_a%d_circle)
      call write_field_data_on_circle                                   &
     &   (istep, time, cdat_a%circle, cdat_a%d_circle)
!
      call copy_deviation_circle_field(cdat_a%circle, cdat_a%d_circle)
      call write_field_data_on_circle                                   &
     &   (istep, time, cdat_a%circle, cdat_a%d_circle)
      call dealloc_circle_field(cdat_a%circle, cdat_a%d_circle)
!
      stop 'Finished'
      end program t_ave_sph_picked_circle
