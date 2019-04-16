!set_ele_id_peri.f90
!     module set_ele_id_peri
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_element_id_periodic                              &
!!     &         (c_size, nb_rng, nx, ny, ipe, jpe, kpe,                &
!!     &          i, j, k, element_id, element_id_gl)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module set_ele_id_peri
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_element_id_periodic                                &
     &         (c_size, nb_rng, nx, ny, ipe, jpe, kpe,                  &
     &          i, j, k, element_id, element_id_gl)
!
      use t_size_of_cube
      use t_neib_range_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind=kint), intent(in) :: nx, ny
      integer(kind = kint), intent(in) :: ipe, jpe, kpe
      integer(kind = kint), intent(in) :: i, j, k
      integer(kind = kint), intent(inout) :: element_id, element_id_gl
!
!
      element_id    =  element_id + 1
!
!
      element_id_gl =  (nb_rng%ioff + i  )                              &
     &               + (nb_rng%joff + j-1) * (c_size%nx_all - 1)        &
     &               + (nb_rng%koff + k-1) * (c_size%nx_all - 1)        &
     &                * (c_size%ny_all - 1)
!
      if(ipe .eq. 1 .and. i.le.c_size%ndepth ) then
        element_id_gl = (c_size%nx_all - 1) * (c_size%ny_all - 1)       &
     &                  * (c_size%nz_all - 1)                           &
     &                 + i                                              &
     &                 + (nb_rng%joff + j-1) * c_size%ndepth            &
     &                 + (nb_rng%koff + k-1) * (c_size%ny_all - 1)      &
     &                  * c_size%ndepth
      end if
!
      if(ipe .eq. c_size%ndx .and. i.ge. nx-c_size%ndepth ) then
        element_id_gl =  ( (c_size%nx_all - 1) * (c_size%ny_all - 1)    &
     &                    + c_size%ndepth * (c_size%ny_all - 1))        &
     &                   * (c_size%nz_all - 1)                          &
     &                 + (i-nx + c_size%ndepth+1)                       &
     &                 + (nb_rng%joff + j-1) * c_size%ndepth            &
     &                 + (nb_rng%koff + k-1) * (c_size%ny_all - 1)      &
     &                  * c_size%ndepth
      end if
!
      if(jpe .eq. 1 .and. j .le. c_size%ndepth) then
        element_id_gl =  ( (c_size%nx_all - 1) * (c_size%ny_all - 1)    &
     &                    + 2 * c_size%ndepth * (c_size%ny_all - 1))    &
     &                   * (c_size%nz_all - 1)                          &
     &                 + (nb_rng%ioff + i)                              &
     &                 + (j-1) * (c_size%nx_all - 1)                    &
     &                 + (nb_rng%koff + k-1) * (c_size%nx_all - 1)      &
     &                  * c_size%ndepth
      end if
!
      if(jpe .eq. c_size%ndy .and. j .ge. ny-c_size%ndepth) then
        element_id_gl =  ((c_size%nx_all - 1) * (c_size%ny_all - 1)     &
     &                 + 2 * c_size%ndepth * (c_size%ny_all - 1)        &
     &                     + c_size%ndepth * (c_size%nx_all - 1) )      &
     &                  * (c_size%nz_all - 1)                           &
     &                 + (nb_rng%ioff + i)                              &
     &                 + (j-ny + c_size%ndepth) * (c_size%nx_all - 1)   &
     &                 + (nb_rng%koff + k-1) * (c_size%nx_all - 1)      &
     &                  * c_size%ndepth
      end if
!
      if(ipe .eq. 1 .and. jpe .eq. 1) then
        if ( i.le. c_size%ndepth .and. j .le. c_size%ndepth ) then
          element_id_gl =  ((c_size%nx_all - 1) * (c_size%ny_all - 1)   &
     &                       + 2 * c_size%ndepth * (c_size%ny_all - 1)  &
     &                       + 2 * c_size%ndepth * (c_size%nx_all - 1)) &
     &                    * (c_size%nz_all - 1)                         &
     &                 + i + (j-1) * c_size%ndepth                      &
     &                 + (nb_rng%koff + k-1) * c_size%ndepth**2
        end if
      end if
!
      if(ipe .eq. c_size%ndx .and. jpe .eq. 1) then
        if ( i.ge. nx-c_size%ndepth .and. j .le. c_size%ndepth ) then
          element_id_gl =  ( (c_size%nx_all - 1) * (c_size%ny_all - 1)  &
     &               + 2 * c_size%ndepth * (c_size%ny_all - 1)          &
     &               + 2 * c_size%ndepth * (c_size%nx_all - 1)          &
     &               + c_size%ndepth**2) * (c_size%nz_all - 1)          &
     &               + (i-nx + c_size%ndepth+1) + (j-1) * c_size%ndepth &
     &               + (nb_rng%koff + k-1) * c_size%ndepth**2
        end if
      end if
!
      if(ipe .eq. c_size%ndx .and. jpe .eq. c_size%ndy) then
        if ( i.ge. nx-c_size%ndepth .and. j .ge. ny-c_size%ndepth ) then
          element_id_gl =  ( (c_size%nx_all - 1) * (c_size%ny_all - 1)  &
     &                 + 2 * c_size%ndepth * (c_size%ny_all - 1)        &
     &                 + 2 * c_size%ndepth * (c_size%nx_all - 1)        &
     &                  + 2 * c_size%ndepth**2) * (c_size%nz_all - 1)   &
     &                 + (i-nx + c_size%ndepth+1)                       &
     &                 + (j-ny + c_size%ndepth) * c_size%ndepth         &
     &                 + (nb_rng%koff + k-1) * c_size%ndepth**2
        end if
      end if
!
      if (ipe .eq. 1 .and. jpe .eq. c_size%ndy) then
        if ( i.le. c_size%ndepth .and. j .ge. ny-c_size%ndepth ) then
          element_id_gl =  ( (c_size%nx_all - 1) * (c_size%ny_all - 1)  &
     &                 + 2 * c_size%ndepth * (c_size%ny_all - 1)        &
     &                 + 2 * c_size%ndepth * (c_size%nx_all - 1)        &
     &                  + 3 * c_size%ndepth**2 ) * (c_size%nz_all - 1)  &
     &                 + i + (j-ny + c_size%ndepth) * c_size%ndepth     &
     &                 + (nb_rng%koff + k-1) * c_size%ndepth**2
        end if
      end if
!
      end subroutine set_element_id_periodic
!
!
! ----------------------------------------------------------------------
!
      end module set_ele_id_peri
