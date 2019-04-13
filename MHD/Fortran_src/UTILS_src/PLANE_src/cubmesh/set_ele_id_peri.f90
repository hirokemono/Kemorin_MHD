!set_ele_id_peri.f90
!     module set_ele_id_peri
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_element_id_periodic(nb_rng, ipe, jpe, kpe,      &
!!     &          i, j, k, element_id, element_id_gl)
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
      subroutine set_element_id_periodic(nb_rng, ipe, jpe, kpe,        &
     &          i, j, k, element_id, element_id_gl)
!
      use t_neib_range_cube
      use m_size_4_plane
      use m_size_of_cube
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer(kind = kint), intent(in) :: ipe, jpe, kpe
      integer(kind = kint), intent(in) :: i, j, k
      integer(kind = kint), intent(inout) :: element_id, element_id_gl
!
!
      element_id    =  element_id + 1
!
!
      element_id_gl =  (nb_rng%ioff + i  )                              &
     &               + (nb_rng%joff + j-1)*(nx_all-1)                   &
     &               + (nb_rng%koff + k-1)*(nx_all-1)*(ny_all-1)
!
      if (ipe .eq. 1 .and. i.le.ndepth ) then
        element_id_gl = (nx_all-1)*(ny_all-1)*(nz_all-1)                &
     &                 + i                                              &
     &                 + (nb_rng%joff + j-1) * ndepth                   &
     &                 + (nb_rng%koff + k-1)*(ny_all-1) * ndepth
      end if
!
      if (ipe .eq. ndx .and. i.ge. nx-ndepth ) then
        element_id_gl =  ( (nx_all-1)*(ny_all-1)                        &
     &                 + ndepth*(ny_all-1) ) * (nz_all-1)               &
     &                 + (i-nx+ndepth+1)                                &
     &                 + (nb_rng%joff + j-1) * ndepth                   &
     &                 + (nb_rng%koff + k-1)*(ny_all-1) * ndepth
      end if
!
      if (jpe .eq. 1 .and. j.le. ndepth ) then
        element_id_gl =  ( (nx_all-1)*(ny_all-1)                        &
     &                 + 2*ndepth*(ny_all-1) ) * (nz_all-1)             &
     &                 + (nb_rng%ioff + i)                              &
     &                 + (j-1) *(nx_all-1)                              &
     &                 + (nb_rng%koff + k-1)*(nx_all-1) * ndepth
      end if
!
      if (jpe .eq. ndy .and. j.ge. ny-ndepth ) then
        element_id_gl =  ( (nx_all-1)*(ny_all-1)                        &
     &                 + 2*ndepth*(ny_all-1)                            &
     &                 +   ndepth*(nx_all-1) ) * (nz_all-1)             &
     &                 + (nb_rng%ioff + i)                              &
     &                 + (j-ny+ndepth) *(nx_all-1)                      &
     &                 + (nb_rng%koff + k-1)*(nx_all-1) * ndepth
      end if
!
      if (ipe .eq. 1 .and. jpe .eq. 1) then
        if ( i.le. ndepth .and. j .le. ndepth ) then
          element_id_gl =  ( (nx_all-1)*(ny_all-1)                      &
     &                 + 2*ndepth*(ny_all-1)                            &
     &                 + 2*ndepth*(nx_all-1) ) * (nz_all-1)             &
     &                 + i + (j-1) * ndepth                             &
     &                 + (nb_rng%koff + k-1) * ndepth**2
        end if
      end if
!
      if (ipe .eq. ndx .and. jpe .eq. 1) then
        if ( i.ge. nx-ndepth .and. j .le. ndepth ) then
          element_id_gl =  ( (nx_all-1)*(ny_all-1)                      &
     &                 + 2*ndepth*(ny_all-1)                            &
     &                 + 2*ndepth*(nx_all-1)                            &
     &                  + ndepth**2) * (nz_all-1)                       &
     &                 + (i-nx+ndepth+1) + (j-1) * ndepth               &
     &                 + (nb_rng%koff + k-1) * ndepth**2
        end if
      end if
!
      if (ipe .eq. ndx .and. jpe .eq. ndy) then
        if ( i.ge. nx-ndepth .and. j .ge. ny-ndepth ) then
          element_id_gl =  ( (nx_all-1)*(ny_all-1)                      &
     &                 + 2*ndepth*(ny_all-1)                            &
     &                 + 2*ndepth*(nx_all-1)                            &
     &                  + 2*ndepth**2) * (nz_all-1)                     &
     &                 + (i-nx+ndepth+1)                                &
     &                 + (j-ny+ndepth) * ndepth                         &
     &                 + (nb_rng%koff + k-1) * ndepth**2
        end if
      end if
!
      if (ipe .eq. 1 .and. jpe .eq. ndy) then
        if ( i.le. ndepth .and. j .ge. ny-ndepth ) then
          element_id_gl =  ( (nx_all-1)*(ny_all-1)                      &
     &                 + 2*ndepth*(ny_all-1)                            &
     &                 + 2*ndepth*(nx_all-1)                            &
     &                  + 3*ndepth**2 ) * (nz_all-1)                    &
     &                 + i + (j-ny+ndepth) * ndepth                     &
     &                 + (nb_rng%koff + k-1) * ndepth**2
        end if
      end if
!
      end subroutine set_element_id_periodic
!
!
! ----------------------------------------------------------------------
!
      end module set_ele_id_peri
