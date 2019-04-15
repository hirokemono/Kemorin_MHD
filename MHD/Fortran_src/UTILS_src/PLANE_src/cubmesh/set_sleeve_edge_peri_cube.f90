!set_sleeve_edge_peri_cube.f90
!     module set_sleeve_edge_peri_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
! ***** set and write for sleeve edges for periodical boundary
!
!!      subroutine set_sleeve_edge_peri                                 &
!!     &         (c_size, nb_rng, ipe, jpe, kpe, inod)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module set_sleeve_edge_peri_cube
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
      subroutine set_sleeve_edge_peri                                   &
     &         (c_size, nb_rng, ipe, jpe, kpe, inod)
!
      use t_size_of_cube
      use t_neib_range_cube
      use t_sleeve_cube
      use m_size_4_plane
      use m_size_of_cube
      use sleeve_edge_side_cube
      use sleeve_edge_corner_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe, kpe
      integer (kind = kint), intent(inout) :: inod
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: ioff_gl
      integer (kind = kint) :: inp, jnp, knp
!
!
!  outdside (x<xmin)
!

      if (ipe .eq. 1 ) then
       inp = -1
       do knp = nb_rng%knp_st, nb_rng%knp_end
        do jnp = nb_rng%jnp_st, nb_rng%jnp_end

         call set_sleeve_size                                           &
     &      (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
         sl_rng1%is = 1
         sl_rng1%ie = ndepth
!
          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all
          call set_sleeve_edge_xmin(nb_rng, sl_rng1, kpe, jnp, knp,     &
     &        ioff_gl, ione, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &             + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all           &
     &             +   ndepth*ny_all*nz_all
          call set_sleeve_edge_xmin(nb_rng, sl_rng1, kpe, jnp, knp,     &
     &        ioff_gl, itwo, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &             + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all           &
     &             + 2*ndepth*ny_all*nz_all
          call set_sleeve_edge_xmin(nb_rng, sl_rng1, kpe, jnp, knp,     &
     &        ioff_gl, ithree, inod)

        enddo
       enddo
      endif


!
!  outdside (x>xmax)
!
      if (ipe .eq. ndx ) then
       do knp = nb_rng%knp_st, nb_rng%knp_end
        do jnp = nb_rng%jnp_st, nb_rng%jnp_end

         call set_sleeve_size                                           &
     &      (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
         sl_rng1%is = 1
         sl_rng1%ie = ndepth

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*ny_all*(3*nz_all-1)
          call set_sleeve_edge_xmax(c_size1, nb_rng, sl_rng1,           &
     &        kpe, jnp, knp, ioff_gl, ione, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*ny_all*(3*nz_all-1)                          &
     &            + (ndepth-1)*ny_all*nz_all
          call set_sleeve_edge_xmax(c_size1, nb_rng, sl_rng1,           &
     &        kpe, jnp, knp, ioff_gl, itwo, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*ny_all*(4*nz_all-1)                          &
     &            + (ndepth-1)*ny_all*nz_all
          call set_sleeve_edge_xmax(c_size1, nb_rng, sl_rng1,           &
     &        kpe, jnp, knp, ioff_gl, ithree, inod)

         enddo
        enddo
       endif

!
!  outdside (y<ymin)
!
      if ( jpe .eq. 1 ) then
       do knp = nb_rng%knp_st, nb_rng%knp_end
        do inp = nb_rng%inp_st, nb_rng%inp_end

         call set_sleeve_size                                           &
     &      (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
          sl_rng1%js = 1
          sl_rng1%je = ndepth

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*ny_all*(5*nz_all-2)                          &
     &            + (ndepth-1)*ny_all*nz_all
          call set_sleeve_edge_ymin(nb_rng, sl_rng1, kpe, inp, knp,     &
     &        ioff_gl, ione, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*ny_all*(5*nz_all-2)                          &
     &            + (ndepth-1)*ny_all*nz_all                            &
     &            + ndepth*nx_all*nz_all
          call set_sleeve_edge_ymin(nb_rng, sl_rng1, kpe, inp, knp,     &
     &        ioff_gl, itwo, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*ny_all*(5*nz_all-2)                          &
     &            + (ndepth-1)*ny_all*nz_all                            &
     &            + 2*ndepth*nx_all*nz_all
          call set_sleeve_edge_ymin(nb_rng, sl_rng1, kpe, inp, knp,     &
     &        ioff_gl, ithree, inod)

         enddo
        enddo
       endif
!
!  outdside (y>ymax)
!
       if ( jpe .eq. ndy ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
         do inp = nb_rng%inp_st, nb_rng%inp_end

         call set_sleeve_size                                           &
     &      (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
         sl_rng1%js = 1
         sl_rng1%je = ndepth

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*ny_all*(5*nz_all-2)                          &
     &            + (ndepth-1)*ny_all*nz_all                            &
     &            + ndepth*nx_all*(3*nz_all-1)
          call set_sleeve_edge_ymax(c_size1, nb_rng, sl_rng1,           &
     &        kpe, inp, knp, ioff_gl, ione, inod)
!
          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*ny_all*(5*nz_all-2)                          &
     &            + (ndepth-1)*ny_all*nz_all                            &
     &            + ndepth*nx_all*(4*nz_all-1)
          call set_sleeve_edge_ymax(c_size1, nb_rng, sl_rng1,           &
     &        kpe, inp, knp, ioff_gl, itwo, inod)
!
          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*ny_all*(5*nz_all-2)                          &
     &            + (ndepth-1)*ny_all*nz_all                            &
     &            + ndepth*nx_all*(4*nz_all-1)                          &
     &            + (ndepth-1)*nx_all*nz_all
          call set_sleeve_edge_ymax(c_size, nb_rng, sl_rng1,            &
     &        kpe, inp, knp, ioff_gl, ithree, inod)

        enddo
       enddo
      endif
!
!  outdside corner (x<xmin, y<ymin)
!
      if ( ipe .eq. 1  .and. jpe .eq. 1 ) then
       do knp = nb_rng%knp_st, nb_rng%knp_end

         call set_sleeve_size                                           &
     &      (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
         sl_rng1%is = 1
         sl_rng1%ie = ndepth
         sl_rng1%js = 1
         sl_rng1%je = ndepth

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*(nx_all+ny_all)*(5*nz_all-2)                 &
     &            + (ndepth-1)*(nx_all+ny_all)*nz_all
          call set_sleeve_edge_xmin_ymin(c_size1, sl_rng1, kpe, knp,    &
     &        ioff_gl, nb_rng%koff, ione, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*(nx_all+ny_all)*(5*nz_all-2)                 &
     &            + (ndepth-1)*(nx_all+ny_all)*nz_all                   &
     &            + ndepth*ndepth*nz_all
          call set_sleeve_edge_xmin_ymin(c_size1, sl_rng1, kpe, knp,    &
     &        ioff_gl, nb_rng%koff, itwo, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*(nx_all+ny_all)*(5*nz_all-2)                 &
     &            + (ndepth-1)*(nx_all+ny_all)*nz_all                   &
     &            + 2*ndepth*ndepth*nz_all
          call set_sleeve_edge_xmin_ymin(c_size1, sl_rng1, kpe, knp,    &
     &        ioff_gl, nb_rng%koff, ithree, inod)

       enddo
      endif
!
!  outdside (x>xmax, y<ymin)
!
      if ( ipe .eq. ndx  .and. jpe .eq. 1 ) then
       do knp = nb_rng%knp_st, nb_rng%knp_end

         call set_sleeve_size                                           &
     &      (nb_rng, ndepth, inp, jnp, knp, sl_rng1)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*(nx_all+ny_all)*(5*nz_all-2)                 &
     &            + (ndepth-1)*(nx_all+ny_all)*nz_all                   &
     &            + ndepth*ndepth*(3*nz_all-1)
          call set_sleeve_edge_xmax_ymin(c_size1, sl_rng1, kpe, knp,    &
     &        ioff_gl, nb_rng%koff, ione, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*(nx_all+ny_all)*(5*nz_all-2)                 &
     &            + (ndepth-1)*(nx_all+ny_all)*nz_all                   &
     &            + ndepth*ndepth*(3*nz_all-1)                          &
     &            + (ndepth-1)*ndepth*nz_all
          call set_sleeve_edge_xmax_ymin(c_size1, sl_rng1, kpe, knp,    &
     &        ioff_gl, nb_rng%koff, itwo, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*(nx_all+ny_all)*(5*nz_all-2)                 &
     &            + (ndepth-1)*(nx_all+ny_all)*nz_all                   &
     &            + ndepth*ndepth*(4*nz_all-1)                          &
     &            + (ndepth-1)*ndepth*nz_all
          call set_sleeve_edge_xmax_ymin(c_size1, sl_rng1, kpe, knp,    &
     &        ioff_gl, nb_rng%koff, ithree, inod)

        enddo
       end if
!
!  outdside (x>xmax, y>ymax)
!
       if ( ipe .eq. ndx  .and. jpe .eq. ndy ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
!                                       .. start side
         call set_sleeve_size                                           &
     &      (nb_rng, ndepth, inp, jnp, knp, sl_rng1)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*(nx_all+ny_all)*(5*nz_all-2)                 &
     &            + (ndepth-1)*(nx_all+ny_all)*nz_all                   &
     &            + ndepth*ndepth*(5*nz_all-2)                          &
     &            + (ndepth-1)*ndepth*nz_all
          call set_sleeve_edge_xmax_ymax(c_size1, sl_rng1, kpe, knp,    &
     &        ioff_gl, nb_rng%koff, ione, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*(nx_all+ny_all)*(5*nz_all-2)                 &
     &            + (ndepth-1)*(nx_all+ny_all)*nz_all                   &
     &            + ndepth*ndepth*(5*nz_all-2)                          &
     &            + 2*(ndepth-1)*ndepth*nz_all
          call set_sleeve_edge_xmax_ymax(c_size1, sl_rng1, kpe, knp,    &
     &        ioff_gl, nb_rng%koff, itwo, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*(nx_all+ny_all)*(5*nz_all-2)                 &
     &            + (ndepth-1)*(nx_all+ny_all)*nz_all                   &
     &            + ndepth*ndepth*(5*nz_all-2)                          &
     &            + 3*(ndepth-1)*ndepth*nz_all
          call set_sleeve_edge_xmax_ymax(c_size1, sl_rng1, kpe, knp,    &
     &        ioff_gl, nb_rng%koff, ithree, inod)

       enddo
      end if
!
!  outdside (x<xmin, y>ymax)
!
      if ( ipe .eq. 1  .and. jpe .eq. ndy ) then
       do knp = nb_rng%knp_st, nb_rng%knp_end

          call set_sleeve_size                                          &
     &       (nb_rng, ndepth, inp, jnp, knp, sl_rng1)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*(nx_all+ny_all)*(5*nz_all-2)                 &
     &            + (ndepth-1)*(nx_all+ny_all)*nz_all                   &
     &            + ndepth*ndepth*(6*nz_all-3)                          &
     &            + 3*(ndepth-1)*ndepth*nz_all
          call set_sleeve_edge_xmin_ymax(c_size1, sl_rng1, kpe, knp,    &
     &        ioff_gl, nb_rng%koff, ione, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*(nx_all+ny_all)*(5*nz_all-2)                 &
     &            + (ndepth-1)*(nx_all+ny_all)*nz_all                   &
     &            + ndepth*ndepth*(7*nz_all-3)                          &
     &            + 3*(ndepth-1)*ndepth*nz_all
          call set_sleeve_edge_xmin_ymax(c_size1, sl_rng1, kpe, knp,    &
     &        ioff_gl, nb_rng%koff, itwo, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all            &
     &            + ndepth*(nx_all+ny_all)*(5*nz_all-2)                 &
     &            + (ndepth-1)*(nx_all+ny_all)*nz_all                   &
     &            + ndepth*ndepth*(7*nz_all-3)                          &
     &            + 4*(ndepth-1)*ndepth*nz_all
          call set_sleeve_edge_xmin_ymax(c_size1, sl_rng1, kpe, knp,    &
     &        ioff_gl, nb_rng%koff, ithree, inod)

                enddo
              end if
!
!          ioff_gl = nx_all*ny_all*nz_all                               &
!     &            + nx_all*ny_all*(3*nz_all-1)                         &
!     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all           &
!     &            + ndepth*(nx_all+ny_all)*(5*nz_all-2)                &
!     &            + (ndepth-1)*(nx_all+ny_all)*nz_all                  &
!     &            + ndepth*ndepth*(8*nz_all-4)                         &
!     &            + 4*(ndepth-1)*ndepth*nz_all
!
!          ioff_gl = nx_all*ny_all*nz_all                               &
!     &            + nx_all*ny_all*(3*nz_all-1)                         &
!     &            + 2*(nx_all+ny_all+2*ndepth)*ndepth*nz_all           &
!     &            + ndepth*(nx_all+ny_all)*(5*nz_all-2)                &
!     &            + (ndepth-1)*(nx_all+ny_all+4*ndepth)*nz_all         &
!     &            + ndepth*ndepth*(8*nz_all-4)                         &
!
!              do k = 1, nz
!               do j = 1, ny
!                do i = 1, nx
!                 write(*,'(7i7)') ipe, jpe, kpe, i, j, k, node_id_lc(i,j,k)
!                end do
!               end do
!              end do
!
!              do k = 1, nz
!               do j = 1, ny
!                do i = 1, nx
!                 write(*,'(9i7)') ipe, jpe, kpe, i, j, k, edge_id_lc(i,j,k,1:3)
!                end do
!               end do
!              end do
!
!
      end subroutine set_sleeve_edge_peri
!
! ----------------------------------------------------------------------
!
      end module set_sleeve_edge_peri_cube
