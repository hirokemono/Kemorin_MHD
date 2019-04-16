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
      integer (kind = kint) :: nwall_z, nwall_x, nwall_xy, nwall_x2
      integer (kind = kint) :: nwall_x4, nwall_x5, nwall_y, nwall_y3
      integer (kind = kint) :: nwall_y4, nwall_y2, nwall_y5, nwall_c1
      integer (kind = kint) :: nwall_cn, nwall_c3, nwall_c4
      integer (kind = kint) :: nwall_xz, nwall_z5, nwall_z6, nwall_z7
 !
      integer (kind = kint) :: ncube_in, ncube_3, nwall_z8, nwall_c8
!
!
      nwall_z = (c_size%nx_all + c_size%ny_all + 2*c_size%ndepth)       &
     &         * c_size%ndepth * c_size%nz_all
      nwall_x =  c_size%ndepth * c_size%ny_all * c_size%nz_all
      nwall_xy = c_size%ndepth * c_size%ny_all * (3*c_size%nz_all - 1)
      nwall_x2 = (c_size%ndepth-1) * c_size%ny_all * c_size%nz_all
      nwall_x4 = c_size%ndepth * c_size%ny_all * (4*c_size%nz_all - 1)
      nwall_x5 = c_size%ndepth * c_size%ny_all * (5*c_size%nz_all - 2)
      nwall_y = c_size%ndepth * c_size%nx_all * c_size%nz_all
      nwall_y3 = c_size%ndepth * c_size%nx_all * (3*c_size%nz_all - 1)
      nwall_y4 = c_size%ndepth * c_size%nx_all * (4*c_size%nz_all - 1)
      nwall_y2 = (c_size%ndepth-1) * c_size%nx_all * c_size%nz_all
      nwall_y5 = c_size%ndepth * (c_size%nx_all + c_size%ny_all)        &
     &          * (5*c_size%nz_all - 2)
      nwall_c1 = (c_size%ndepth-1) * (c_size%nx_all + c_size%ny_all)    &
     &          * c_size%nz_all
      nwall_cn = c_size%ndepth * c_size%ndepth * c_size%nz_all
      nwall_c3 = c_size%ndepth * c_size%ndepth * (3*c_size%nz_all - 1)
      nwall_c4 = c_size%ndepth * c_size%ndepth * (4*c_size%nz_all - 1)
      nwall_xz = (c_size%ndepth-1) * c_size%ndepth * c_size%nz_all
      nwall_z5 = c_size%ndepth * c_size%ndepth * (5*c_size%nz_all - 2)
      nwall_z6 = c_size%ndepth * c_size%ndepth * (6*c_size%nz_all - 3)
      nwall_z7 = c_size%ndepth * c_size%ndepth * (7*c_size%nz_all - 3)
 !
      ncube_in = c_size%nx_all * c_size%ny_all * c_size%nz_all
      ncube_3 = c_size%nx_all * c_size%ny_all * (3*c_size%nz_all - 1)
      nwall_z8 = c_size%ndepth * c_size%ndepth * (8*c_size%nz_all - 4)
      nwall_c8 = (c_size%ndepth-1)                                      &
     &          * (c_size%nx_all + c_size%ny_all + 4*c_size%ndepth)     &
     &          * c_size%nz_all
!
!  outdside (x<xmin)
!
      if (ipe .eq. 1 ) then
       inp = -1
       do knp = nb_rng%knp_st, nb_rng%knp_end
        do jnp = nb_rng%jnp_st, nb_rng%jnp_end

         call set_sleeve_size                                           &
     &      (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
         sl_rng1%is = 1
         sl_rng1%ie = c_size%ndepth
!
          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z
          call set_sleeve_edge_xmin(c_size, nb_rng, sl_rng1,            &
     &        kpe, jnp, knp, ioff_gl, ione, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &             + 2 * nwall_z + nwall_x
          call set_sleeve_edge_xmin(c_size, nb_rng, sl_rng1,            &
     &        kpe, jnp, knp, ioff_gl, itwo, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &             + 2 * nwall_z + 2 * nwall_x
          call set_sleeve_edge_xmin(c_size, nb_rng, sl_rng1,            &
     &        kpe, jnp, knp, ioff_gl, ithree, inod)
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
     &      (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
         sl_rng1%is = 1
         sl_rng1%ie = c_size%ndepth

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_xy
          call set_sleeve_edge_xmax(c_size, nb_rng, sl_rng1,            &
     &        kpe, jnp, knp, ioff_gl, ione, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_xy + nwall_x2
          call set_sleeve_edge_xmax(c_size, nb_rng, sl_rng1,            &
     &        kpe, jnp, knp, ioff_gl, itwo, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_x4 + nwall_x2
          call set_sleeve_edge_xmax(c_size, nb_rng, sl_rng1,            &
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
     &      (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
          sl_rng1%js = 1
          sl_rng1%je = c_size%ndepth

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_x5 + nwall_x2
          call set_sleeve_edge_ymin(c_size, nb_rng, sl_rng1,            &
     &        kpe, inp, knp, ioff_gl, ione, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_x5 + nwall_x2 + nwall_y
          call set_sleeve_edge_ymin(c_size, nb_rng, sl_rng1,            &
     &        kpe, inp, knp, ioff_gl, itwo, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_x5 + nwall_x2 + 2 * nwall_y
          call set_sleeve_edge_ymin(c_size, nb_rng, sl_rng1,            &
     &        kpe, inp, knp, ioff_gl, ithree, inod)

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
     &      (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
         sl_rng1%js = 1
         sl_rng1%je = c_size%ndepth

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_x5 + nwall_x2 + nwall_y3
          call set_sleeve_edge_ymax(c_size, nb_rng, sl_rng1,            &
     &        kpe, inp, knp, ioff_gl, ione, inod)
!
          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_x5 + nwall_x2 + nwall_y4
          call set_sleeve_edge_ymax(c_size, nb_rng, sl_rng1,            &
     &        kpe, inp, knp, ioff_gl, itwo, inod)
!
          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_x5 + nwall_x2 + nwall_y4        &
     &            + nwall_y2
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
     &      (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
         sl_rng1%is = 1
         sl_rng1%ie = c_size%ndepth
         sl_rng1%js = 1
         sl_rng1%je = c_size%ndepth

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_y5 + nwall_c1
          call set_sleeve_edge_xmin_ymin(c_size, sl_rng1, kpe, knp,     &
     &        ioff_gl, nb_rng%koff, ione, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_y5 + nwall_c1 + nwall_cn
          call set_sleeve_edge_xmin_ymin(c_size, sl_rng1, kpe, knp,     &
     &        ioff_gl, nb_rng%koff, itwo, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_y5 + nwall_c1 + 2 * nwall_cn
          call set_sleeve_edge_xmin_ymin(c_size, sl_rng1, kpe, knp,     &
     &        ioff_gl, nb_rng%koff, ithree, inod)

       enddo
      endif
!
!  outdside (x>xmax, y<ymin)
!
      if ( ipe .eq. ndx  .and. jpe .eq. 1 ) then
       do knp = nb_rng%knp_st, nb_rng%knp_end

         call set_sleeve_size                                           &
     &      (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_y5 + nwall_c1 + nwall_c3
          call set_sleeve_edge_xmax_ymin(c_size, sl_rng1, kpe, knp,     &
     &        ioff_gl, nb_rng%koff, ione, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_y5 + nwall_c1 + nwall_c3        &
     &            + nwall_xz
          call set_sleeve_edge_xmax_ymin(c_size, sl_rng1, kpe, knp,     &
     &        ioff_gl, nb_rng%koff, itwo, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_y5 + nwall_c1 + nwall_c4        &
     &            + nwall_xz
          call set_sleeve_edge_xmax_ymin(c_size, sl_rng1, kpe, knp,     &
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
     &      (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_y5 + nwall_c1 + nwall_z5        &
     &            + nwall_xz
          call set_sleeve_edge_xmax_ymax(c_size, sl_rng1, kpe, knp,     &
     &        ioff_gl, nb_rng%koff, ione, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_y5 + nwall_c1 + nwall_z5        &
     &            + 2*nwall_xz
          call set_sleeve_edge_xmax_ymax(c_size, sl_rng1, kpe, knp,     &
     &        ioff_gl, nb_rng%koff, itwo, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_y5 + nwall_c1 + nwall_z5        &
     &            + 3*nwall_xz
          call set_sleeve_edge_xmax_ymax(c_size, sl_rng1, kpe, knp,     &
     &        ioff_gl, nb_rng%koff, ithree, inod)

       enddo
      end if
!
!  outdside (x<xmin, y>ymax)
!
      if ( ipe .eq. 1  .and. jpe .eq. ndy ) then
       do knp = nb_rng%knp_st, nb_rng%knp_end

          call set_sleeve_size                                          &
     &       (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_y5 + nwall_c1 + nwall_z6        &
     &            + 3*nwall_xz
          call set_sleeve_edge_xmin_ymax(c_size, sl_rng1, kpe, knp,     &
     &        ioff_gl, nb_rng%koff, ione, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_y5 + nwall_c1 + nwall_z7        &
     &            + 3*nwall_xz
          call set_sleeve_edge_xmin_ymax(c_size, sl_rng1, kpe, knp,     &
     &        ioff_gl, nb_rng%koff, itwo, inod)

          ioff_gl = c_size%nod_gltot + c_size%edge_gltot                &
     &            + 2 * nwall_z + nwall_y5 + nwall_c1 + nwall_z7        &
     &            + 4*nwall_xz
          call set_sleeve_edge_xmin_ymax(c_size, sl_rng1, kpe, knp,     &
     &        ioff_gl, nb_rng%koff, ithree, inod)

                enddo
              end if
!
!          ioff_gl = ncube_in + ncube_3                                 &
!     &            + 2 * nwall_z + nwall_y5 + nwall_c1 + nwall_z8       &
!     &            + 4*nwall_xz
!
!          ioff_gl = ncube_in + ncube_3                                 &
!     &            + 2 * nwall_z + nwall_y5 + nwall_c8 + nwall_z8
!
!              do k = 1, c_each%nz
!               do j = 1, c_each%ny
!                do i = 1, c_each%nx
!                 write(*,'(7i7)') ipe, jpe, kpe, i, j, k, node_id_lc(i,j,k)
!                end do
!               end do
!              end do
!
!              do k = 1, c_each%nz
!               do j = 1, c_each%ny
!                do i = 1, c_each%nx
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
