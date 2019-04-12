!set_sleeve_nod_peri_cube.f90
!     module set_sleeve_nod_peri_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_sleeve_node_peri(nb_rng, ipe, jpe, inod)
!!      subroutine set_sleeve_node_peri_quad(nb_rng, ipe, jpe, inod)
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module set_sleeve_nod_peri_cube
!
      use m_precision
!
      use t_neib_range_cube
      use m_size_4_plane
      use m_size_of_cube
      use m_sleeve_cube
      use m_sleeve_nod_side_cube
      use m_sleeve_nod_corner_cube
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_sleeve_node_peri(nb_rng, ipe, jpe, inod)
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind = kint) :: ioff_gl
      integer (kind = kint) :: inp, jnp, knp
!
!  outdside (x<xmin)
!
      ioff_gl = nod_gltot
      if (ipe .eq. 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do jnp = nb_rng%jnp_st, nb_rng%jnp_end

            call set_sleeve_size                                        &
     &         (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%is = 1
            sl_rng1%ie = ndepth
!
            call set_sleeve_node_xmin(inod, ioff_gl)
          enddo
        enddo
      endif

!
!  outdside (x>xmax)
!
      ioff_gl = nod_gltot + ndepth*ny_all*nz_all
      if (ipe .eq. ndx ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do jnp = nb_rng%jnp_st, nb_rng%jnp_end

            call set_sleeve_size                                        &
     &         (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%is = 1
            sl_rng1%ie = ndepth

            call set_sleeve_node_xmax(inod, ioff_gl)
!
          enddo
        enddo
      endif

!
!  outdside (y<ymin)
!
      ioff_gl = nod_gltot + 2*ndepth*ny_all*nz_all
      if ( jpe .eq. 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do inp = nb_rng%inp_st, nb_rng%inp_end

            call set_sleeve_size                                        &
     &         (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%js = 1
            sl_rng1%je = ndepth

            call set_sleeve_node_ymin(inod, ioff_gl)

          enddo
        enddo
      endif
!
!  outdside (y<ymax)
!
      ioff_gl = nod_gltot                                               &
     &         + 2*ndepth*ny_all*nz_all + ndepth*nx_all*nz_all 
      if ( jpe .eq. ndy ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do inp = nb_rng%inp_st, nb_rng%inp_end

            call set_sleeve_size                                        &
     &         (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%js = 1
            sl_rng1%je = ndepth

            call set_sleeve_node_ymax(inod, ioff_gl)

          enddo
        enddo
      endif
!
!  outdside (x<xmin, y<ymin)
!
      ioff_gl = nod_gltot                                               &
     &         + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all 
      if ( ipe .eq. 1  .and. jpe .eq. 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end

          call set_sleeve_size                                          &
     &       (nb_rng, ndepth, inp, jnp, knp, sl_rng1)

          call set_sleeve_node_xmin_ymin(inod, ioff_gl)

        enddo
      endif
!
!  outdside (x>xmax, y<ymin)
!
      ioff_gl = nod_gltot                                               &
     &         + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all        &
     &         + ndepth*ndepth*nz_all
      if( ipe .eq. ndx  .and. jpe .eq. 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end

          call set_sleeve_size                                          &
     &       (nb_rng, ndepth, inp, jnp, knp, sl_rng1)

          call set_sleeve_node_xmax_ymin(inod, ioff_gl)

        enddo
      end if
!
!  outdside (x>xmax, y>ymax)
!
      ioff_gl = nod_gltot                                               &
     &         + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all        &
     &         + 2*ndepth*ndepth*nz_all
      if ( ipe .eq. ndx  .and. jpe .eq. ndy ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
!                                       .. start side

          call set_sleeve_size                                          &
     &       (nb_rng, ndepth, inp, jnp, knp, sl_rng1)

          call set_sleeve_node_xmax_ymax(inod, ioff_gl)
        enddo
      end if
!
!  outdside (x<xmax, y<ymax)
!
      ioff_gl = nod_gltot                                               &
     &         + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all        &
     &         + 3*ndepth*ndepth*nz_all
      if ( ipe .eq. 1  .and. jpe .eq. ndy ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end

          call set_sleeve_size                                          &
     &       (nb_rng, ndepth, inp, jnp, knp, sl_rng1)

          call set_sleeve_node_xmin_ymax(inod, ioff_gl)

         enddo
      end if
!
      end subroutine set_sleeve_node_peri
!
! ----------------------------------------------------------------------
!
      subroutine set_sleeve_node_peri_quad(nb_rng, ipe, jpe, inod)
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind = kint) :: ioff_gl
      integer (kind = kint) :: inp, jnp, knp
!
! ***** set and write for sleeve area nodes for periodical boundary
!  outdside (x<xmin)
!
      ioff_gl = nod_gltot + edge_gltot
      if (ipe .eq. 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do jnp = nb_rng%jnp_st, nb_rng%jnp_end
!
            call set_sleeve_size                                        &
     &         (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%is = 1
            sl_rng1%ie = ndepth
            call set_sleeve_node_xmin(inod, ioff_gl)

          enddo
        enddo
      endif
!
!  outdside (x>xmax)
!
      ioff_gl = nod_gltot + edge_gltot + ndepth*ny_all*nz_all
      if (ipe .eq. ndx ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do jnp = nb_rng%jnp_st, nb_rng%jnp_end

            call set_sleeve_size                                        &
     &         (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%is = 1
            sl_rng1%ie = ndepth
            call set_sleeve_node_xmax(inod, ioff_gl)

          enddo
        enddo
      endif
!
!  outdside (y<ymin)
!
      ioff_gl = nod_gltot + edge_gltot + 2*ndepth*ny_all*nz_all
      if ( jpe .eq. 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do inp = nb_rng%inp_st, nb_rng%inp_end

            call set_sleeve_size                                        &
     &         (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%js = 1
            sl_rng1%je = ndepth
!
            call set_sleeve_node_ymin(inod, ioff_gl)
          enddo
        enddo
      endif
!
!  outdside (y<ymax)
!
      ioff_gl = nod_gltot + edge_gltot                                  &
     &         + 2*ndepth*ny_all*nz_all + ndepth*nx_all*nz_all 
      if ( jpe .eq. ndy ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do inp = nb_rng%inp_st, nb_rng%inp_end

            call set_sleeve_size                                        &
     &         (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%js = 1
            sl_rng1%je = ndepth

            call set_sleeve_node_ymax(inod, ioff_gl)
          enddo
        enddo
      endif
!
!  outdside (x<xmin, y<ymin)
!
      ioff_gl = nod_gltot + edge_gltot                            &
     &         + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all 
      if ( ipe .eq. 1  .and. jpe .eq. 1 ) then
       do knp = nb_rng%knp_st, nb_rng%knp_end

      call set_sleeve_size                                        &
     &         (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
         call set_sleeve_node_xmin_ymin(inod, ioff_gl)

       enddo
      endif
!
!  outdside (x>xmax, y<ymin)
!
      ioff_gl = nod_gltot + edge_gltot                                  &
     &         + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all        &
     &         + ndepth*ndepth*nz_all
      if ( ipe .eq. ndx  .and. jpe .eq. 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end

          call set_sleeve_size                                          &
     &       (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
          call set_sleeve_node_xmax_ymin(inod, ioff_gl)

        enddo
      end if
!
!  outdside (x>xmax, y>ymax)
!
      ioff_gl = nod_gltot + edge_gltot                                  &
     &         + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all        &
     &         + 2*ndepth*ndepth*nz_all
      if ( ipe .eq. ndx  .and. jpe .eq. ndy ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
!                                       .. start side

          call set_sleeve_size                                          &
     &       (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
          call set_sleeve_node_xmax_ymax(inod, ioff_gl)
        enddo
      end if
!
!  outdside (x<xmax, y<ymax)
!
      ioff_gl = nod_gltot + edge_gltot                                  &
     &         + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all        &
     &         + 3*ndepth*ndepth*nz_all
       if ( ipe .eq. 1  .and. jpe .eq. ndy ) then
         do knp = nb_rng%knp_st, nb_rng%knp_end

          call set_sleeve_size                                          &
     &       (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
          call set_sleeve_node_xmin_ymax(inod, ioff_gl)

        enddo
      end if
!
      end subroutine set_sleeve_node_peri_quad
!
! ----------------------------------------------------------------------
!
      end module set_sleeve_nod_peri_cube
