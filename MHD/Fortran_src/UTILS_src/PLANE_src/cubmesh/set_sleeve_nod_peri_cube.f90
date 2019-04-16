!set_sleeve_nod_peri_cube.f90
!     module set_sleeve_nod_peri_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_sleeve_node_peri(c_size, nb_rng, ipe, jpe, inod)
!!      subroutine set_sleeve_node_peri_quad                            &
!!     &         (c_size, nb_rng, ipe, jpe, inod)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module set_sleeve_nod_peri_cube
!
      use m_precision
!
      use t_size_of_cube
      use t_size_of_cube
      use t_neib_range_cube
      use t_sleeve_cube
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_sleeve_node_peri(c_size, nb_rng, ipe, jpe, inod)
!
      use sleeve_nod_side_cube
      use sleeve_nod_corner_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe
      integer (kind = kint), intent(inout) :: inod
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: ioff_gl
      integer (kind = kint) :: inp, jnp, knp
!
!  outdside (x<xmin)
!
      ioff_gl = c_size%nod_gltot
      if (ipe .eq. 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do jnp = nb_rng%jnp_st, nb_rng%jnp_end

            call set_sleeve_size                                        &
     &         (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%is = 1
            sl_rng1%ie = c_size%ndepth
!
            call set_sleeve_node_xmin                                   &
     &         (c_size, nb_rng, sl_rng1, ioff_gl, inod)
          enddo
        enddo
      endif

!
!  outdside (x>xmax)
!
      ioff_gl = c_size%nod_gltot                                        &
     &       + c_size%ndepth * c_size%ny_all * c_size%nz_all
      if (ipe .eq. c_size%ndx) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do jnp = nb_rng%jnp_st, nb_rng%jnp_end

            call set_sleeve_size                                        &
     &         (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%is = 1
            sl_rng1%ie = c_size%ndepth
            call set_sleeve_node_xmax                                   &
     &         (c_size, nb_rng, sl_rng1, ioff_gl, inod)
          enddo
        enddo
      endif

!
!  outdside (y<ymin)
!
      ioff_gl = c_size%nod_gltot                                        &
     &       + 2*c_size%ndepth * c_size%ny_all * c_size%nz_all
      if ( jpe .eq. 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do inp = nb_rng%inp_st, nb_rng%inp_end

            call set_sleeve_size                                        &
     &         (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%js = 1
            sl_rng1%je = c_size%ndepth
            call set_sleeve_node_ymin                                   &
     &         (c_size, nb_rng, sl_rng1, ioff_gl, inod)
          enddo
        enddo
      endif
!
!  outdside (y<ymax)
!
      ioff_gl = c_size%nod_gltot                                        &
     &         + 2*c_size%ndepth * c_size%ny_all * c_size%nz_all        &
     &         + c_size%ndepth * c_size%nx_all * c_size%nz_all 
      if(jpe .eq. c_size%ndy) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do inp = nb_rng%inp_st, nb_rng%inp_end

            call set_sleeve_size                                        &
     &         (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%js = 1
            sl_rng1%je = c_size%ndepth
            call set_sleeve_node_ymax                                   &
     &         (c_size, nb_rng, sl_rng1, ioff_gl, inod)
          enddo
        enddo
      endif
!
!  outdside (x<xmin, y<ymin)
!
      ioff_gl = c_size%nod_gltot                                        &
     &         + 2*c_size%ndepth * c_size%ny_all * c_size%nz_all        &
     &         + 2*c_size%ndepth * c_size%nx_all * c_size%nz_all
      if ( ipe .eq. 1  .and. jpe .eq. 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end

          call set_sleeve_size                                          &
     &       (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
          call set_sleeve_node_xmin_ymin                                &
     &       (c_size, sl_rng1, ioff_gl, nb_rng%koff, inod)
        enddo
      endif
!
!  outdside (x>xmax, y<ymin)
!
      ioff_gl = c_size%nod_gltot                                        &
     &         + 2*c_size%ndepth * c_size%ny_all * c_size%nz_all        &
     &         + 2*c_size%ndepth * c_size%nx_all * c_size%nz_all        &
     &         + c_size%ndepth * c_size%ndepth * c_size%nz_all
      if(ipe .eq. c_size%ndx  .and. jpe .eq. 1) then
        do knp = nb_rng%knp_st, nb_rng%knp_end

          call set_sleeve_size                                          &
     &       (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
          call set_sleeve_node_xmax_ymin                                &
     &       (c_size, sl_rng1, ioff_gl, nb_rng%koff, inod)

        enddo
      end if
!
!  outdside (x>xmax, y>ymax)
!
      ioff_gl = c_size%nod_gltot                                        &
     &         + 2*c_size%ndepth * c_size%ny_all * c_size%nz_all        &
     &         + 2*c_size%ndepth * c_size%nx_all * c_size%nz_all        &
     &         + 2*c_size%ndepth * c_size%ndepth * c_size%nz_all
      if ( ipe .eq. c_size%ndx  .and. jpe .eq. c_size%ndy) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
!                                       .. start side
          call set_sleeve_size                                          &
     &       (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
          call set_sleeve_node_xmax_ymax                                &
     &       (c_size, sl_rng1, ioff_gl, nb_rng%koff, inod)
        enddo
      end if
!
!  outdside (x<xmax, y<ymax)
!
      ioff_gl = c_size%nod_gltot                                        &
     &         + 2*c_size%ndepth * c_size%ny_all * c_size%nz_all        &
     &         + 2*c_size%ndepth * c_size%nx_all * c_size%nz_all        &
     &         + 3*c_size%ndepth * c_size%ndepth * c_size%nz_all
      if(ipe .eq. 1  .and. jpe .eq. c_size%ndy) then
        do knp = nb_rng%knp_st, nb_rng%knp_end

          call set_sleeve_size                                          &
     &       (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
          call set_sleeve_node_xmin_ymax                                &
     &       (c_size, sl_rng1, ioff_gl, nb_rng%koff, inod)
        enddo
      end if
!
      end subroutine set_sleeve_node_peri
!
! ----------------------------------------------------------------------
!
      subroutine set_sleeve_node_peri_quad                              &
     &         (c_size, nb_rng, ipe, jpe, inod)
!
      use sleeve_nod_side_cube
      use sleeve_nod_corner_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe
      integer (kind = kint), intent(inout) :: inod
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: ioff_gl
      integer (kind = kint) :: inp, jnp, knp
!
! ***** set and write for sleeve area nodes for periodical boundary
!  outdside (x<xmin)
!
      ioff_gl = c_size%nod_gltot + c_size%edge_gltot
      if (ipe .eq. 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do jnp = nb_rng%jnp_st, nb_rng%jnp_end
!
            call set_sleeve_size                                        &
     &         (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%is = 1
            sl_rng1%ie = c_size%ndepth
!
            call set_sleeve_node_xmin                                   &
     &         (c_size, nb_rng, sl_rng1, ioff_gl, inod)
          enddo
        enddo
      endif
!
!  outdside (x>xmax)
!
      ioff_gl = c_size%nod_gltot + c_size%edge_gltot                    &
     &         + c_size%ndepth * c_size%ny_all * c_size%nz_all
      if (ipe .eq. c_size%ndx) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do jnp = nb_rng%jnp_st, nb_rng%jnp_end

            call set_sleeve_size                                        &
     &         (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%is = 1
            sl_rng1%ie = c_size%ndepth
            call set_sleeve_node_xmax                                   &
     &         (c_size, nb_rng, sl_rng1, ioff_gl, inod)
          enddo
        enddo
      endif
!
!  outdside (y<ymin)
!
      ioff_gl = c_size%nod_gltot + c_size%edge_gltot                    &
     &         + 2*c_size%ndepth * c_size%ny_all * c_size%nz_all
      if ( jpe .eq. 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do inp = nb_rng%inp_st, nb_rng%inp_end

            call set_sleeve_size                                        &
     &         (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%js = 1
            sl_rng1%je = c_size%ndepth
            call set_sleeve_node_ymin                                   &
     &         (c_size, nb_rng, sl_rng1, ioff_gl, inod)
          enddo
        enddo
      endif
!
!  outdside (y<ymax)
!
      ioff_gl = c_size%nod_gltot + c_size%edge_gltot                    &
     &         + 2*c_size%ndepth * c_size%ny_all * c_size%nz_all        &
     &         + c_size%ndepth * c_size%nx_all * c_size%nz_all 
      if(jpe .eq. c_size%ndy) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do inp = nb_rng%inp_st, nb_rng%inp_end

            call set_sleeve_size                                        &
     &         (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
            sl_rng1%js = 1
            sl_rng1%je = c_size%ndepth
            call set_sleeve_node_ymax                                   &
     &         (c_size, nb_rng, sl_rng1, ioff_gl, inod)
          enddo
        enddo
      endif
!
!  outdside (x<xmin, y<ymin)
!
      ioff_gl = c_size%nod_gltot + c_size%edge_gltot                    &
     &         + 2*c_size%ndepth * c_size%ny_all * c_size%nz_all        &
     &         + 2*c_size%ndepth * c_size%nx_all * c_size%nz_all
      if(ipe .eq. 1  .and. jpe .eq. 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end

          call set_sleeve_size                                          &
     &       (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
          call set_sleeve_node_xmin_ymin                                &
     &       (c_size, sl_rng1, ioff_gl, nb_rng%koff, inod)

        enddo
      endif
!
!  outdside (x>xmax, y<ymin)
!
      ioff_gl = c_size%nod_gltot + c_size%edge_gltot                    &
     &         + 2*c_size%ndepth * c_size%ny_all * c_size%nz_all        &
     &         + 2*c_size%ndepth * c_size%nx_all * c_size%nz_all        &
     &         + c_size%ndepth * c_size%ndepth * c_size%nz_all
      if(ipe .eq. c_size%ndx  .and. jpe .eq. 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end

          call set_sleeve_size                                          &
     &       (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
          call set_sleeve_node_xmax_ymin                                &
     &       (c_size, sl_rng1, ioff_gl, nb_rng%koff, inod)

        enddo
      end if
!
!  outdside (x>xmax, y>ymax)
!
      ioff_gl = c_size%nod_gltot + c_size%edge_gltot                    &
     &         + 2*c_size%ndepth * c_size%ny_all * c_size%nz_all        &
     &         + 2*c_size%ndepth * c_size%nx_all * c_size%nz_all        &
     &         + 2*c_size%ndepth * c_size%ndepth * c_size%nz_all
      if(ipe .eq. c_size%ndx  .and. jpe .eq. c_size%ndy) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
!                                       .. start side

          call set_sleeve_size                                          &
     &       (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
          call set_sleeve_node_xmax_ymax                                &
     &       (c_size, sl_rng1, ioff_gl, nb_rng%koff, inod)
        enddo
      end if
!
!  outdside (x<xmax, y<ymax)
!
      ioff_gl = c_size%nod_gltot + c_size%edge_gltot                    &
     &         + 2*c_size%ndepth * c_size%ny_all * c_size%nz_all        &
     &         + 2*c_size%ndepth * c_size%nx_all * c_size%nz_all        &
     &         + 3*c_size%ndepth * c_size%ndepth * c_size%nz_all
       if(ipe .eq. 1  .and. jpe .eq. c_size%ndy) then
         do knp = nb_rng%knp_st, nb_rng%knp_end

          call set_sleeve_size                                          &
     &       (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
          call set_sleeve_node_xmin_ymax                                &
     &       (c_size, sl_rng1, ioff_gl, nb_rng%koff, inod)

        enddo
      end if
!
      end subroutine set_sleeve_node_peri_quad
!
! ----------------------------------------------------------------------
!
      end module set_sleeve_nod_peri_cube
