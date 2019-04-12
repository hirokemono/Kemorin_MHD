!set_export_peri_cube.f90
!     module set_export_peri_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_export_peri(nb_rng, ipe, jpe, icou, inod)
!!      subroutine set_export_peri_quad                                 &
!!     &         (nb_rng, ipe, jpe, kpe, icou, inod)
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module set_export_peri_cube
!
      use m_precision
      use m_constants
!
      use t_neib_range_cube
      use t_sleeve_cube
      use m_size_4_plane
      use m_size_of_cube
      use m_comm_data_cube_kemo
      use set_comm_nod_4_cube
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_export_peri(nb_rng, ipe, jpe, icou, inod)
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe
      integer (kind = kint), intent(inout) :: icou, inod
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inp, jnp, knp
!
!
            if (ipe .eq. ndx) then
             inp = 1
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do jnp = nb_rng%jnp_st, nb_rng%jnp_end

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = nxi+1
               sl_rng1%ie = nxi+ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, inod)

              enddo
             enddo
            endif

!    ---   outside wall (x<xmin)
!!
            if (ipe .eq. 1) then
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do jnp = nb_rng%jnp_st, nb_rng%jnp_end

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = ndepth+1
               sl_rng1%ie = 2*ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, inod)

              enddo
             enddo
            endif
!
!
!  outdside (y<ymax)
!
            if ( jpe .eq. ndy ) then
             jnp = 1
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%js = nyi+1
               sl_rng1%je = nyi+ndepth

                  icou = icou  + 1
               call set_ex_node(sl_rng1, inod)

              enddo
             enddo
            endif

!
!  outdside (y<ymin)
!
            if ( jpe .eq. 1 ) then
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%js = ndepth+1
               sl_rng1%je = 2*ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, inod)

              enddo
             enddo
            endif

!
!  outdside (x>xmax, y>ymax)
!
            if ( ipe .eq. ndx  .and. jpe .eq. ndy ) then
             inp = 1
             jnp = 1
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = nxi+1
               sl_rng1%ie = nxi+ndepth
               sl_rng1%js = nyi+1
               sl_rng1%je = nyi+ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, inod)

              enddo
            endif

!
!  outdside (x>xmin, y<ymax)
!
            if ( ipe .eq. 1  .and. jpe .eq. ndy ) then
             jnp = 1
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = ndepth+1
               sl_rng1%ie = 2*ndepth
               sl_rng1%js = nyi+1
               sl_rng1%je = nyi+ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, inod)

              enddo
            endif
!
!  outdside (x<xmin, y<ymin)
!
            if ( ipe .eq. 1  .and. jpe .eq. 1 ) then
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = ndepth+1
               sl_rng1%ie = 2*ndepth
               sl_rng1%js = ndepth+1
               sl_rng1%je = 2*ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, inod)

              enddo
            endif


!  outdside (x>xmax, y<ymin)
!
            if ( ipe .eq. ndx  .and. jpe .eq. 1 ) then
             inp = 1
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = nxi+1
               sl_rng1%ie = nxi+ndepth
               sl_rng1%js = ndepth+1
               sl_rng1%je = 2*ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, inod)

              enddo
            endif
!
      end subroutine set_export_peri
!
! ----------------------------------------------------------------------
!
      subroutine set_export_peri_quad                                   &
     &         (nb_rng, ipe, jpe, kpe, icou, inod)
!
      use set_comm_edge_4_cube
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe, kpe
      integer (kind = kint), intent(inout) :: icou, inod
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inp, jnp, knp
!
!  outdside (x>xmax)
!
            if (ipe .eq. ndx) then
             inp = 1
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do jnp = nb_rng%jnp_st, nb_rng%jnp_end
!
               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = nxi+1
               sl_rng1%ie = nxi+ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, inod)

               call set_ex_edge                                         &
     &            (sl_rng1, kpe, inp, jnp, knp, inod, ione)
               call set_ex_edge                                         &
     &            (sl_rng1, kpe, inp, jnp, knp, inod, itwo)
               call set_ex_edge                                         &
     &            (sl_rng1, kpe, inp, jnp, knp, inod, ithree)
               write(*,*) 'export 1 from',                              &
     &             (neibpe(icou)-1), inp, jnp, knp, inod

              enddo
             enddo
            endif

!    ---   outside wall (x<xmin)
!!
            if (ipe .eq. 1) then
              do knp = nb_rng%knp_st, nb_rng%knp_end
                do jnp = nb_rng%jnp_st, nb_rng%jnp_end

                 call set_boundary_size                                 &
     &              (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
                 sl_rng1%is = ndepth+1

                 icou = icou  + 1
                 sl_rng1%ie = 2*ndepth
                 call set_ex_node(sl_rng1, inod)

                 sl_rng1%ie = 2*ndepth - 1
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, ione)

                 sl_rng1%ie = 2*ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, itwo)

                 sl_rng1%ie = 2*ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, ithree)
                 write(*,*) 'export 2 from',                            &
     &               (neibpe(icou)-1), inp, jnp, knp, inod

                enddo
              enddo
            endif
!
!
!  outdside (y<ymax)
!
            if ( jpe .eq. ndy ) then
             jnp = 1
              do knp = nb_rng%knp_st, nb_rng%knp_end
                do inp = nb_rng%inp_st, nb_rng%inp_end

                 call set_boundary_size                                 &
     &              (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
                 sl_rng1%js = nyi+1
                 sl_rng1%je = nyi+ndepth

                 icou = icou  + 1
                 call set_ex_node(sl_rng1, inod)

                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, ione)
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, itwo)
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, ithree)
                 write(*,*) 'export 3 from',                            &
     &               (neibpe(icou)-1), inp, jnp, knp, inod

                enddo
              enddo
            endif

!
!  outdside (y<ymin)
!
            if ( jpe .eq. 1 ) then
              do knp = nb_rng%knp_st, nb_rng%knp_end
                do inp = nb_rng%inp_st, nb_rng%inp_end

                 call set_boundary_size                                 &
     &              (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
                 sl_rng1%js = ndepth+1

                 icou = icou  + 1
                 sl_rng1%je = 2*ndepth
                 call set_ex_node(sl_rng1, inod)

                 sl_rng1%je = 2*ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, ione)

                 sl_rng1%je = 2*ndepth - 1
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, itwo)

                 sl_rng1%je = 2*ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, ithree)
                 write(*,*) 'export 4 from',                            &
     &               (neibpe(icou)-1), inp, jnp, knp, inod

                enddo
              enddo
            endif

!
!  outdside (x>xmax, y>ymax)
!
            if ( ipe .eq. ndx  .and. jpe .eq. ndy ) then
             inp = 1
             jnp = 1
              do knp = nb_rng%knp_st, nb_rng%knp_end

                 call set_boundary_size                                 &
     &              (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
                 sl_rng1%is = nxi+1
                 sl_rng1%ie = nxi+ndepth
                 sl_rng1%js = nyi+1
                 sl_rng1%je = nyi+ndepth

                 icou = icou  + 1
                 call set_ex_node(sl_rng1, inod)

                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, ione)
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, itwo)
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, ithree)
                 write(*,*) 'export 5 from',                            &
     &                (neibpe(icou)-1), inp, jnp, knp, inod

              enddo
            endif

!
!  outdside (x>xmin, y<ymax)
!
            if ( ipe .eq. 1  .and. jpe .eq. ndy ) then
             jnp = 1
              do knp = nb_rng%knp_st, nb_rng%knp_end

                 call set_boundary_size                                 &
     &              (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
                 sl_rng1%js = nyi+1
                 sl_rng1%je = nyi+ndepth
                 sl_rng1%is = ndepth+1

                 icou = icou  + 1
                 sl_rng1%ie = 2*ndepth
                 call set_ex_node(sl_rng1, inod)

                 sl_rng1%ie = 2*ndepth - 1
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, ione)

                 sl_rng1%ie = 2*ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, itwo)

                 sl_rng1%ie = 2*ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, ithree)
                 write(*,*) 'export 6 from',                            &
     &               (neibpe(icou)-1), inp, jnp, knp, inod

              enddo
            endif
!
!  outdside (x<xmin, y<ymin)
!
            if ( ipe .eq. 1  .and. jpe .eq. 1 ) then
              do knp = nb_rng%knp_st, nb_rng%knp_end

                 call set_boundary_size                                 &
     &              (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
                 sl_rng1%is = ndepth+1
                 sl_rng1%js = ndepth+1

                 icou = icou  + 1
                 sl_rng1%ie = 2*ndepth
                 sl_rng1%je = 2*ndepth
                 call set_ex_node(sl_rng1, inod)

                 sl_rng1%ie = 2*ndepth - 1
                 sl_rng1%je = 2*ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, ione)

                 sl_rng1%ie = 2*ndepth
                 sl_rng1%je = 2*ndepth - 1
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, itwo)

                 sl_rng1%ie = 2*ndepth
                 sl_rng1%je = 2*ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, ithree)
                 write(*,*) 'export 7 from',                            &
     &               (neibpe(icou)-1), inp, jnp, knp, inod

              enddo
            endif


!  outdside (x>xmax, y<ymin)
!
            if ( ipe .eq. ndx  .and. jpe .eq. 1 ) then
             inp = 1
              do knp = nb_rng%knp_st, nb_rng%knp_end

                 call set_boundary_size                                 &
     &              (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
                 sl_rng1%is = nxi+1
                 sl_rng1%ie = nxi+ndepth
                 sl_rng1%js = ndepth+1

                 icou = icou  + 1
                 sl_rng1%je = 2*ndepth
                 call set_ex_node(sl_rng1, inod)

                 sl_rng1%je = 2*ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, ione)

                 sl_rng1%je = 2*ndepth - 1
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, itwo)

                 sl_rng1%je = 2*ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, kpe, inp, jnp, knp, inod, ithree)
                 write(*,*) 'export 8 to',                              &
     &              (neibpe(icou)-1), inp, jnp, knp, inod

              enddo
            endif
!
      end subroutine set_export_peri_quad
!
! ----------------------------------------------------------------------
!
      end module set_export_peri_cube
