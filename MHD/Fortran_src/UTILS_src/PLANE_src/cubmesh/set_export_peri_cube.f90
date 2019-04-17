!set_export_peri_cube.f90
!     module set_export_peri_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_export_peri                                      &
!!     &         (c_size, nb_rng, loc_id, ipe, jpe, icou, inod)
!!      subroutine set_export_peri_quad                                 &
!!     &         (c_size, nb_rng, loc_id, ipe, jpe, kpe, icou, inod)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type(local_node_id_cube), intent(in) :: loc_id
!
      module set_export_peri_cube
!
      use m_precision
      use m_constants
!
      use t_size_of_cube
      use t_neib_range_cube
      use t_sleeve_cube
      use t_local_node_id_cube
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
      subroutine set_export_peri                                        &
     &         (c_size, nb_rng, loc_id, ipe, jpe, icou, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      type(local_node_id_cube), intent(in) :: loc_id
      integer (kind = kint), intent(in) :: ipe, jpe
      integer (kind = kint), intent(inout) :: icou, inod
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inp, jnp, knp
!
!
            if(ipe .eq. c_size%ndx) then
             inp = 1
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do jnp = nb_rng%jnp_st, nb_rng%jnp_end

               call set_boundary_size                                   &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = c_size%nxi + 1
               sl_rng1%ie = c_size%nxi + c_size%ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, loc_id,                        &
     &             comm%ntot_export, comm%item_export, inod)

              enddo
             enddo
            endif

!    ---   outside wall (x<xmin)
!!
            if(ipe .eq. 1) then
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do jnp = nb_rng%jnp_st, nb_rng%jnp_end

               call set_boundary_size                                   &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = c_size%ndepth + 1
               sl_rng1%ie = 2*c_size%ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, loc_id,                        &
     &             comm%ntot_export, comm%item_export, inod)

              enddo
             enddo
            endif
!
!
!  outdside (y<ymax)
!
            if(jpe .eq. c_size%ndy) then
             jnp = 1
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               call set_boundary_size                                   &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%js = c_size%nyi + 1
               sl_rng1%je = c_size%nyi + c_size%ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, loc_id,                        &
     &             comm%ntot_export, comm%item_export, inod)

              enddo
             enddo
            endif

!
!  outdside (y<ymin)
!
            if(jpe .eq. 1) then
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               call set_boundary_size                                   &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%js = c_size%ndepth + 1
               sl_rng1%je = 2*c_size%ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, loc_id,                        &
     &             comm%ntot_export, comm%item_export, inod)

              enddo
             enddo
            endif

!
!  outdside (x>xmax, y>ymax)
!
            if(ipe .eq. c_size%ndx  .and. jpe .eq. c_size%ndy) then
             inp = 1
             jnp = 1
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_boundary_size                                   &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = c_size%nxi + 1
               sl_rng1%ie = c_size%nxi + c_size%ndepth
               sl_rng1%js = c_size%nyi + 1
               sl_rng1%je = c_size%nyi + c_size%ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, loc_id,                        &
     &             comm%ntot_export, comm%item_export, inod)

              enddo
            endif

!
!  outdside (x>xmin, y<ymax)
!
            if(ipe .eq. 1  .and. jpe .eq. c_size%ndy) then
             jnp = 1
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_boundary_size                                   &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = c_size%ndepth + 1
               sl_rng1%ie = 2*c_size%ndepth
               sl_rng1%js = c_size%nyi + 1
               sl_rng1%je = c_size%nyi + c_size%ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, loc_id,                        &
     &             comm%ntot_export, comm%item_export, inod)

              enddo
            endif
!
!  outdside (x<xmin, y<ymin)
!
            if(ipe .eq. 1  .and. jpe .eq. 1) then
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_boundary_size                                   &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = c_size%ndepth + 1
               sl_rng1%ie = 2*c_size%ndepth
               sl_rng1%js = c_size%ndepth + 1
               sl_rng1%je = 2*c_size%ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, loc_id,                        &
     &             comm%ntot_export, comm%item_export, inod)

              enddo
            endif


!  outdside (x>xmax, y<ymin)
!
            if(ipe .eq. c_size%ndx  .and. jpe .eq. 1) then
             inp = 1
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_boundary_size                                   &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = c_size%nxi + 1
               sl_rng1%ie = c_size%nxi + c_size%ndepth
               sl_rng1%js = c_size%ndepth + 1
               sl_rng1%je = 2*c_size%ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, loc_id,                        &
     &             comm%ntot_export, comm%item_export, inod)

              enddo
            endif
!
      end subroutine set_export_peri
!
! ----------------------------------------------------------------------
!
      subroutine set_export_peri_quad                                   &
     &         (c_size, nb_rng, loc_id, ipe, jpe, kpe, icou, inod)
!
      use set_comm_edge_4_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      type(local_node_id_cube), intent(in) :: loc_id
      integer (kind = kint), intent(in) :: ipe, jpe, kpe
      integer (kind = kint), intent(inout) :: icou, inod
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inp, jnp, knp
!
!  outdside (x>xmax)
!
            if(ipe .eq. c_size%ndx) then
             inp = 1
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do jnp = nb_rng%jnp_st, nb_rng%jnp_end
!
               call set_boundary_size                                   &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = c_size%nxi + 1
               sl_rng1%ie = c_size%nxi + c_size%ndepth

               icou = icou  + 1
               call set_ex_node(sl_rng1, loc_id,                        &
     &             comm%ntot_export, comm%item_export, inod)

               call set_ex_edge                                         &
     &            (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,     &
     &             ione, comm%ntot_export, comm%item_export, inod)
               call set_ex_edge                                         &
     &            (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,     &
     &             itwo, comm%ntot_export, comm%item_export, inod)
               call set_ex_edge                                         &
     &            (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,     &
     &             ithree, comm%ntot_export, comm%item_export, inod)
               write(*,*) 'export 1 from',                              &
     &             (comm%id_neib(icou)-1), inp, jnp, knp, inod

              enddo
             enddo
            endif

!    ---   outside wall (x<xmin)
!!
            if(ipe .eq. 1) then
              do knp = nb_rng%knp_st, nb_rng%knp_end
                do jnp = nb_rng%jnp_st, nb_rng%jnp_end

                 call set_boundary_size                                 &
     &              (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
                 sl_rng1%is = c_size%ndepth + 1

                 icou = icou  + 1
                 sl_rng1%ie = 2*c_size%ndepth
                 call set_ex_node(sl_rng1, loc_id,                      &
     &             comm%ntot_export, comm%item_export, inod)

                 sl_rng1%ie = 2*c_size%ndepth - 1
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               ione, comm%ntot_export, comm%item_export, inod)

                 sl_rng1%ie = 2*c_size%ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               itwo, comm%ntot_export, comm%item_export, inod)

                 sl_rng1%ie = 2*c_size%ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               ithree, comm%ntot_export, comm%item_export, inod)
                 write(*,*) 'export 2 from',                            &
     &               (comm%id_neib(icou)-1), inp, jnp, knp, inod

                enddo
              enddo
            endif
!
!
!  outdside (y<ymax)
!
            if(jpe .eq. c_size%ndy) then
             jnp = 1
              do knp = nb_rng%knp_st, nb_rng%knp_end
                do inp = nb_rng%inp_st, nb_rng%inp_end

                 call set_boundary_size                                 &
     &              (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
                 sl_rng1%js = c_size%nyi + 1
                 sl_rng1%je = c_size%nyi + c_size%ndepth

                 icou = icou  + 1
                 call set_ex_node(sl_rng1, loc_id,                      &
     &             comm%ntot_export, comm%item_export, inod)

                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               ione, comm%ntot_export, comm%item_export, inod)
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               itwo, comm%ntot_export, comm%item_export, inod)
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               ithree, comm%ntot_export, comm%item_export, inod)
                 write(*,*) 'export 3 from',                            &
     &               (comm%id_neib(icou)-1), inp, jnp, knp, inod

                enddo
              enddo
            endif

!
!  outdside (y<ymin)
!
            if(jpe .eq. 1) then
              do knp = nb_rng%knp_st, nb_rng%knp_end
                do inp = nb_rng%inp_st, nb_rng%inp_end

                 call set_boundary_size                                 &
     &              (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
                 sl_rng1%js = c_size%ndepth + 1

                 icou = icou  + 1
                 sl_rng1%je = 2*c_size%ndepth
                 call set_ex_node(sl_rng1, loc_id,                      &
     &             comm%ntot_export, comm%item_export, inod)

                 sl_rng1%je = 2*c_size%ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               ione, comm%ntot_export, comm%item_export, inod)

                 sl_rng1%je = 2*c_size%ndepth - 1
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               itwo, comm%ntot_export, comm%item_export, inod)

                 sl_rng1%je = 2*c_size%ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               ithree, comm%ntot_export, comm%item_export, inod)
                 write(*,*) 'export 4 from',                            &
     &               (comm%id_neib(icou)-1), inp, jnp, knp, inod

                enddo
              enddo
            endif

!
!  outdside (x>xmax, y>ymax)
!
            if(ipe .eq. c_size%ndx  .and. jpe .eq. c_size%ndy) then
             inp = 1
             jnp = 1
              do knp = nb_rng%knp_st, nb_rng%knp_end

                 call set_boundary_size                                 &
     &              (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
                 sl_rng1%is = c_size%nxi + 1
                 sl_rng1%ie = c_size%nxi + c_size%ndepth
                 sl_rng1%js = c_size%nyi + 1
                 sl_rng1%je = c_size%nyi + c_size%ndepth

                 icou = icou  + 1
                 call set_ex_node(sl_rng1, loc_id,                      &
     &             comm%ntot_export, comm%item_export, inod)

                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               ione, comm%ntot_export, comm%item_export, inod)
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               itwo, comm%ntot_export, comm%item_export, inod)
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               ithree, comm%ntot_export, comm%item_export, inod)
                 write(*,*) 'export 5 from',                            &
     &                (comm%id_neib(icou)-1), inp, jnp, knp, inod

              enddo
            endif

!
!  outdside (x>xmin, y<ymax)
!
            if(ipe .eq. 1  .and. jpe .eq. c_size%ndy) then
             jnp = 1
              do knp = nb_rng%knp_st, nb_rng%knp_end

                 call set_boundary_size                                 &
     &              (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
                 sl_rng1%js = c_size%nyi + 1
                 sl_rng1%je = c_size%nyi + c_size%ndepth
                 sl_rng1%is = c_size%ndepth + 1

                 icou = icou  + 1
                 sl_rng1%ie = 2*c_size%ndepth
                 call set_ex_node(sl_rng1, loc_id,                      &
     &             comm%ntot_export, comm%item_export, inod)

                 sl_rng1%ie = 2*c_size%ndepth - 1
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               ione, comm%ntot_export, comm%item_export, inod)

                 sl_rng1%ie = 2*c_size%ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               itwo, comm%ntot_export, comm%item_export, inod)

                 sl_rng1%ie = 2*c_size%ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               ithree, comm%ntot_export, comm%item_export, inod)
                 write(*,*) 'export 6 from',                            &
     &               (comm%id_neib(icou)-1), inp, jnp, knp, inod

              enddo
            endif
!
!  outdside (x<xmin, y<ymin)
!
            if(ipe .eq. 1  .and. jpe .eq. 1) then
              do knp = nb_rng%knp_st, nb_rng%knp_end

                 call set_boundary_size                                 &
     &              (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
                 sl_rng1%is = c_size%ndepth + 1
                 sl_rng1%js = c_size%ndepth + 1

                 icou = icou  + 1
                 sl_rng1%ie = 2*c_size%ndepth
                 sl_rng1%je = 2*c_size%ndepth
                 call set_ex_node(sl_rng1, loc_id,                      &
     &             comm%ntot_export, comm%item_export, inod)

                 sl_rng1%ie = 2*c_size%ndepth - 1
                 sl_rng1%je = 2*c_size%ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               ione, comm%ntot_export, comm%item_export, inod)

                 sl_rng1%ie = 2*c_size%ndepth
                 sl_rng1%je = 2*c_size%ndepth - 1
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               itwo, comm%ntot_export, comm%item_export, inod)

                 sl_rng1%ie = 2*c_size%ndepth
                 sl_rng1%je = 2*c_size%ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               ithree, comm%ntot_export, comm%item_export, inod)
                 write(*,*) 'export 7 from',                            &
     &               (comm%id_neib(icou)-1), inp, jnp, knp, inod

              enddo
            endif


!  outdside (x>xmax, y<ymin)
!
            if(ipe .eq. c_size%ndx  .and. jpe .eq. 1) then
             inp = 1
              do knp = nb_rng%knp_st, nb_rng%knp_end

                 call set_boundary_size                                 &
     &              (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
                 sl_rng1%is = c_size%nxi + 1
                 sl_rng1%ie = c_size%nxi + c_size%ndepth
                 sl_rng1%js = c_size%ndepth + 1

                 icou = icou  + 1
                 sl_rng1%je = 2*c_size%ndepth
                 call set_ex_node(sl_rng1, loc_id,                      &
     &             comm%ntot_export, comm%item_export, inod)

                 sl_rng1%je = 2*c_size%ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               ione, comm%ntot_export, comm%item_export, inod)

                 sl_rng1%je = 2*c_size%ndepth - 1
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               itwo, comm%ntot_export, comm%item_export, inod)

                 sl_rng1%je = 2*c_size%ndepth
                 call set_ex_edge                                       &
     &              (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,   &
     &               ithree, comm%ntot_export, comm%item_export, inod)
                 write(*,*) 'export 8 to',                              &
     &              (comm%id_neib(icou)-1), inp, jnp, knp, inod

              enddo
            endif
!
      end subroutine set_export_peri_quad
!
! ----------------------------------------------------------------------
!
      end module set_export_peri_cube
