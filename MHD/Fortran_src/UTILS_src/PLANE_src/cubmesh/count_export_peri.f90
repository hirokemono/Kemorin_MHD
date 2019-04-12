!
!     module count_export_peri
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine count_export_peri_linear                             &
!!     &         (nb_rng, ipe, jpe, icou, inod)
!!      subroutine count_export_peri_quad                               &
!!     &          (nb_rng, ipe, jpe, kpe, icou, inod)
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module count_export_peri
!
      use m_precision
      use m_constants
!
      use t_neib_range_cube
      use m_size_4_plane
      use m_size_of_cube
      use m_comm_data_cube_kemo
      use m_sleeve_cube
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
      subroutine count_export_peri_linear                               &
     &         (nb_rng, ipe, jpe, icou, inod)
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe
!
      integer (kind = kint), intent(inout) :: icou, inod
!
      integer (kind = kint) :: inp, jnp, knp
!
!
!  outdside (x>xmax)
!
!                                     .... count nodes 
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
               call count_node_id(inod)

               stack_export(icou) = inod

              enddo
             enddo
            endif

!
!    ---   outside wall (x<xmin)
!
!                                     .... count nodes 
            if (ipe .eq. 1) then
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do jnp = nb_rng%jnp_st, nb_rng%jnp_end

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = ndepth+1
               sl_rng1%ie = 2*ndepth

               icou = icou  + 1
               call count_node_id(inod)

               stack_export(icou) = inod

              enddo
             enddo
            endif
!
!  outdside (y<ymax)
!
!                                     .... count nodes 
            if ( jpe .eq. ndy ) then
             jnp = 1
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%js = nyi+1
               sl_rng1%je = nyi+ndepth

               icou = icou  + 1
               call count_node_id(inod)

               stack_export(icou) = inod

              enddo
             enddo
            endif

!  outdside (y<ymin)
!
!                                     .... count nodes 
            if ( jpe .eq. 1 ) then
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%js = ndepth+1
               sl_rng1%je = 2*ndepth

               icou = icou  + 1
               call count_node_id(inod)

               stack_export(icou) = inod

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
               call count_node_id(inod)

               stack_export(icou) = inod

              enddo
            endif

!
!  outdside (x<xmin, y>ymax)
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
               call count_node_id(inod)

               stack_export(icou) = inod

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
               call count_node_id(inod)

               stack_export(icou) = inod

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
               call count_node_id(inod)

               stack_export(icou) = inod

              enddo
            endif

          end subroutine count_export_peri_linear
!
! ----------------------------------------------------------------------
!
      subroutine count_export_peri_quad                                 &
     &          (nb_rng, ipe, jpe, kpe, icou, inod)
!
      use set_comm_edge_4_cube
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe, kpe
!
      integer (kind = kint), intent(inout) :: icou, inod
!
      integer (kind = kint) :: inp, jnp, knp
!
!
!  outdside (x>xmax)
!                                     .... count nodes 
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
               call count_node_id(inod)

               call count_ex_edge(kpe, inp, jnp, knp, inod, ione)
               call count_ex_edge(kpe, inp, jnp, knp, inod, itwo)
               call count_ex_edge(kpe, inp, jnp, knp, inod, ithree)

               stack_export(icou) = inod

              enddo
             enddo
            endif
!
!    ---   outside wall (x<xmin)
!                                     .... count nodes 
            if (ipe .eq. 1) then
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do jnp = nb_rng%jnp_st, nb_rng%jnp_end

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = ndepth+1

               icou = icou  + 1
               sl_rng1%ie = 2*ndepth
               call count_node_id(inod)

               sl_rng1%ie = 2*ndepth - 1
               call count_ex_edge(kpe, inp, jnp, knp, inod, ione)

               sl_rng1%ie = 2*ndepth
               call count_ex_edge(kpe, inp, jnp, knp, inod, itwo)

               sl_rng1%ie = 2*ndepth
               call count_ex_edge(kpe, inp, jnp, knp, inod, ithree)

               stack_export(icou) = inod

              enddo
             enddo
            endif
!
!  outdside (y>ymax)
!                                     .... count nodes 
            if ( jpe .eq. ndy ) then
             jnp = 1
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%js = nyi+1
               sl_rng1%je = nyi+ndepth

               icou = icou  + 1
               call count_node_id(inod)

               call count_ex_edge(kpe, inp, jnp, knp, inod, ione)
               call count_ex_edge(kpe, inp, jnp, knp, inod, itwo)
               call count_ex_edge(kpe, inp, jnp, knp, inod, ithree)

               stack_export(icou) = inod

                enddo
              enddo
            endif
!
!  outdside (y<ymin)
!                                     .... count nodes 
            if ( jpe .eq. 1 ) then
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%js = ndepth+1

               icou = icou  + 1
               sl_rng1%je = 2*ndepth
               call count_node_id(inod)

               sl_rng1%je = 2*ndepth
               call count_ex_edge(kpe, inp, jnp, knp, inod, ione)

               sl_rng1%je = 2*ndepth - 1
               call count_ex_edge(kpe, inp, jnp, knp, inod, itwo)

               sl_rng1%je = 2*ndepth
               call count_ex_edge(kpe, inp, jnp, knp, inod, ithree)

               stack_export(icou) = inod

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
               call count_node_id(inod)

               call count_ex_edge(kpe, inp, jnp, knp, inod, ione)
               call count_ex_edge(kpe, inp, jnp, knp, inod, itwo)
               call count_ex_edge(kpe, inp, jnp, knp, inod, ithree)

               stack_export(icou) = inod

              enddo
            endif
!
!  outdside (x<xmin, y>ymax)
!
            if ( ipe .eq. 1  .and. jpe .eq. ndy ) then
             jnp = 1
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = ndepth+1
               sl_rng1%js = nyi+1
               sl_rng1%je = nyi+ndepth

               icou = icou  + 1
               sl_rng1%ie = 2*ndepth
               call count_node_id(inod)

               sl_rng1%ie = 2*ndepth - 1
               call count_ex_edge(kpe, inp, jnp, knp, inod, ione)

               sl_rng1%ie = 2*ndepth
               call count_ex_edge(kpe, inp, jnp, knp, inod, itwo)

               sl_rng1%ie = 2*ndepth
               call count_ex_edge(kpe, inp, jnp, knp, inod, ithree)
!
               stack_export(icou) = inod

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
               sl_rng1%js = ndepth+1

               icou = icou  + 1
               sl_rng1%ie = 2*ndepth
               sl_rng1%je = 2*ndepth
               call count_node_id(inod)

               sl_rng1%ie = 2*ndepth - 1
               sl_rng1%je = 2*ndepth
               call count_ex_edge(kpe, inp, jnp, knp, inod, ione)

               sl_rng1%ie = 2*ndepth
               sl_rng1%je = 2*ndepth - 1
               call count_ex_edge(kpe, inp, jnp, knp, inod, itwo)

               sl_rng1%ie = 2*ndepth
               sl_rng1%je = 2*ndepth
               call count_ex_edge(kpe, inp, jnp, knp, inod, ithree)

               stack_export(icou) = inod

              enddo
            endif
!
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

               icou = icou  + 1
               sl_rng1%je = 2*ndepth
               call count_node_id(inod)

               sl_rng1%je = 2*ndepth
               call count_ex_edge(kpe, inp, jnp, knp, inod, ione)

               sl_rng1%je = 2*ndepth - 1
               call count_ex_edge(kpe, inp, jnp, knp, inod, itwo)

               sl_rng1%je = 2*ndepth
               call count_ex_edge(kpe, inp, jnp, knp, inod, ithree)

               stack_export(icou) = inod

              enddo
            endif

          end subroutine count_export_peri_quad
!
! ----------------------------------------------------------------------
!
      end module count_export_peri
