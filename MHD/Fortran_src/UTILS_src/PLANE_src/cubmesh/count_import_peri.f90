!count_import_peri.f90
!     module count_import_peri
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine count_import_peri_linear(c_size, nb_rng, ipe, jpe,   &
!!     &          num_neib, istack_import, icou, inod)
!!      subroutine count_import_peri_quad(c_size, nb_rng, ipe, jpe, kpe,&
!!     &          num_neib, istack_import, icou, inod)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module count_import_peri
!
      use m_precision
      use m_constants
!
      use t_size_of_cube
      use t_neib_range_cube
      use t_sleeve_cube
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
      subroutine count_import_peri_linear(c_size, nb_rng, ipe, jpe,     &
     &          num_neib, istack_import, icou, inod)
!
      type(neib_range_cube), intent(in) :: nb_rng
      type(size_of_cube), intent(in) :: c_size
      integer (kind = kint), intent(in) :: ipe, jpe
      integer (kind = kint), intent(in) :: num_neib
!
      integer (kind = kint), intent(inout) :: istack_import(0:num_neib)
      integer (kind = kint), intent(inout) :: icou, inod
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inp, jnp, knp
!
!    ---   outside wall (x<xmin)
!                                     .... count nodes 
            if (ipe .eq. 1) then
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do jnp = nb_rng%jnp_st, nb_rng%jnp_end

               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = 1
               sl_rng1%ie = c_size%ndepth

               icou = icou  + 1
               call count_node_id(sl_rng1, inod)

               istack_import(icou) = inod

              enddo
             enddo
            endif
!
!  outdside (x>xmax)
!                                     .... count nodes 
            if (ipe .eq. c_size%ndx) then
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do jnp = nb_rng%jnp_st, nb_rng%jnp_end
!
               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = c_size%nxi + c_size%ndepth + 1
               sl_rng1%ie = c_size%nxi + 2*c_size%ndepth

               icou = icou  + 1
               call count_node_id(sl_rng1, inod)

               istack_import(icou) = inod

              enddo
             enddo
            endif
!
!  outdside (y<ymin)
!                                     .... count nodes 
            if ( jpe .eq. 1 ) then
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%js = 1
               sl_rng1%je = c_size%ndepth

               icou = icou  + 1
               call count_node_id(sl_rng1, inod)

               istack_import(icou) = inod

              enddo
             enddo
            endif
!
!  outdside (y<ymax)
!                                     .... count nodes 
            if ( jpe .eq. c_size%ndy) then
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%js = c_size%nyi + c_size%ndepth + 1
               sl_rng1%je = c_size%nyi + 2*c_size%ndepth

               icou = icou  + 1
               call count_node_id(sl_rng1, inod)

               istack_import(icou) = inod

              enddo
             enddo
            endif
!
!  outdside (x<xmin, y<ymin)
!
            if ( ipe .eq. 1  .and. jpe .eq. 1 ) then
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = 1
               sl_rng1%ie = c_size%ndepth
               sl_rng1%js = 1
               sl_rng1%je = c_size%ndepth

               icou = icou  + 1
               call count_node_id(sl_rng1, inod)

               istack_import(icou) = inod

              enddo
            endif
!
!  outdside (x>xmax, y<ymin)
!
            if ( ipe .eq. c_size%ndx  .and. jpe .eq. 1 ) then
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = c_size%nxi + c_size%ndepth + 1
               sl_rng1%ie = c_size%nxi + 2*c_size%ndepth
               sl_rng1%js = 1
               sl_rng1%je = c_size%ndepth

               icou = icou  + 1
               call count_node_id(sl_rng1, inod)

               istack_import(icou) = inod

              enddo
            endif
!
!  outdside (x>xmax, y<ymax)
!
            if(ipe .eq. c_size%ndx  .and. jpe .eq. c_size%ndy) then
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = c_size%nxi + c_size%ndepth + 1
               sl_rng1%ie = c_size%nxi + 2*c_size%ndepth
               sl_rng1%js = c_size%nyi + c_size%ndepth + 1
               sl_rng1%je = c_size%nyi + 2*c_size%ndepth

               icou = icou  + 1
               call count_node_id(sl_rng1, inod)

               istack_import(icou) = inod

              enddo
            endif
!
!  outdside (x>xmin, y<ymax)
!
            if ( ipe .eq. 1  .and. jpe .eq. c_size%ndy) then
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = 1
               sl_rng1%ie = c_size%ndepth
               sl_rng1%js = c_size%nyi + c_size%ndepth + 1
               sl_rng1%je = c_size%nyi + 2*c_size%ndepth

               icou = icou  + 1
               call count_node_id(sl_rng1, inod)

               istack_import(icou) = inod

              enddo
            endif
!
      end subroutine  count_import_peri_linear
!
! ----------------------------------------------------------------------
!
      subroutine count_import_peri_quad(c_size, nb_rng, ipe, jpe, kpe,  &
     &          num_neib, istack_import, icou, inod)
!
      use set_comm_edge_4_cube
!
      type(neib_range_cube), intent(in) :: nb_rng
      type(size_of_cube), intent(in) :: c_size
      integer (kind = kint), intent(in) :: ipe, jpe, kpe
      integer (kind = kint), intent(in) :: num_neib
!
      integer (kind = kint), intent(inout) :: istack_import(0:num_neib)
      integer (kind = kint), intent(inout) :: icou, inod
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inp, jnp, knp
!
!    ---   outside wall (x<xmin)
!                                     .... count nodes 
            if (ipe .eq. 1) then
             inp = -1
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do jnp = nb_rng%jnp_st, nb_rng%jnp_end

               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = 1
               sl_rng1%ie = c_size%ndepth

               icou = icou  + 1
               call count_node_id(sl_rng1, inod)

               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ione)
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, itwo)
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ithree)

               istack_import(icou) = inod

              enddo
             enddo
            endif
!
!  outdside (x>xmax)
!                                     .... count nodes 
!
            if (ipe .eq. c_size%ndx) then
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do jnp = nb_rng%jnp_st, nb_rng%jnp_end
!
               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = c_size%nxi + c_size%ndepth + 1

               icou = icou  + 1
               sl_rng1%ie = c_size%nxi + 2*c_size%ndepth
               call count_node_id(sl_rng1, inod)

               sl_rng1%ie = c_size%nxi + 2*c_size%ndepth - 1
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ione)

               sl_rng1%ie = c_size%nxi + 2*c_size%ndepth
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, itwo)

               sl_rng1%ie = c_size%nxi + 2*c_size%ndepth
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ithree)

               istack_import(icou) = inod

              enddo
             enddo
            endif
!
!  outdside (y<ymin)
!                                     .... count nodes 
            if ( jpe .eq. 1 ) then
             jnp = -1
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%js = 1
               sl_rng1%je = c_size%ndepth

               icou = icou  + 1
               call count_node_id(sl_rng1, inod)

               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ione)
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, itwo)
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ithree)

               istack_import(icou) = inod

              enddo
             enddo
            endif
!
!  outdside (y>ymax)
!                                     .... count nodes 
            if ( jpe .eq. c_size%ndy) then
             do knp = nb_rng%knp_st, nb_rng%knp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%js = c_size%nyi + c_size%ndepth + 1

               icou = icou  + 1
               sl_rng1%je = c_size%nyi + 2*c_size%ndepth
               call count_node_id(sl_rng1, inod)

               sl_rng1%je = c_size%nyi + 2*c_size%ndepth
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ione)

               sl_rng1%je = c_size%nyi + 2*c_size%ndepth - 1
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, itwo)

               sl_rng1%je = c_size%nyi + 2*c_size%ndepth
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ithree)

               istack_import(icou) = inod

              enddo
             enddo
            endif
!
!  outdside (x<xmin, y<ymin)
!
            if ( ipe .eq. 1  .and. jpe .eq. 1 ) then
             inp = -1
             jnp = -1
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = 1
               sl_rng1%ie = c_size%ndepth
               sl_rng1%js = 1
               sl_rng1%je = c_size%ndepth

               icou = icou  + 1
               call count_node_id(sl_rng1, inod)

               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ione)
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, itwo)
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ithree)

               istack_import(icou) = inod

              enddo
            endif
!
!  outdside (x>xmax, y<ymin)
!
            if ( ipe .eq. c_size%ndx  .and. jpe .eq. 1 ) then
             jnp = -1
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%js = 1
               sl_rng1%je = c_size%ndepth
               sl_rng1%is = c_size%nxi + c_size%ndepth + 1

               icou = icou  + 1
               sl_rng1%ie = c_size%nxi + 2*c_size%ndepth
               call count_node_id(sl_rng1, inod)

               sl_rng1%ie = c_size%nxi + 2*c_size%ndepth - 1
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ione)

               sl_rng1%ie = c_size%nxi + 2*c_size%ndepth
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, itwo)

               sl_rng1%ie = c_size%nxi + 2*c_size%ndepth
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ithree)

               istack_import(icou) = inod

              enddo
            endif
!
!  outdside (x>xmax, y<ymax)
!
            if ( ipe .eq. c_size%ndx  .and. jpe .eq. c_size%ndy) then
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = c_size%nxi + c_size%ndepth + 1
               sl_rng1%js = c_size%nyi + c_size%ndepth + 1

               icou = icou  + 1
               sl_rng1%ie = c_size%nxi + 2*c_size%ndepth
               sl_rng1%je = c_size%nyi + 2*c_size%ndepth
               call count_node_id(sl_rng1, inod)

               sl_rng1%ie = c_size%nxi + 2*c_size%ndepth - 1
               sl_rng1%je = c_size%nyi + 2*c_size%ndepth
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ione)

               sl_rng1%ie = c_size%nxi + 2*c_size%ndepth
               sl_rng1%je = c_size%nyi + 2*c_size%ndepth - 1
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, itwo)

               sl_rng1%ie = c_size%nxi + 2*c_size%ndepth
               sl_rng1%je = c_size%nyi + 2*c_size%ndepth
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ithree)

               istack_import(icou) = inod

              enddo
            endif
!
!  outdside (x>xmin, y<ymax)
!
            if ( ipe .eq. 1  .and. jpe .eq. c_size%ndy) then
             inp = -1
              do knp = nb_rng%knp_st, nb_rng%knp_end

               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               sl_rng1%is = 1
               sl_rng1%ie = c_size%ndepth
               sl_rng1%js = c_size%nyi + c_size%ndepth + 1

               icou = icou  + 1
               sl_rng1%je = c_size%nyi + 2*c_size%ndepth
               call count_node_id(sl_rng1, inod)

               sl_rng1%je = c_size%nyi + 2*c_size%ndepth
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ione)

               sl_rng1%je = c_size%nyi + 2*c_size%ndepth - 1
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, itwo)

               sl_rng1%je = c_size%nyi + 2*c_size%ndepth
               call count_im_edge(sl_rng1, c_size%ndz,                  &
     &             kpe, inp, jnp, knp, inod, ithree)

               istack_import(icou) = inod

              enddo
            endif
!
          end subroutine count_import_peri_quad
!
! ----------------------------------------------------------------------
!
      end module count_import_peri
