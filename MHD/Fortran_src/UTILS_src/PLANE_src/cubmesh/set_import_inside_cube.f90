!set_import_inside_cube.f90
!     module set_import_inside_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_import_inside                                    &
!!     &         (c_size, nb_rng, loc_id, comm, icou, inod)
!!      subroutine set_import_inside_quad                               &
!!     &         (c_size, nb_rng, loc_id, kpe, comm, icou, inod)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type(local_node_id_cube), intent(in) :: loc_id
!!        type(communication_table), intent(inout) :: comm
!
      module set_import_inside_cube
!
      use m_precision
      use m_constants
!
      use t_size_of_cube
      use t_neib_range_cube
      use t_sleeve_cube
      use t_comm_table
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
      subroutine set_import_inside                                      &
     &         (c_size, nb_rng, loc_id, comm, icou, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      type(local_node_id_cube), intent(in) :: loc_id
!
      type(communication_table), intent(inout) :: comm
      integer (kind = kint), intent(inout) :: icou, inod
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inp, jnp, knp
!
!
      do knp = nb_rng%knp_st, nb_rng%knp_end
        do jnp = nb_rng%jnp_st, nb_rng%jnp_end
          do inp = nb_rng%inp_st, nb_rng%inp_end

            if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

            call set_sleeve_size                                        &
     &         (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
            icou = icou  + 1

            call set_im_node(sl_rng1, loc_id,                           &
     &          comm%ntot_import, comm%item_import, inod)

          enddo
        enddo
      enddo
!
      end subroutine set_import_inside
!
! ----------------------------------------------------------------------
!
      subroutine set_import_inside_quad                                 &
     &         (c_size, nb_rng, loc_id, kpe, comm, icou, inod)
!
      use set_comm_edge_4_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      type(local_node_id_cube), intent(in) :: loc_id
      integer (kind = kint), intent(in) :: kpe
!
      type(communication_table), intent(inout) :: comm
      integer (kind = kint), intent(inout) :: icou, inod
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inp, jnp, knp
!
            do knp = nb_rng%knp_st, nb_rng%knp_end
             do jnp = nb_rng%jnp_st, nb_rng%jnp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

               call set_sleeve_size                                     &
     &            (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
               icou = icou  + 1

               call set_im_node(sl_rng1, loc_id,                        &
     &             comm%ntot_import, comm%item_import, inod)
               write(*,*) 'import node 0 from',                         &
     &                     comm%id_neib(icou), inp, jnp, knp, inod

               call set_im_edge                                         &
     &            (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,     &
     &             ione, comm%ntot_import, comm%item_import, inod)
               write(*,*) 'import edge1 0 from',                        &
     &                     comm%id_neib(icou), inp, jnp, knp, inod

               call set_im_edge                                         &
     &            (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,     &
     &             itwo, comm%ntot_import, comm%item_import, inod)
               write(*,*) 'import edge2 0 from',                        &
     &                     comm%id_neib(icou), inp, jnp, knp, inod

               call set_im_edge                                         &
     &            (sl_rng1, loc_id, c_size%ndz, kpe, inp, jnp, knp,     &
     &             ithree, comm%ntot_import, comm%item_import, inod)

               write(*,*) 'import edge3 0 from',                        &
     &                     comm%id_neib(icou), inp, jnp, knp, inod

                enddo
              enddo
            enddo
!
!
      end subroutine set_import_inside_quad
!
! ----------------------------------------------------------------------
!
      end module set_import_inside_cube
