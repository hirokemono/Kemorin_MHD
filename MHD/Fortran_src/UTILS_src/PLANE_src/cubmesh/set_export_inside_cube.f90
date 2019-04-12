!set_export_inside_cube.f90
!     module set_export_inside_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_export_inside(nb_rng, icou, inod)
!!      subroutine set_export_inside_quad(nb_rng, kpe, icou, inod)
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module set_export_inside_cube
!
      use m_precision
!
      use t_neib_range_cube
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
      subroutine set_export_inside(nb_rng, icou, inod)
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(inout) :: icou, inod
!
      integer (kind = kint) :: inp, jnp, knp
!
!
            do knp = nb_rng%knp_st, nb_rng%knp_end
             do jnp = nb_rng%jnp_st, nb_rng%jnp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)

               icou = icou  + 1

               call set_ex_node(sl_rng1, inod)
!               write(*,*) 'inod', inod

              enddo
             enddo
            enddo
!
!
      end subroutine set_export_inside
!
! ----------------------------------------------------------------------
!
      subroutine set_export_inside_quad(nb_rng, kpe, icou, inod)
!
      use set_comm_edge_4_cube
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: kpe
      integer (kind = kint), intent(inout) :: icou, inod
!
      integer (kind = kint) :: inp, jnp, knp
      integer (kind = kint) :: nd
!
!
            do knp = nb_rng%knp_st, nb_rng%knp_end
             do jnp = nb_rng%jnp_st, nb_rng%jnp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

               call set_boundary_size                                   &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)

               icou = icou  + 1

               call set_ex_node(sl_rng1, inod)
               write(*,*) 'export node 0 to',                           &
     &                   (neibpe(icou)-1), inp, jnp, knp, inod

               nd = 1
               call set_ex_edge(sl_rng1, kpe, inp, jnp, knp, inod, nd)
               write(*,*) 'export edge1 0 to',                          &
     &                   (neibpe(icou)-1), inp, jnp, knp, inod

               nd = 2
               call set_ex_edge(sl_rng1, kpe, inp, jnp, knp, inod, nd)
               write(*,*) 'export edge2 0 to',                          &
     &                   (neibpe(icou)-1), inp, jnp, knp, inod

               nd = 3
               call set_ex_edge(sl_rng1, kpe, inp, jnp, knp, inod, nd)
               write(*,*) 'export edge3 0 to',                          &
     &                   (neibpe(icou)-1), inp, jnp, knp, inod

              enddo
             enddo
            enddo
!
      end subroutine set_export_inside_quad
!
!
! ----------------------------------------------------------------------
!
      end module set_export_inside_cube
