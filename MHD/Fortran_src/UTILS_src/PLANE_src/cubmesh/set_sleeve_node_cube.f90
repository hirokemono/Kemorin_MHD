!set_sleeve_node_cube.f90
!     module set_sleeve_node_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
! ***** set and write coordinate for sleeve area nodes
!
!!      subroutine set_sleeve_node(c_size, c_vert, nb_rng, inod)
!!      subroutine set_sleeve_node_quad                                 &
!!     &         (c_size, c_vert, nb_rng, kpe, inod)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(vertical_position_cube), intent(in) :: c_vert
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module set_sleeve_node_cube
!
      use m_precision
!
      use t_size_of_cube
      use t_neib_range_cube
      use t_sleeve_cube
      use t_cube_position
      use set_internal_nod_cube
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_sleeve_node(c_size, c_vert, nb_rng, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(inout) :: inod
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inp, jnp, knp
!
!
      do knp = nb_rng%knp_st, nb_rng%knp_end
        do jnp = nb_rng%jnp_st, nb_rng%jnp_end
          do inp = nb_rng%inp_st, nb_rng%inp_end

            if ((inp==0).and.(jnp==0).and.(knp==0)) cycle
!
            call set_sleeve_size                                        &
     &         (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
            call set_internal_node                                      &
     &         (c_size, c_vert, nb_rng, sl_rng1, inod)
          enddo
        enddo
      enddo
!
      end subroutine set_sleeve_node
!
! ----------------------------------------------------------------------
!
      subroutine set_sleeve_node_quad                                   &
     &         (c_size, c_vert, nb_rng, kpe, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: kpe
      integer (kind = kint), intent(inout) :: inod

      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inp, jnp, knp
      integer (kind = kint) :: nd
!
!
! ***** set and write coordinate for sleeve area nodes
!
      do knp = nb_rng%knp_st, nb_rng%knp_end
        do jnp = nb_rng%jnp_st, nb_rng%jnp_end
          do inp = nb_rng%inp_st, nb_rng%inp_end

            if ((inp==0).and.(jnp==0).and.(knp==0)) cycle
!
            call set_sleeve_size                                        &
     &         (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)
!
!     nd = 1: for edge on y = const, z = const
!     nd = 2: for edge on y = const, z = const
!     nd = 3: for edge on x = const, y = const
            call set_internal_node                                      &
     &         (c_size, c_vert, nb_rng, sl_rng1, inod)
            do nd = 1, 3
              call set_internal_edge(c_size, c_vert, nb_rng, sl_rng1,   &
     &            kpe, inp, jnp, knp, inod, nd)
            end do
!
          enddo
        enddo
      enddo
!
      end subroutine set_sleeve_node_quad
!
! ----------------------------------------------------------------------
!
      end module set_sleeve_node_cube
