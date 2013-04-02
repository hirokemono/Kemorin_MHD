!set_sleeve_node_cube.f90
!     module set_sleeve_node_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
! ***** set and write coordinate for sleeve area nodes
!
!       subroutine set_sleeve_node(inod)
!       subroutine set_sleeve_node_quad(kpe, inod)
!
      module set_sleeve_node_cube
!
      use m_precision
!
      use m_size_of_cube
      use m_neighb_range_cube
      use m_sleeve_cube
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
       subroutine set_sleeve_node(inod)
!
      integer (kind = kint), intent(inout) :: inod
      integer (kind = kint) :: inp, jnp, knp
!
!
      do knp=knp_st,knp_end
        do jnp=jnp_st,jnp_end
          do inp=inp_st,inp_end

            if ((inp==0).and.(jnp==0).and.(knp==0)) cycle
!
            call set_sleeve_size(inp, jnp, knp)
            call set_internal_node(inod)
          enddo
        enddo
      enddo
!
      end subroutine set_sleeve_node
!
! ----------------------------------------------------------------------
!
       subroutine set_sleeve_node_quad(kpe, inod)
!
      integer (kind = kint), intent(in) :: kpe
      integer (kind = kint), intent(inout) :: inod

      integer (kind = kint) :: inp, jnp, knp
      integer (kind = kint) :: nd
!
!
! ***** set and write coordinate for sleeve area nodes
!
      do knp=knp_st,knp_end
        do jnp=jnp_st,jnp_end
          do inp=inp_st,inp_end

            if ((inp==0).and.(jnp==0).and.(knp==0)) cycle
!
            call set_sleeve_size(inp,jnp,knp)

            call set_internal_node(inod)
!
!     nd = 1: for edge on y = const, z = const
!     nd = 2: for edge on y = const, z = const
!     nd = 3: for edge on x = const, y = const
            do nd = 1, 3
              call set_internal_edge(kpe, inp, jnp, knp, inod, nd)
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
