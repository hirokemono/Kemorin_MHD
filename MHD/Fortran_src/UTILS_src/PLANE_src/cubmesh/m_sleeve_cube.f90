!
!     module m_sleeve_cube
!
!      Written by Kemorin
!
      module m_sleeve_cube
!
      use m_precision
      use t_neib_range_cube
      use t_sleeve_cube
!
      implicit none
!
!
      type(slleve_range), save :: sl_rng1
      integer(kind = kint) :: is, js, ks
      integer(kind = kint) :: ie, je, ke
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine set_sleeve_size(inp, jnp, knp, nb_rng)
!
      use m_size_of_cube
!
      integer (kind = kint), intent(in) :: inp, jnp, knp
      type(neib_range_cube), intent(in) :: nb_rng
!
      call s_set_sleeve_size(nb_rng, ndepth, inp, jnp, knp, sl_rng1)
      call copy_internal_size
!
       return
       end subroutine set_sleeve_size
!
! ----------------------------------------------------------------------
!
       subroutine set_boundary_size(inp, jnp, knp, nb_rng)
!
      use m_size_of_cube
!
      integer (kind = kint), intent(in) :: inp, jnp, knp
      type(neib_range_cube), intent(in) :: nb_rng
!
!
      call s_set_boundary_size(nb_rng, ndepth, inp, jnp, knp, sl_rng1)
      call copy_internal_size
!
      return
      end subroutine set_boundary_size
!
! ----------------------------------------------------------------------
!
      subroutine set_internal_size(nb_rng)
!
      type(neib_range_cube), intent(in) :: nb_rng
!
      call s_set_internal_size(nb_rng, sl_rng1)
      call copy_internal_size
!
      end subroutine set_internal_size
!
! ----------------------------------------------------------------------
!
      subroutine set_internal_edge_size(nb_rng, nd)
!
      integer (kind = kint), intent(in) :: nd
      type(neib_range_cube), intent(in) :: nb_rng
!
      call s_set_internal_edge_size(nb_rng, nd, sl_rng1)
      call copy_internal_size
!
      end subroutine set_internal_edge_size
!
! ----------------------------------------------------------------------
!
      subroutine copy_internal_size
!
!
            is = sl_rng1%is
            js = sl_rng1%js
            ks = sl_rng1%ks
            ie = sl_rng1%ie
            je = sl_rng1%je
            ke = sl_rng1%ke
!
      end subroutine copy_internal_size
!
! ----------------------------------------------------------------------
!
      end module m_sleeve_cube

