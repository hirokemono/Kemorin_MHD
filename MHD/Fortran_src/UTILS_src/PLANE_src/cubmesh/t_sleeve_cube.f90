!
!     module t_sleeve_cube
!
!!     written by H. Matsui
!
!!      subroutine set_sleeve_size(nb_rng, inp, jnp, knp, sl_rng)
!!      subroutine set_boundary_size(nb_rng, inp, jnp, knp, sl_rng)
!!      subroutine set_internal_size(nb_rng, sl_rng)
!!      subroutine set_internal_edge_size(nb_rng, nd, sl_rng)
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type(slleve_range), intent(inout) :: sl_rng
!!
      module t_sleeve_cube
!
      use m_precision
      use t_neib_range_cube
!
      implicit none
!
!
      type slleve_range
        integer(kind = kint) :: is
        integer(kind = kint) :: ie
!
        integer(kind = kint) :: js
        integer(kind = kint) :: je
!
        integer(kind = kint) :: ks
        integer(kind = kint) :: ke
      end type slleve_range
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_sleeve_size(nb_rng, inp, jnp, knp, sl_rng)
!
      use m_size_of_cube
!
      integer (kind = kint), intent(in) :: inp, jnp, knp
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(inout) :: sl_rng
!
!                                       .. start side

       if ( inp == -1 )  sl_rng%is =      1
       if ( inp == -1 )  sl_rng%ie =      ndepth
       if ( jnp == -1 )  sl_rng%js =      1
       if ( jnp == -1 )  sl_rng%je =      ndepth
       if ( knp == -1 )  sl_rng%ks =      1
       if ( knp == -1 )  sl_rng%ke =      ndepth

!                                       .. finish side

       if ( inp ==  1 )  sl_rng%is = nb_rng%i_end + 1
       if ( inp ==  1 )  sl_rng%ie = nb_rng%i_end + ndepth
       if ( jnp ==  1 )  sl_rng%js = nb_rng%j_end + 1
       if ( jnp ==  1 )  sl_rng%je = nb_rng%j_end + ndepth
       if ( knp ==  1 )  sl_rng%ks = nb_rng%k_end + 1
       if ( knp ==  1 )  sl_rng%ke = nb_rng%k_end + ndepth

!                                       .. line pattern

       if ( inp ==  0 )  sl_rng%is = nb_rng%i_st
       if ( inp ==  0 )  sl_rng%ie = nb_rng%i_end
       if ( jnp ==  0 )  sl_rng%js = nb_rng%j_st
       if ( jnp ==  0 )  sl_rng%je = nb_rng%j_end
       if ( knp ==  0 )  sl_rng%ks = nb_rng%k_st
       if ( knp ==  0 )  sl_rng%ke = nb_rng%k_end
!
       return
       end subroutine set_sleeve_size
!
! ----------------------------------------------------------------------
!
      subroutine set_boundary_size(nb_rng, inp, jnp, knp, sl_rng)
!
      use m_size_of_cube
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: inp, jnp, knp
      type(slleve_range), intent(inout) :: sl_rng
!
!                                       .. start side

       if ( inp == -1 )  sl_rng%is = nb_rng%i_st
       if ( inp == -1 )  sl_rng%ie = nb_rng%i_st + ndepth - 1
       if ( jnp == -1 )  sl_rng%js = nb_rng%j_st
       if ( jnp == -1 )  sl_rng%je = nb_rng%j_st + ndepth - 1
       if ( knp == -1 )  sl_rng%ks = nb_rng%k_st
       if ( knp == -1 )  sl_rng%ke = nb_rng%k_st + ndepth - 1

!                                       .. finish side

       if ( inp ==  1 )  sl_rng%is = nb_rng%i_end - ndepth + 1
       if ( inp ==  1 )  sl_rng%ie = nb_rng%i_end
       if ( jnp ==  1 )  sl_rng%js = nb_rng%j_end - ndepth + 1
       if ( jnp ==  1 )  sl_rng%je = nb_rng%j_end
       if ( knp ==  1 )  sl_rng%ks = nb_rng%k_end - ndepth + 1
       if ( knp ==  1 )  sl_rng%ke = nb_rng%k_end

!                                       .. line pattern

       if ( inp ==  0 )  sl_rng%is = nb_rng%i_st
       if ( inp ==  0 )  sl_rng%ie = nb_rng%i_end
       if ( jnp ==  0 )  sl_rng%js = nb_rng%j_st
       if ( jnp ==  0 )  sl_rng%je = nb_rng%j_end
       if ( knp ==  0 )  sl_rng%ks = nb_rng%k_st
       if ( knp ==  0 )  sl_rng%ke = nb_rng%k_end
!
       return
       end subroutine set_boundary_size
!
! ----------------------------------------------------------------------
!
      subroutine set_internal_size(nb_rng, sl_rng)
!
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(inout) :: sl_rng
!
      sl_rng%is = nb_rng%i_st
      sl_rng%js = nb_rng%j_st
      sl_rng%ks = nb_rng%k_st
      sl_rng%ie = nb_rng%i_end
      sl_rng%je = nb_rng%j_end
      sl_rng%ke = nb_rng%k_end
!
      end subroutine set_internal_size
!
! ----------------------------------------------------------------------
!
      subroutine set_internal_edge_size(nb_rng, nd, sl_rng)
!
      integer(kind = kint), intent(in) :: nd
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(inout) :: sl_rng
!

      sl_rng%is = nb_rng%i_st
      sl_rng%js = nb_rng%j_st
      sl_rng%ks = nb_rng%k_st
      sl_rng%ie = nb_rng%i_end
      sl_rng%je = nb_rng%j_end
      sl_rng%ke = nb_rng%k_end
!
      if (nd .eq. 1) then
        sl_rng%is = nb_rng%iedge_st
        sl_rng%ie = nb_rng%iedge_end
      else if (nd .eq. 2) then
        sl_rng%js = nb_rng%jedge_st
        sl_rng%je = nb_rng%jedge_end
      else if (nd .eq. 3) then
        sl_rng%ks = nb_rng%kedge_st
        sl_rng%ke = nb_rng%kedge_end
      end if
!
      end subroutine set_internal_edge_size
!
! ----------------------------------------------------------------------
!
      end module t_sleeve_cube

