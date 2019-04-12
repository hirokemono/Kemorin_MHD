!
!     module m_sleeve_cube
!
!      Written by Kemorin
!
      module m_sleeve_cube
!
      use m_precision
       use t_neib_range_cube
!
      implicit none
!
!
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
      type(neib_range_cube), intent(inout) :: nb_rng
!
!                                       .. start side

       if ( inp == -1 )  is=      1
       if ( inp == -1 )  ie=      ndepth
       if ( jnp == -1 )  js=      1
       if ( jnp == -1 )  je=      ndepth
       if ( knp == -1 )  ks=      1
       if ( knp == -1 )  ke=      ndepth

!                                       .. finish side

       if ( inp ==  1 )  is = nb_rng%i_end + 1
       if ( inp ==  1 )  ie = nb_rng%i_end+ndepth
       if ( jnp ==  1 )  js = nb_rng%j_end + 1
       if ( jnp ==  1 )  je = nb_rng%j_end+ndepth
       if ( knp ==  1 )  ks = nb_rng%k_end + 1
       if ( knp ==  1 )  ke = nb_rng%k_end + ndepth

!                                       .. line pattern

       if ( inp ==  0 )  is = nb_rng%i_st
       if ( inp ==  0 )  ie = nb_rng%i_end
       if ( jnp ==  0 )  js = nb_rng%j_st
       if ( jnp ==  0 )  je = nb_rng%j_end
       if ( knp ==  0 )  ks = nb_rng%k_st
       if ( knp ==  0 )  ke = nb_rng%k_end
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
      type(neib_range_cube), intent(inout) :: nb_rng
!
!                                       .. start side

       if ( inp == -1 )  is = nb_rng%i_st
       if ( inp == -1 )  ie = nb_rng%i_st + ndepth - 1
       if ( jnp == -1 )  js = nb_rng%j_st
       if ( jnp == -1 )  je = nb_rng%j_st + ndepth - 1
       if ( knp == -1 )  ks = nb_rng%k_st
       if ( knp == -1 )  ke = nb_rng%k_st + ndepth - 1

!                                       .. finish side

       if ( inp ==  1 )  is = nb_rng%i_end - ndepth + 1
       if ( inp ==  1 )  ie = nb_rng%i_end
       if ( jnp ==  1 )  js = nb_rng%j_end - ndepth + 1
       if ( jnp ==  1 )  je = nb_rng%j_end
       if ( knp ==  1 )  ks = nb_rng%k_end - ndepth + 1
       if ( knp ==  1 )  ke = nb_rng%k_end

!                                       .. line pattern

       if ( inp ==  0 )  is = nb_rng%i_st
       if ( inp ==  0 )  ie = nb_rng%i_end
       if ( jnp ==  0 )  js = nb_rng%j_st
       if ( jnp ==  0 )  je = nb_rng%j_end
       if ( knp ==  0 )  ks = nb_rng%k_st
       if ( knp ==  0 )  ke = nb_rng%k_end
!
       return
       end subroutine set_boundary_size
!
! ----------------------------------------------------------------------
!
       subroutine set_internal_size(nb_rng)
!
      type(neib_range_cube), intent(inout) :: nb_rng
!
            is = nb_rng%i_st
            js = nb_rng%j_st
            ks = nb_rng%k_st
            ie = nb_rng%i_end
            je = nb_rng%j_end
            ke = nb_rng%k_end
!
       end subroutine set_internal_size
!
! ----------------------------------------------------------------------
!
       subroutine set_internal_edge_size(nd, nb_rng)
!
       implicit none
       integer (kind = kint), intent(in) :: nd
      type(neib_range_cube), intent(inout) :: nb_rng
!
       call set_internal_size(nb_rng)
!
       if (nd .eq. 1) then
        is = nb_rng%iedge_st
        ie = nb_rng%iedge_end
       else if (nd .eq. 2) then
        js = nb_rng%jedge_st
        je = nb_rng%jedge_end
       else if (nd .eq. 3) then
        ks = nb_rng%kedge_st
        ke = nb_rng%kedge_end
       end if
!
       end subroutine set_internal_edge_size
!
! ----------------------------------------------------------------------
!
      end module m_sleeve_cube

