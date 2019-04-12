!
!     module m_sleeve_cube
!
      module m_sleeve_cube
!
      use m_precision
!
      implicit none
!
!
!      type(neib_range_cube), save :: nb_rng1
      integer(kind = kint) :: is, js, ks
      integer(kind = kint) :: ie, je, ke
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
!
!       subroutine set_sleeve_size
!
       subroutine set_sleeve_size(inp, jnp, knp)
!
!      Written by Kemorin
!
      use m_size_of_cube
      use m_neighb_range_cube
!
      implicit none
!
      integer (kind = kint) :: inp,jnp,knp
!
!                                       .. start side

       if ( inp == -1 )  is=      1
       if ( inp == -1 )  ie=      ndepth
       if ( jnp == -1 )  js=      1
       if ( jnp == -1 )  je=      ndepth
       if ( knp == -1 )  ks=      1
       if ( knp == -1 )  ke=      ndepth

!                                       .. finish side

       if ( inp ==  1 )  is=i_end+1
       if ( inp ==  1 )  ie=i_end+ndepth
       if ( jnp ==  1 )  js=j_end+1
       if ( jnp ==  1 )  je=j_end+ndepth
       if ( knp ==  1 )  ks=k_end+1
       if ( knp ==  1 )  ke=k_end+ndepth

!                                       .. line pattern

       if ( inp ==  0 )  is=i_st
       if ( inp ==  0 )  ie=i_end
       if ( jnp ==  0 )  js=j_st
       if ( jnp ==  0 )  je=j_end
       if ( knp ==  0 )  ks=k_st
       if ( knp ==  0 )  ke=k_end
!
       return
       end subroutine set_sleeve_size
!
! ----------------------------------------------------------------------
!
!       subroutine set_boundary_size
!
       subroutine set_boundary_size(inp, jnp, knp)
!
!      Written by Kemorin
!
      use m_size_of_cube
      use m_neighb_range_cube
!
      implicit none
!
      integer (kind = kint) :: inp, jnp, knp
!
!                                       .. start side

       if ( inp == -1 )  is=i_st
       if ( inp == -1 )  ie=i_st+ndepth-1
       if ( jnp == -1 )  js=j_st
       if ( jnp == -1 )  je=j_st+ndepth-1
       if ( knp == -1 )  ks=k_st
       if ( knp == -1 )  ke=k_st+ndepth-1

!                                       .. finish side

       if ( inp ==  1 )  is=i_end-ndepth+1
       if ( inp ==  1 )  ie=i_end
       if ( jnp ==  1 )  js=j_end-ndepth+1
       if ( jnp ==  1 )  je=j_end
       if ( knp ==  1 )  ks=k_end-ndepth+1
       if ( knp ==  1 )  ke=k_end

!                                       .. line pattern

       if ( inp ==  0 )  is=i_st
       if ( inp ==  0 )  ie=i_end
       if ( jnp ==  0 )  js=j_st
       if ( jnp ==  0 )  je=j_end
       if ( knp ==  0 )  ks=k_st
       if ( knp ==  0 )  ke=k_end
!
       return
       end subroutine set_boundary_size
!
! ----------------------------------------------------------------------
!
       subroutine set_internal_size
!
       use m_neighb_range_cube
!
            is = i_st
            js = j_st
            ks = k_st
            ie = i_end
            je = j_end
            ke = k_end
!
       end subroutine set_internal_size
!
! ----------------------------------------------------------------------
!
       subroutine set_internal_edge_size(nd)
!
       use m_neighb_range_cube
       use m_neib_range_edge_cube
!
       implicit none
       integer (kind = kint) :: nd
!

       is = i_st
       js = j_st
       ks = k_st
       ie = i_end
       je = j_end
       ke = k_end
!
       if (nd .eq. 1) then
        is = iedge_st
        ie = iedge_end
       else if (nd .eq. 2) then
        js = jedge_st
        je = jedge_end
       else if (nd .eq. 3) then
        ks = kedge_st
        ke = kedge_end
       end if
!
       end subroutine set_internal_edge_size
!
! ----------------------------------------------------------------------
!
      end module m_sleeve_cube

