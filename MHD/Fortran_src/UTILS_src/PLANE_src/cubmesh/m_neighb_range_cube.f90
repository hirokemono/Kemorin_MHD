!
!     module m_neighb_range_cube
!
      module m_neighb_range_cube
!
!     written by Kemorin
!
      use m_precision
      use t_neib_range_cube
!
      implicit  none
!
      type(neib_range_cube), save :: nb_rng1
!
      integer(kind=kint )  ::  inp_st , jnp_st , knp_st
      integer(kind=kint )  ::  inp_end, jnp_end, knp_end
!
      integer(kind=kint )  ::  i_st   , j_st   , k_st
      integer(kind=kint )  ::  i_end  , j_end  , k_end
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_range_4_neighbour(ipe, jpe, kpe)
!
      use m_size_4_plane
      use m_size_of_cube
!
      integer (kind = kint) :: ipe, jpe, kpe
!
!
      call set_range_4_neighbour(ipe, jpe, kpe, nb_rng1)
!
      inp_st =  nb_rng1%inp_st 
      inp_end = nb_rng1%inp_end

      jnp_st =  nb_rng1%jnp_st 
      jnp_end = nb_rng1%jnp_end

      knp_st =  nb_rng1%knp_st 
      knp_end = nb_rng1%knp_end
!
!
      end subroutine s_set_range_4_neighbour
!
! ----------------------------------------------------------------------
!
       subroutine s_set_range_4_nodeloop(kpe)
!
       use m_size_of_cube
!
       implicit none
!
       integer(kind = kint) :: kpe
!
      call set_range_4_nodeloop(kpe, nb_rng1)
!
      i_st =  nb_rng1%i_st 
      i_end = nb_rng1%i_end

      j_st =  nb_rng1%j_st 
      j_end = nb_rng1%j_end

      k_st =  nb_rng1%k_st 
      k_end = nb_rng1%k_end
!
       end subroutine s_set_range_4_nodeloop
!
! ----------------------------------------------------------------------
!
      end module m_neighb_range_cube
