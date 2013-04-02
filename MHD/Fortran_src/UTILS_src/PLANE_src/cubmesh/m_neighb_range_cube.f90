!
!     module m_neighb_range_cube
!
      module m_neighb_range_cube
!
!     written by Kemorin
!
      use m_precision
!
      implicit  none
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
      subroutine set_range_4_neighbour(ipe, jpe, kpe)
!
      use m_size_4_plane
      use m_size_of_cube
!
      integer (kind = kint) :: ipe, jpe, kpe
!
!                                       .. search neighbor pe
                            inp_st  = -1
            if (ipe ==   1) inp_st  =  0
                            inp_end =  1
            if (ipe == ndx) inp_end =  0

                            jnp_st  = -1
            if (jpe ==   1) jnp_st  =  0
                            jnp_end =  1
            if (jpe == ndy) jnp_end =  0

                            knp_st  = -1
            if (kpe ==   1) knp_st  =  0
                            knp_end =  1
            if (kpe == ndz) knp_end =  0
!
      end subroutine set_range_4_neighbour
!
! ----------------------------------------------------------------------
!
       subroutine set_range_4_nodeloop(kpe)
!
       use m_size_of_cube
!
       implicit none
!
       integer(kind = kint) :: kpe
!
                            i_st  =     1         + ndepth
                            i_end =  i_st + nxi-1

                            j_st  =     1         + ndepth
                            j_end =  j_st + nyi-1

                            k_st  =     1         + ndepth
            if (kpe ==   1) k_st  =     1
                            k_end =  k_st + nzi-1
!
       end subroutine set_range_4_nodeloop
!
! ----------------------------------------------------------------------
!
      end module m_neighb_range_cube
