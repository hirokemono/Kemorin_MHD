!
!      module m_neib_range_edge_cube
!
      module m_neib_range_edge_cube
!
      use m_precision
!
      implicit none
!
      integer(kind=kint )  ::  iedge_st, jedge_st, kedge_st
      integer(kind=kint )  ::  iedge_end, jedge_end, kedge_end
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
!
      subroutine init_edge_para_4_each_pe( kpe, ndz )
!
      use m_neighb_range_cube
!
      integer (kind = kint) :: kpe, ndz
!
                            iedge_st  = i_st
                            iedge_end =  i_end

                            jedge_st  = j_st
                            jedge_end =  j_end

                            kedge_st  = k_st
                            kedge_end = k_end
            if (kpe == ndz) kedge_end = k_end - 1

       end subroutine init_edge_para_4_each_pe
!
! ----------------------------------------------------------------------
!
      end module m_neib_range_edge_cube
