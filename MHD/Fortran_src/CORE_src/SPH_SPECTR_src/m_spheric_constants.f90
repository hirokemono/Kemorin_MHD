!
!      module m_spheric_constants
!
!        programmed by H.Matsui on July, 2007
!
!        igrid_Chebyshev =    2 :: Chebyshev collocation points
!        igrid_non_euqidist = 1 :: non-equi-distance
!        igrid_euqidistance = 0 :: equi-distance
!
!       spherical mesh type  -1: none, 0: Gauss points only
!                             1: include poles 2: include center
!
      module m_spheric_constants
!
      use m_precision
!
      integer(kind = kint), parameter :: igrid_Chebyshev =    2
      integer(kind = kint), parameter :: igrid_non_euqidist = 1
      integer(kind = kint), parameter :: igrid_euqidistance = 0
!
      integer(kind = kint), parameter :: iflag_no_FEMMESH =   -1
      integer(kind = kint), parameter :: iflag_MESH_same =     0
      integer(kind = kint), parameter :: iflag_MESH_w_pole =   1
      integer(kind = kint), parameter :: iflag_MESH_w_center = 2
!
      end module m_spheric_constants
