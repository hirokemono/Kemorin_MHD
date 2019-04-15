!
!     module m_size_4_plane
!
      module m_size_4_plane
!
!     Written by H. Matsui
!
      use m_precision
!
      implicit    none
!
      integer(kind=kint ) :: nx_all, ny_all, nz_all
!  nodal count at x direction line for all model
!  nodal count at y direction line for all model
!  nodal count at z direction line for all model
!
      integer(kind=kint ) :: ndx, ndy, ndz
! subdomain division count at x direction line
! subdomain division count at y direction line
! subdomain division count at z direction line
!
      integer(kind=kint ) :: mesh_type_plane
!
!
      end module m_size_4_plane
