!set_zplane_xyz_vect_pg.f90
!      module set_zplane_xyz_vect_pg
!
!      subroutine set_zplane_xyz_vector(nnod_pg, vector,                &
!     &          vect_pg, vert_pg)
!      subroutine set_xplane_xyz_vector(nnod_pg, vector,                &
!     &          vect_pg, vert_pg)
!      subroutine set_yplane_xyz_vector(nnod_pg, vector,                &
!     &          vect_pg, vert_pg)
!
      module set_zplane_xyz_vect_pg
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_zplane_xyz_vector(nnod_pg, vector,                 &
     &          vect_pg, vert_pg)
!
      integer(kind = kint), intent(in) :: nnod_pg
      real(kind = kreal), intent(in) :: vector(nnod_pg,3)
!
      real(kind = kreal), intent(inout) :: vect_pg(2,nnod_pg)
      real(kind = kreal), intent(inout) :: vert_pg(nnod_pg)
!
!
      vect_pg(1,1:nnod_pg) = vector(1:nnod_pg,1)
      vect_pg(2,1:nnod_pg) = vector(1:nnod_pg,2)
      vert_pg(1:nnod_pg) =   vector(1:nnod_pg,3)
!
      end subroutine set_zplane_xyz_vector
!
! ----------------------------------------------------------------------
!
      subroutine set_xplane_xyz_vector(nnod_pg, vector,                 &
     &          vect_pg, vert_pg)
!
      integer(kind = kint), intent(in) :: nnod_pg
      real(kind = kreal), intent(in) :: vector(nnod_pg,3)
!
      real(kind = kreal), intent(inout) :: vect_pg(2,nnod_pg)
      real(kind = kreal), intent(inout) :: vert_pg(nnod_pg)
!
!
      vert_pg(1:nnod_pg) =   vector(1:nnod_pg,1)
      vect_pg(1,1:nnod_pg) = vector(1:nnod_pg,2)
      vect_pg(2,1:nnod_pg) = vector(1:nnod_pg,3)
!
      end subroutine set_xplane_xyz_vector
!
! ----------------------------------------------------------------------
!
      subroutine set_yplane_xyz_vector(nnod_pg, vector,                 &
     &          vect_pg, vert_pg)
!
      integer(kind = kint), intent(in) :: nnod_pg
      real(kind = kreal), intent(in) :: vector(nnod_pg,3)
!
      real(kind = kreal), intent(inout) :: vect_pg(2,nnod_pg)
      real(kind = kreal), intent(inout) :: vert_pg(nnod_pg)
!
!
      vect_pg(1,1:nnod_pg) = vector(1:nnod_pg,1)
      vert_pg(1:nnod_pg) =   vector(1:nnod_pg,2)
      vect_pg(2,1:nnod_pg) = vector(1:nnod_pg,3)
!
      end subroutine set_yplane_xyz_vector
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      end module set_zplane_xyz_vect_pg
