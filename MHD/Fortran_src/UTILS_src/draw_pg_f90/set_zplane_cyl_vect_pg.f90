!set_zplane_cyl_vect_pg.f90
!      module set_zplane_cyl_vect_pg
!
!      subroutine set_zplane_cyl_vector(nnod_pg, xx_psf, vector,        &
!     &          vect_pg, vert_pg)
!      subroutine set_xplane_cyl_vector(nnod_pg, xx_psf, vector,        &
!     &          vect_pg, vert_pg)
!      subroutine set_yplane_cyl_vector(nnod_pg, xx_psf, vector,        &
!     &          vect_pg, vert_pg)
!
      module set_zplane_cyl_vect_pg
!
      use m_precision
!
      use m_constants
      use cvt_cyl_vect_2_cartecian
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_zplane_cyl_vector(nnod_pg, xx_psf, vector,         &
     &          vect_pg, vert_pg)
!
      integer(kind = kint), intent(in) :: nnod_pg
      real(kind = kreal), intent(in) :: xx_psf(nnod_pg,3)
      real(kind = kreal), intent(in) :: vector(nnod_pg,3)
!
      real(kind = kreal), intent(inout) :: vect_pg(2,nnod_pg)
      real(kind = kreal), intent(inout) :: vert_pg(nnod_pg)
!
      integer(kind = kint) :: inod
      real(kind = kreal) :: vec_cyl(3), vec_xyz(3), phi
!
!
!!$omp parallel do private(vec_cyl,vec_xyz,phi)
      do inod = 1, nnod_pg
        vec_cyl(1:3) = vector(inod,1:3)
        phi = atan2(xx_psf(inod,2),xx_psf(inod,1))
        call cvt_one_cyl_vect_2_cart(vec_xyz, vec_cyl, phi)
!
        vect_pg(1:2,inod) = vec_xyz(1:2)
        vert_pg(inod) =     vec_xyz(3)
      end do
!!$omp end parallel do
!
      end subroutine set_zplane_cyl_vector
!
! ----------------------------------------------------------------------
!
      subroutine set_xplane_cyl_vector(nnod_pg, xx_psf, vector,         &
     &          vect_pg, vert_pg)
!
      integer(kind = kint), intent(in) :: nnod_pg
      real(kind = kreal), intent(in) :: xx_psf(nnod_pg,3)
      real(kind = kreal), intent(in) :: vector(nnod_pg,3)
!
      real(kind = kreal), intent(inout) :: vect_pg(2,nnod_pg)
      real(kind = kreal), intent(inout) :: vert_pg(nnod_pg)
!
      integer(kind = kint) :: inod
      real(kind = kreal) :: vec_cyl(3), vec_xyz(3), phi
!
!
!!$omp parallel do private(vec_cyl,vec_xyz,phi)
      do inod = 1, nnod_pg
        vec_cyl(1:3) = vector(inod,1:3)
        phi = atan2(xx_psf(inod,2),xx_psf(inod,1))
        call cvt_one_cyl_vect_2_cart(vec_xyz, vec_cyl, phi)
!
        vert_pg(inod) =     vec_xyz(1)
        vect_pg(1:2,inod) = vec_xyz(2:3)
      end do
!!$omp end parallel do
!
      end subroutine set_xplane_cyl_vector
!
! ----------------------------------------------------------------------
!
      subroutine set_yplane_cyl_vector(nnod_pg, xx_psf, vector,         &
     &          vect_pg, vert_pg)
!
      integer(kind = kint), intent(in) :: nnod_pg
      real(kind = kreal), intent(in) :: xx_psf(nnod_pg,3)
      real(kind = kreal), intent(in) :: vector(nnod_pg,3)
!
      real(kind = kreal), intent(inout) :: vect_pg(2,nnod_pg)
      real(kind = kreal), intent(inout) :: vert_pg(nnod_pg)
!
      integer(kind = kint) :: inod
      real(kind = kreal) :: vec_cyl(3), vec_xyz(3), phi
!
!
!!$omp parallel do private(vec_cyl,vec_xyz,phi)
      do inod = 1, nnod_pg
        vec_cyl(1:3) = vector(inod,1:3)
        phi = atan2(xx_psf(inod,2),xx_psf(inod,1))
        call cvt_one_cyl_vect_2_cart(vec_xyz, vec_cyl, phi)
!
        vect_pg(1,inod) = vec_xyz(1)
        vert_pg(inod) =   vec_xyz(2)
        vect_pg(2,inod) = vec_xyz(3)
      end do
!!$omp end parallel do
!
      end subroutine set_yplane_cyl_vector
!
! ----------------------------------------------------------------------
!
      end module set_zplane_cyl_vect_pg
