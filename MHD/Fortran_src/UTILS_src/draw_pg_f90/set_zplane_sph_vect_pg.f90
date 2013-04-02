!set_zplane_sph_vect_pg.90
!      module set_zplane_sph_vect_pg
!
!      subroutine set_zplane_sph_vector(nnod_pg, xx_psf, vector,        &
!     &          vect_pg, vert_pg)
!      subroutine set_xplane_sph_vector(nnod_pg, xx_psf, vector,        &
!     &          vect_pg, vert_pg)
!      subroutine set_yplane_sph_vector(nnod_pg, xx_psf, vector,        &
!     &          vect_pg, vert_pg)
!
      module set_zplane_sph_vect_pg
!
      use m_precision
!
      use m_constants
      use cvt_vector_2_cartecian
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
!
      subroutine set_zplane_sph_vector(nnod_pg, xx_psf, vector,         &
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
      real(kind = kreal) :: vec_cyl(3), vec_xyz(3), theta, phi, s_rad
!
!
!!$omp parallel do private(vec_cyl,vec_xyz,theta,phi,s_rad)
      do inod = 1, nnod_pg
        vec_cyl(1:3) = vector(inod,1:3)
        s_rad = sqrt(xx_psf(inod,1)**2+xx_psf(inod,2)**2)
        theta = acos(xx_psf(inod,3)/s_rad)
        phi = atan2(xx_psf(inod,2),xx_psf(inod,1))
        call cvt_one_vector_2_cart(vec_xyz, vec_cyl, theta, phi)
!
        vect_pg(1:2,inod) = vec_xyz(1:2)
        vert_pg(inod) =     vec_xyz(3)
      end do
!!$omp end parallel do
!
      end subroutine set_zplane_sph_vector
!
! ----------------------------------------------------------------------
!
      subroutine set_xplane_sph_vector(nnod_pg, xx_psf, vector,         &
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
      real(kind = kreal) :: vec_cyl(3), vec_xyz(3), theta, phi, s_rad
!
!
!!$omp parallel do private(vec_cyl,vec_xyz,theta,phi,s_rad)
      do inod = 1, nnod_pg
        vec_cyl(1:3) = vector(inod,1:3)
        s_rad = sqrt(xx_psf(inod,1)**2+xx_psf(inod,2)**2)
        theta = acos(xx_psf(inod,3)/s_rad)
        phi = atan2(xx_psf(inod,2),xx_psf(inod,1))
        call cvt_one_vector_2_cart(vec_xyz, vec_cyl, theta, phi)
!
        vert_pg(inod) =     vec_xyz(1)
        vect_pg(1:2,inod) = vec_xyz(2:3)
      end do
!!$omp end parallel do
!
      end subroutine set_xplane_sph_vector
!
! ----------------------------------------------------------------------
!
      subroutine set_yplane_sph_vector(nnod_pg, xx_psf, vector,         &
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
      real(kind = kreal) :: vec_cyl(3), vec_xyz(3), theta, phi, s_rad
!
!
!$omp parallel do private(vec_cyl,vec_xyz,theta,phi,s_rad)
      do inod = 1, nnod_pg
        vec_cyl(1:3) = vector(inod,1:3)
        s_rad = sqrt(xx_psf(inod,1)**2+xx_psf(inod,2)**2)
        theta = acos(xx_psf(inod,3)/s_rad)
        phi = atan2(xx_psf(inod,2),xx_psf(inod,1))
        call cvt_one_vector_2_cart(vec_xyz, vec_cyl, theta, phi)
!
        vect_pg(1,inod) = vec_xyz(1)
        vert_pg(inod) =   vec_xyz(2)
        vect_pg(2,inod) = vec_xyz(3)
      end do
!$omp end parallel do
!
      end subroutine set_yplane_sph_vector
!
! ----------------------------------------------------------------------
!
      end module set_zplane_sph_vect_pg
