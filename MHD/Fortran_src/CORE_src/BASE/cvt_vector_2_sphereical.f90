!cvt_vector_2_sphereical.f90
!      module cvt_vector_2_sphereical
!
!      Written by H. Matsui on June, 2005
!
!***********************************************************************
!*
!*   convert vector from certecian coordinate to spherical coordinate
!*      vr =  vx*sin(th)*cos(phi) + vy*sin(th)*sin(phi) + vz*cos(phi)
!*      vt =  vx*cos(th)*cos(phi) + vy*cos(th)*sin(phi) - vz*sin(phi)
!*      vp = -vx*sin(phi) + vy*cos(phi)
!*
!*   convert vector from certecian coordinate to cylindrical coordinate
!*      vs =  vx*cos(phi) + vy*sin(phi)
!*
!***********************************************************************
!
!      subroutine cvt_vector_2_spherical(numnod, vect, v_sph, xx, r, rs,&
!     &          a_r, a_rs)
!      subroutine cal_radial_component(numnod, vect, v_r, xx, r, a_r)
!      subroutine cal_theta_component(numnod, vect, v_theta, xx, r, rs, &
!     &          a_r, a_rs)
!      subroutine cal_phi_component(numnod, vect, v_phi, xx, rs, a_rs)
!
!      subroutine cvt_vector_2_cylindrical(numnod, vect, v_cyl, xx,     &
!     &          rs, a_rs)
!      subroutine cal_cylinder_r_component(numnod, vect, v_s, xx,       &
!     &          rs, a_rs)
!
!         numnod :: number of node
!         vect :: vector in certecian coorcinate
!
!         v_sph :: obtained vector on spherical coordinate
!         v_r :: obtained radial component
!         v_theta :: obtained meridional component
!         v_phi :: obtained zonal component
!         v_s ::   obtained radial component for cylinder
!
!         xx :: position vector
!         r :: radious
!
      module cvt_vector_2_sphereical
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint), parameter :: istack(0:1) = (/0, 1/)
      private :: istack
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cvt_vector_2_spherical(numnod, vect, v_sph, xx, r, rs, &
     &          a_r, a_rs)
!
      use cvt_vector_2_spheric_smp
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in)    :: vect(numnod,3)
       real(kind=kreal), intent(inout) :: v_sph(numnod,3)
       real(kind=kreal), intent(in)    :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod), rs(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod), a_rs(numnod)
!
       call cvt_vector_2_sph_smp(ione, numnod, istack,                  &
     &     vect, v_sph, xx, r, rs, a_r, a_rs)
!
      end subroutine cvt_vector_2_spherical
!
! -----------------------------------------------------------------------
!
      subroutine cal_radial_component(numnod, vect, v_r, xx, r, a_r)
!
      use cvt_vector_2_spheric_smp
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in)    :: vect(numnod,3)
       real(kind=kreal), intent(inout) :: v_r(numnod)
       real(kind=kreal), intent(in)    :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
!
      call cal_radial_comp_smp(ione, numnod, istack,                    &
     &          vect, v_r, xx, r, a_r)
!
      end subroutine cal_radial_component
!
! -----------------------------------------------------------------------
!
      subroutine cal_theta_component(numnod, vect, v_theta, xx, r, rs,  &
     &          a_r, a_rs)
!
      use cvt_vector_2_spheric_smp
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in)    :: vect(numnod,3)
       real(kind=kreal), intent(inout) :: v_theta(numnod)
       real(kind=kreal), intent(in)    :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod), rs(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod), a_rs(numnod)
!
       call cal_theta_comp_smp(ione, numnod, istack,                    &
     &     vect, v_theta, xx, r, rs, a_r, a_rs)
!
      end subroutine cal_theta_component
!
! -----------------------------------------------------------------------
!
      subroutine cal_phi_component(numnod, vect, v_phi, xx, rs, a_rs)
!
      use cvt_vector_2_spheric_smp
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in)    :: vect(numnod,3)
       real(kind=kreal), intent(inout) :: v_phi(numnod)
       real(kind=kreal), intent(in)    :: xx(numnod,3)
       real(kind=kreal), intent(in) :: rs(numnod)
       real(kind=kreal), intent(in) :: a_rs(numnod)
!
      call cal_phi_comp_smp(ione, numnod, istack,                       &
     &    vect, v_phi, xx, rs, a_rs)
!
      end subroutine cal_phi_component
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cvt_vector_2_cylindrical(numnod, vect, v_cyl, xx,      &
     &          rs, a_rs)
!
      use cvt_vector_2_cylinder_smp
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in)    :: vect(numnod,3)
       real(kind=kreal), intent(inout) :: v_cyl(numnod,3)
       real(kind=kreal), intent(in)    :: xx(numnod,3)
       real(kind=kreal), intent(in) :: rs(numnod)
       real(kind=kreal), intent(in) :: a_rs(numnod)
!
      call cvt_vector_2_cyl_smp(ione, numnod, istack,                   &
     &    vect, v_cyl, xx, rs, a_rs)
!
      end subroutine cvt_vector_2_cylindrical
!
! -----------------------------------------------------------------------
!
      subroutine cal_cylinder_r_component(numnod, vect, v_s, xx,        &
     &          rs, a_rs)
!
      use cvt_vector_2_cylinder_smp
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in)    :: vect(numnod,3)
       real(kind=kreal), intent(inout) :: v_s(numnod)
       real(kind=kreal), intent(in)    :: xx(numnod,3)
       real(kind=kreal), intent(in) :: rs(numnod)
       real(kind=kreal), intent(in) :: a_rs(numnod)
!
      call cal_cylinder_r_comp_smp(ione, numnod, istack,                &
     &    vect, v_s, xx, rs, a_rs)
!
      end subroutine cal_cylinder_r_component
!
! -----------------------------------------------------------------------
!
      end module cvt_vector_2_sphereical
