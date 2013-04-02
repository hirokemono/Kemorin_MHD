!cvt_vector_2_cartecian.f90
!      module cvt_vector_2_cartecian
!
!      Written by H. Matsui on June, 2005
!      Modified by H. Matsui on Oct., 2007
!
!*********************************************************************
!*
!*   convert vector from spherical coordinate to certecian coordinate
!*      vx =  vr*sin(th)*cos(phi) + vt*cos(th)*cos(phi) - vp*sin(phi)
!*      vy =  vr*sin(th)*sin(phi) + vt*cos(th)*sin(phi) + vp*cos(phi)
!*      vz =  vr*cos(th) - vt*sin(th)
!*
!*********************************************************************
!
!      subroutine cvt_vector_2_cart(numnod, vect, v_sph, theta, phi)
!      subroutine cal_x_component(numnod, v_x, v_sph, theta, phi)
!      subroutine cal_y_component(numnod, v_y, v_sph, theta, phi)
!      subroutine cal_z_component(numnod, v_z, v_sph, theta)
!         numnod :: number of node
!         theta :: colatitude ( 0 to phi)
!         phi :: longitude ( 0 to 2\pi)
!         v_sph :: vector on spherical coordinate
!         vect :: obtained vector in certecian coorcinate
!         v_x :: obtained x component
!         v_y :: obtained y component
!         v_z :: obtained z component
!
!      subroutine cvt_one_vector_2_cart(vect, v_sph, theta, phi)
!
!*********************************************************************
!
      module cvt_vector_2_cartecian
!
      use m_precision
!
      use m_constants
      use cvt_sph_vect_2_cart_smp
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
      subroutine cvt_vector_2_cart(numnod, vect, v_sph, theta, phi)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(inout) :: vect(numnod,3)
       real(kind=kreal), intent(in) :: v_sph(numnod,3)
       real(kind=kreal), intent(in) :: theta(numnod)
       real(kind=kreal), intent(in) :: phi(numnod)
!
!
       call cvt_sph_vect_2_xyz_smp(ione, numnod, istack,                &
     &          vect, v_sph, theta, phi)
!
      end subroutine cvt_vector_2_cart
!
! -----------------------------------------------------------------------
!
      subroutine cal_x_component(numnod, v_x, v_sph, theta, phi)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in)    :: v_sph(numnod,3)
       real(kind=kreal), intent(inout) :: v_x(numnod)
       real(kind=kreal), intent(in) :: theta(numnod)
       real(kind=kreal), intent(in) :: phi(numnod)
!
!
       call cal_sph_2_x_comp_smp(ione, numnod, istack,                  &
     &          v_x, v_sph, theta, phi)
!
      end subroutine cal_x_component
!
! -----------------------------------------------------------------------
!
      subroutine cal_y_component(numnod, v_y, v_sph, theta, phi)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in) :: v_sph(numnod,3)
       real(kind=kreal), intent(inout) :: v_y(numnod)
       real(kind=kreal), intent(in) :: theta(numnod), phi(numnod)
!
!
        call cal_sph_2_y_comp_smp(ione, numnod, istack,                 &
     &          v_y, v_sph, theta, phi)
!
      end subroutine cal_y_component
!
! -----------------------------------------------------------------------
!
      subroutine cal_z_component(numnod, v_z, v_sph, theta)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in) :: v_sph(numnod,3)
       real(kind=kreal), intent(in) :: theta(numnod)
       real(kind=kreal), intent(inout) :: v_z(numnod)
!
!
       call cal_sph_2_z_comp_smp(ione, numnod, istack,                  &
     &          v_z, v_sph, theta)
!
      end subroutine cal_z_component
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cvt_one_vector_2_cart(vect, v_sph, theta, phi)
!
       real(kind=kreal), intent(inout) :: vect(3)
       real(kind=kreal), intent(in) :: v_sph(3)
       real(kind=kreal), intent(in) :: theta
       real(kind=kreal), intent(in) :: phi
!
!
        vect(1) = ( v_sph(1)*sin( theta )*cos( phi )                    &
     &            + v_sph(2)*cos( theta )*cos( phi )                    &
     &            - v_sph(3)*sin( phi )   )
!
        vect(2) = ( v_sph(1)*sin( theta )*sin( phi )                    &
     &            + v_sph(2)*cos( theta )*sin( phi )                    &
     &            + v_sph(3)*cos( phi )   )
!
        vect(3) = ( v_sph(1)*cos( theta ) - v_sph(2)*sin( theta ) )
!
      end subroutine cvt_one_vector_2_cart
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      end module cvt_vector_2_cartecian
