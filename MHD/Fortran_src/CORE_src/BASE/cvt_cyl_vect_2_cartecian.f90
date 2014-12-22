!>@file   cvt_cyl_vect_2_cartecian.f90
!!@brief  module cvt_cyl_vect_2_cartecian
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2011
!
!>@brief Convert vector from cylindrical coordinate
!!       to Cartesian coordinate
!!
!!@verbatim
!!*********************************************************************
!!*
!!*   convert vector from spherical coordinate to certecian coordinate
!!*      vx =  vr*sin(th)*cos(phi) + vt*cos(th)*cos(phi) - vp*sin(phi)
!!*      vy =  vr*sin(th)*sin(phi) + vt*cos(th)*sin(phi) + vp*cos(phi)
!!*      vz =  vr*cos(th) - vt*sin(th)
!!*
!!*********************************************************************
!!
!!      subroutine cal_cyl_vect_2_cartecian(numnod, vect, v_cyl, phi)
!!      subroutine cal_cyl_vect_2_x_comp(numnod, v_x, v_cyl, phi)
!!      subroutine cal_cyl_vect_2_y_comp(numnod, v_y, v_cyl, phi)
!!      subroutine cal_cyl_vect_2_z_comp(numnod, v_z, v_cyl)
!!         numnod :: number of node
!!         phi :: longitude ( 0 to 2\pi)
!!         v_cyl :: vector on cyrindrical coordinate
!!         vect :: obtained vector in certecian coorcinate
!!         v_x :: obtained x component
!!         v_y :: obtained y component
!!         v_z :: obtained z component
!!
!!      subroutine cvt_one_cyl_vect_2_cart(vect, v_cyl, phi)
!!
!!*********************************************************************
!!@endverbatim
!
      module cvt_cyl_vect_2_cartecian
!
      use m_precision
!
      use m_constants
      use cvt_cyl_vector_2_xyz_smp
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
      subroutine cal_cyl_vect_2_cartecian(numnod, vect, v_cyl, phi)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(inout) :: vect(numnod,3)
       real(kind=kreal), intent(in) :: v_cyl(numnod,3)
       real(kind=kreal), intent(in) :: phi(numnod)
!
!
!$omp parallel
       call cvt_cyl_vect_2_xyz_smp(ione, numnod, istack,                &
     &     vect, v_cyl, phi)
!$omp end parallel
!
      end subroutine cal_cyl_vect_2_cartecian
!
! -----------------------------------------------------------------------
!
      subroutine cal_cyl_vect_2_x_comp(numnod, v_x, v_cyl, phi)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in)    :: v_cyl(numnod,3)
       real(kind=kreal), intent(inout) :: v_x(numnod)
       real(kind=kreal), intent(in) :: phi(numnod)
!
!
!$omp parallel
       call cvt_cyl_vect_2_x_comp_smp(ione, numnod, istack,             &
     &     v_x, v_cyl, phi)
!$omp end parallel
!
      end subroutine cal_cyl_vect_2_x_comp
!
! -----------------------------------------------------------------------
!
      subroutine cal_cyl_vect_2_y_comp(numnod, v_y, v_cyl, phi)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in) :: v_cyl(numnod,3)
       real(kind=kreal), intent(inout) :: v_y(numnod)
       real(kind=kreal), intent(in) :: phi(numnod)
!
!
!$omp parallel
      call cvt_cyl_vect_2_y_comp_smp(ione, numnod, istack,              &
     &    v_y, v_cyl, phi)
!$omp end parallel
!
      end subroutine cal_cyl_vect_2_y_comp
!
! -----------------------------------------------------------------------
!
      subroutine cal_cyl_vect_2_z_comp(numnod, v_z, v_cyl)
!
      use copy_field_smp
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in) :: v_cyl(numnod,3)
       real(kind=kreal), intent(inout) :: v_z(numnod)
!
!
!$omp parallel
      call copy_nod_scalar_smp(ione, numnod, istack, v_cyl(1,3), v_z)
!$omp end parallel
!
      end subroutine cal_cyl_vect_2_z_comp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cvt_one_cyl_vect_2_cart(vect, v_cyl, phi)
!
       real(kind=kreal), intent(inout) :: vect(3)
       real(kind=kreal), intent(in) :: v_cyl(3)
       real(kind=kreal), intent(in) :: phi
!
!
        vect(1) = ( v_cyl(1)*cos( phi ) - v_cyl(2)*sin( phi ) )
        vect(2) = ( v_cyl(1)*sin( phi ) + v_cyl(2)*cos( phi ) )
        vect(3) = v_cyl(3)
!
      end subroutine cvt_one_cyl_vect_2_cart
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      end module cvt_cyl_vect_2_cartecian
