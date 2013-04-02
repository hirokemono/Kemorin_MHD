!cvt_cyl_vect_2_cart_smp.f90
!      module cvt_cyl_vect_2_cart_smp
!
!      Written by H. Matsui on Oct., 2007
!
!*********************************************************************
!*
!*   convert vector from spherical coordinate to certecian coordinate
!*      vx =  vs*cos(phi) - vp*sin(phi)
!*      vy =  vs*sin(phi) + vp*cos(phi)
!*      vz =  vz
!*
!*********************************************************************
!
!      subroutine cvt_cyl_vect_2_xyz_smp(np_smp, numnod, inod_smp_stack,&
!     &          vect, v_cyl, phi)
!
!      subroutine overwrite_cyl_vect_2_xyz_smp(np_smp, numnod,          &
!     &          inod_smp_stack, vect, phi)
!
!      subroutine cvt_cyl_vect_2_x_comp_smp(np_smp, numnod,             &
!     &          inod_smp_stack, v_x, v_cyl, phi)
!      subroutine cvt_cyl_vect_2_y_comp_smp(np_smp, numnod,             &
!     &          inod_smp_stack, v_y, v_cyl, phi)
!      subroutine cvt_cyl_vect_2_z_comp_smp(np_smp, numnod,             &
!     &          inod_smp_stack, v_z, v_cyl)
!         numnod :: number of node
!         phi :: longitude ( 0 to 2\pi)
!         v_cyl :: vector on cylindrical coordinate
!         vect :: obtained vector in certecian coorcinate
!         v_x :: obtained x component
!         v_y :: obtained y component
!         v_z :: obtained z component
!
!*********************************************************************
!
      module cvt_cyl_vect_2_cart_smp
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cvt_cyl_vect_2_xyz_smp(np_smp, numnod, inod_smp_stack, &
     &          vect, v_cyl, phi)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(inout) :: vect(numnod,3)
       real(kind=kreal), intent(in) :: v_cyl(numnod,3)
       real(kind=kreal), intent(in) :: phi(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vs, vp
!
!
!$omp parallel do private(inod,ist,ied,vs,vp)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          vs = v_cyl(inod,1)
          vp = v_cyl(inod,2)
!
          vect(inod,1) = ( vs * cos( phi(inod) )                        &
     &                   - vp * sin( phi(inod) ) )
          vect(inod,2) = ( vs * sin( phi(inod) )                        &
     &                   + vp * cos( phi(inod) ) )
          vect(inod,3) = v_cyl(inod,3)
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cvt_cyl_vect_2_xyz_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_cyl_vect_2_xyz_smp(np_smp, numnod,           &
     &          inod_smp_stack, vect, phi)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: phi(numnod)
!
       real(kind=kreal), intent(inout) :: vect(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vs, vp
!
!
!$omp parallel do private(inod,ist,ied,vs,vp)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          vs = vect(inod,1)
          vp = vect(inod,2)
!
          vect(inod,1) = ( vs * cos( phi(inod) )                        &
     &                   - vp * sin( phi(inod) ) )
          vect(inod,2) = ( vs * sin( phi(inod) )                        &
     &                   + vp * cos( phi(inod) ) )
        end do
      end do
!$omp end parallel do
!
      end subroutine overwrite_cyl_vect_2_xyz_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cvt_cyl_vect_2_x_comp_smp(np_smp, numnod,              &
     &          inod_smp_stack, v_x, v_cyl, phi)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: v_cyl(numnod,2)
       real(kind=kreal), intent(inout) :: v_x(numnod)
       real(kind=kreal), intent(in) :: phi(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vs, vp
!
!
!$omp parallel do private(inod,ist,ied,vs,vp)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          vs = v_cyl(inod,1)
          vp = v_cyl(inod,2)
!
          v_x(inod) = ( vs * cos( phi(inod) )                           &
     &                - vp * sin( phi(inod) ) )
        end do
      end do
!$omp end parallel do
!
      end subroutine cvt_cyl_vect_2_x_comp_smp
!
! -----------------------------------------------------------------------
!
      subroutine cvt_cyl_vect_2_y_comp_smp(np_smp, numnod,              &
     &          inod_smp_stack, v_y, v_cyl, phi)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: v_cyl(numnod,3)
       real(kind=kreal), intent(inout) :: v_y(numnod)
       real(kind=kreal), intent(in) :: phi(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vs, vp
!
!
!$omp parallel do private(inod,ist,ied,vs,vp)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          vs = v_cyl(inod,1)
          vp = v_cyl(inod,2)
!
          v_y(inod) = ( vs * sin( phi(inod) )                           &
     &                + vp * cos( phi(inod) ) )
        end do
      end do
!$omp end parallel do
!
      end subroutine cvt_cyl_vect_2_y_comp_smp
!
! -----------------------------------------------------------------------
!
      subroutine cvt_cyl_vect_2_z_comp_smp(np_smp, numnod,              &
     &          inod_smp_stack, v_z, v_cyl)
!
      use copy_field_smp
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: v_cyl(numnod,3)
       real(kind=kreal), intent(inout) :: v_z(numnod)
!
!
      call copy_nod_scalar_smp(np_smp, numnod, inod_smp_stack,          &
     &    v_cyl(1,3), v_z)
!
      end subroutine cvt_cyl_vect_2_z_comp_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_cyl_vect_2_cart_smp
