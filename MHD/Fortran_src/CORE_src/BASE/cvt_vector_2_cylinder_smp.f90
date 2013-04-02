!cvt_vector_2_cylinder_smp.f90
!      module cvt_vector_2_cylinder_smp
!
!      Written by H. Matsui on June, 2005
!
!***********************************************************************
!
!*   convert vector from certecian coordinate to cylindrical coordinate
!*      vs =  vx*cos(phi) + vy*sin(phi)
!*      vp = -vx*sin(phi) + vy*cos(phi)
!*
!***********************************************************************
!
!      subroutine cvt_vector_2_cyl_smp(np_smp, numnod,                  &
!     &          inod_smp_stack, vect, v_cyl, xx, rs, a_rs)
!
!      subroutine overwrite_vector_2_cyl_smp(np_smp, numnod,            &
!     &          inod_smp_stack, vect, xx, rs, a_rs)
!
!      subroutine cal_cylinder_r_comp_smp(np_smp, numnod,               &
!     &          inod_smp_stack, vect, v_s, xx, rs, a_rs)
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
!***********************************************************************
!
      module cvt_vector_2_cylinder_smp
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
      subroutine cvt_vector_2_cyl_smp(np_smp, numnod,                   &
     &          inod_smp_stack, vect, v_cyl, xx, rs, a_rs)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: vect(numnod,3)
       real(kind=kreal), intent(inout) :: v_cyl(numnod,3)
       real(kind=kreal), intent(in)    :: xx(numnod,3)
       real(kind=kreal), intent(in) :: rs(numnod)
       real(kind=kreal), intent(in) :: a_rs(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vx, vy, vz
!
!
!$omp parallel do private(inod,ist,ied,vx,vy,vz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           vx = vect(inod,1)
           vy = vect(inod,2)
           vz = vect(inod,3)
!
           if ( rs(inod).eq.0.0d0 ) then
             v_cyl(inod,1) = vx
             v_cyl(inod,2) = vy
           else
!
             v_cyl(inod,1) = (  vx * xx(inod,1) + vy * xx(inod,2) )     &
     &                        * a_rs(inod)
             v_cyl(inod,2) = ( -vx * xx(inod,2) + vy * xx(inod,1) )     &
     &                        * a_rs(inod)
!
           end if
!
           v_cyl(inod,3) = vz
!
         end do
       end do
!$omp end parallel do
!
      end subroutine cvt_vector_2_cyl_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_vector_2_cyl_smp(np_smp, numnod,             &
     &          inod_smp_stack, vect, xx, rs, a_rs)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: xx(numnod,3)
       real(kind=kreal), intent(in) :: rs(numnod)
       real(kind=kreal), intent(in) :: a_rs(numnod)
!
       real(kind=kreal), intent(inout) :: vect(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vx, vy
!
!
!$omp parallel do private(inod,ist,ied,vx,vy)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           vx = vect(inod,1)
           vy = vect(inod,2)
!
           if ( rs(inod).eq.0.0d0 ) then
             vect(inod,1) = vx
             vect(inod,2) = vy
           else
             vect(inod,1) = (   vx * xx(inod,1) + vy * xx(inod,2) )     &
     &                        * a_rs(inod)
             vect(inod,2) = (  -vx * xx(inod,2) + vy * xx(inod,1) )     &
     &                        * a_rs(inod)
           end if
!
         end do
       end do
!$omp end parallel do
!
      end subroutine overwrite_vector_2_cyl_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_cylinder_r_comp_smp(np_smp, numnod,                &
     &          inod_smp_stack, vect, v_s, xx, rs, a_rs)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: vect(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: rs(numnod)
       real(kind=kreal), intent(in) :: a_rs(numnod)
!
       real(kind=kreal), intent(inout) :: v_s(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vx, vy
!
!
!$omp parallel do private(inod,ist,ied,vx,vy)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           vx = vect(inod,1)
           vy = vect(inod,2)
!
           if ( rs(inod).eq.0.0d0 ) then
             v_s(inod) = vx
           else
             v_s(inod) = ( vx * xx(inod,1) + vy * xx(inod,2) )          &
     &                    * a_rs(inod)
           end if
!
         end do
       end do
!$omp end parallel do
!
      end subroutine cal_cylinder_r_comp_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_vector_2_cylinder_smp
