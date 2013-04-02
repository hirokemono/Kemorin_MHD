!cvt_cyl_asym_t_2_xyz_smp.f90
!      module cvt_cyl_asym_t_2_xyz_smp
!
!      Written by H. Matsui on March, 2009
!
!      subroutine cal_xyz_asym_t_by_cyl_smp(np_smp, numnod,             &
!     &          inod_smp_stack, t_cyl, txyz, xx, s, a_s)
!
!      subroutine overwrite_xyz_asym_t_by_cyl_smp(np_smp, numnod,       &
!     &          inod_smp_stack, tensor, xx, s, a_s)
!
!      subroutine cal_xy_asym_t_by_cyl_smp(np_smp, numnod,              &
!     &          inod_smp_stack, t_cyl, v_xy)
!      subroutine cal_zx_asym_t_by_cyl_smp(np_smp, numnod,              &
!     &          inod_smp_stack, t_cyl, v_zx, xx, s, a_s)
!      subroutine cal_yz_asym_t_by_cyl_smp(np_smp, numnod,              &
!     &          inod_smp_stack, t_cyl, v_yz, xx, s, a_s)
!
!   uxuy =         us*up
!   uzux = (as) * (      x* uz*us + y* up*uz)
!   uyuz = (as) * (    - y* uz*us + x* up*uz)
!
      module cvt_cyl_asym_t_2_xyz_smp
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_xyz_asym_t_by_cyl_smp(np_smp, numnod,              &
     &          inod_smp_stack, t_cyl, txyz, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: t_cyl(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: txyz(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tsp, tzs, tpz
!
!
!$omp parallel do private(inod,ist,ied,tsp,tzs,tpz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tsp = t_cyl(inod,1)
           tzs = t_cyl(inod,2)
           tpz = t_cyl(inod,3)
!
           if ( s(inod).eq.0.0 ) then
             txyz(inod,1) =       tsp
             txyz(inod,2) =       tzs
             txyz(inod,3) =       tpz
           else
             txyz(inod,1) =   tsp
!
             txyz(inod,2) = ( tzs * xx(inod,1)                          &
     &                      + tpz * xx(inod,2) ) * a_s(inod)
!
             txyz(inod,3) = (-tzs * xx(inod,2)                          &
     &                      + tpz * xx(inod,1) ) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_xyz_asym_t_by_cyl_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_xyz_asym_t_by_cyl_smp(np_smp, numnod,        &
     &          inod_smp_stack, tensor, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: tensor(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tsp, tzs, tpz
!
!
!$omp parallel do private(inod,ist,ied,tsp,tzs,tpz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tsp = tensor(inod,1)
           tzs = tensor(inod,2)
           tpz = tensor(inod,3)
!
           if ( s(inod).eq.0.0 ) then
             tensor(inod,1) =       tsp
             tensor(inod,2) =       tzs
             tensor(inod,3) =       tpz
           else
             tensor(inod,1) =   tsp
!
             tensor(inod,2) = ( tzs * xx(inod,1)                        &
     &                      + tpz * xx(inod,2) ) * a_s(inod)
!
             tensor(inod,3) = (-tzs * xx(inod,2)                        &
     &                      + tpz * xx(inod,1) ) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine overwrite_xyz_asym_t_by_cyl_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_xy_asym_t_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, t_cyl, v_xy)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: t_cyl(numnod,3)
!
       real(kind=kreal), intent(inout) :: v_xy(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           v_xy(inod) =   t_cyl(inod,1)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_xy_asym_t_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_zx_asym_t_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, t_cyl, v_zx, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: t_cyl(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_zx(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tzs, tpz
!
!
!$omp parallel do private(inod,ist,ied,tzs,tpz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tzs = t_cyl(inod,2)
           tpz = t_cyl(inod,3)
!
           if ( s(inod).eq.0.0 ) then
             v_zx(inod) =       tzs
           else
             v_zx(inod) =   ( tzs * xx(inod,1)                          &
     &                      + tpz * xx(inod,2) ) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_zx_asym_t_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_yz_asym_t_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, t_cyl, v_yz, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: t_cyl(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_yz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tzs, tpz
!
!
!$omp parallel do private(inod,ist,ied,tzs,tpz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tzs = t_cyl(inod,2)
           tpz = t_cyl(inod,3)
!
           if ( s(inod).eq.0.0 ) then
             v_yz(inod) =       tpz
           else
             v_yz(inod) = ( - tzs * xx(inod,2)                          &
     &                      + tpz * xx(inod,1) ) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_yz_asym_t_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_cyl_asym_t_2_xyz_smp
