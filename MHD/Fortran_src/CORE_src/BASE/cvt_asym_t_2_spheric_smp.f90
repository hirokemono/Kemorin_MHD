!cvt_asym_t_2_spheric_smp.f90
!      module cvt_asym_t_2_spheric_smp
!
!      Written by H. Matsui on March, 2009
!
!      subroutine cal_sph_asym_t_smp(np_smp, numnod, inod_smp_stack,    &
!     &          tensor, tsph, xx, r, s, a_r, a_s)
!
!      subroutine overwrite_sph_asym_t_smp(np_smp, numnod,              &
!     &          inod_smp_stack, tensor, xx, r, s, a_r, a_s)
!
!      subroutine cal_rt_asym_t_smp(np_smp, numnod, inod_smp_stack,     &
!     &          tensor, v_rt, xx, r, s, a_s)
!      subroutine cal_pr_asym_t_smp(np_smp, numnod, inod_smp_stack,     &
!     &          tensor, v_pr, xx, r, s, a_r, a_s)
!      subroutine cal_tp_asym_t_smp(np_smp, numnod, inod_smp_stack,     &
!     &          tensor, v_tp, xx, r, s, a_r)
!
!   urut = (as) *   (              x   *uz*ux - y   *uy*uz)
!   upur = (aras) * (-s*s *ux*uy + y*z *uz*ux + x*z* uy*uz)
!   utup = (ar) *   (   z *ux*uy + y   *uz*ux + x   *uy*uz)
!
      module cvt_asym_t_2_spheric_smp
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
      subroutine cal_sph_asym_t_smp(np_smp, numnod, inod_smp_stack,     &
     &          tensor, tsph, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: tsph(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txy, tzx, tyz
!
!
!$omp parallel do private(inod,ist,ied,txy,tzx,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txy = tensor(inod,1)
           tzx = tensor(inod,2)
           tyz = tensor(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             tsph(inod,1) =   tzx
             tsph(inod,2) =   tyz
             tsph(inod,3) =   txy
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             tsph(inod,1) =   tzx
             tsph(inod,2) =   tyz
             tsph(inod,3) =   txy
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             tsph(inod,1) =   tzx
             tsph(inod,2) = - tyz
             tsph(inod,3) = - txy
           else
             tsph(inod,1) = ( tzx * xx(inod,1)                          &
     &                      - tyz * xx(inod,2) ) * a_s(inod)
!
             tsph(inod,2) = (-txy *  s(inod)  * s(inod)                 &
     &                      + tzx * xx(inod,2)*xx(inod,3)               &
     &                      + tyz * xx(inod,1)*xx(inod,3) )             &
     &                     * a_r(inod) * a_s(inod)
!
             tsph(inod,3) = ( txy * xx(inod,3)                          &
     &                      + tzx * xx(inod,2)                          &
     &                      + tyz * xx(inod,1) ) * a_r(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sph_asym_t_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_sph_asym_t_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: tensor(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txy, tzx, tyz
!
!
!$omp parallel do private(inod,ist,ied,txy,tzx,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txy = tensor(inod,1)
           tzx = tensor(inod,2)
           tyz = tensor(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             tensor(inod,1) =   tzx
             tensor(inod,2) =   tyz
             tensor(inod,3) =   txy
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             tensor(inod,1) =   tzx
             tensor(inod,2) =   tyz
             tensor(inod,3) =   txy
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             tensor(inod,1) =   tzx
             tensor(inod,2) = - tyz
             tensor(inod,3) = - txy
           else
             tensor(inod,1) = ( tzx * xx(inod,1)                        &
     &                        - tyz * xx(inod,2) ) * a_s(inod)
!
             tensor(inod,2) = (-txy *  s(inod)  * s(inod)               &
     &                        + tzx * xx(inod,2)*xx(inod,3)             &
     &                        + tyz * xx(inod,1)*xx(inod,3) )           &
     &                       * a_r(inod) * a_s(inod)
!
             tensor(inod,3) = ( txy * xx(inod,3)                        &
     &                        + tzx * xx(inod,2)                        &
     &                        + tyz * xx(inod,1) ) * a_r(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine overwrite_sph_asym_t_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rt_asym_t_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, v_rt, xx, r, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_rt(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txy, tzx, tyz
!
!
!$omp parallel do private(inod,ist,ied,txy,tzx,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txy = tensor(inod,1)
           tzx = tensor(inod,2)
           tyz = tensor(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             v_rt(inod) =     tzx
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             v_rt(inod) =     tzx
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             v_rt(inod) =     tzx
           else
             v_rt(inod) =   ( tzx * xx(inod,1)                          &
     &                      - tyz * xx(inod,2) ) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_rt_asym_t_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_pr_asym_t_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, v_pr, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_pr(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txy, tzx, tyz
!
!
!$omp parallel do private(inod,ist,ied,txy,tzx,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txy = tensor(inod,1)
           tzx = tensor(inod,2)
           tyz = tensor(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             v_pr(inod) =     tyz
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             v_pr(inod) =     tyz
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             v_pr(inod) =   - tyz
           else
             v_pr(inod) =   (-txy *  s(inod)  * s(inod)                 &
     &                      + tzx * xx(inod,2)*xx(inod,3)               &
     &                      + tyz * xx(inod,1)*xx(inod,3) )             &
     &                     * a_r(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_pr_asym_t_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_tp_asym_t_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, v_tp, xx, r, s, a_r)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
!
       real(kind=kreal), intent(inout) :: v_tp(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txy, tzx, tyz
!
!
!$omp parallel do private(inod,ist,ied,txy,tzx,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txy = tensor(inod,1)
           tzx = tensor(inod,2)
           tyz = tensor(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             v_tp(inod) =     txy
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             v_tp(inod) =     txy
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             v_tp(inod) =   - txy
           else
             v_tp(inod) =   ( txy * xx(inod,3)                          &
     &                      + tzx * xx(inod,2)                          &
     &                      + tyz * xx(inod,1) ) * a_r(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_tp_asym_t_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_asym_t_2_spheric_smp
