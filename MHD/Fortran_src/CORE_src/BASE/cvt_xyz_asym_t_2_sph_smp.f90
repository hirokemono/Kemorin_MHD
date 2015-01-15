!>@file   cvt_xyz_asym_t_2_sph_smp.f90
!!@brief  module cvt_xyz_asym_t_2_sph_smp
!!
!!@author H. Matsui
!!@date Programmed in March, 2009
!
!>@brief Convert anti-symmetric tensor from Cartesian coordinate
!!       to spherical coordinate
!!
!!@verbatim
!!      subroutine cal_sph_asym_t_smp(np_smp, numnod, inod_smp_stack,   &
!!     &          tensor, tsph, xx, yy, zz, r, s, a_r, a_s)
!!
!!      subroutine overwrite_sph_asym_t_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, xx, yy, zz, r, s, a_r, a_s)
!!
!!      subroutine cal_rt_asym_t_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_rt, xx, yy, zz, r, s, a_s)
!!      subroutine cal_pr_asym_t_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_pr, xx, yy, zz, r, s, a_r, a_s)
!!      subroutine cal_tp_asym_t_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_tp, xx, yy, zz, r, s, a_r)
!!
!!   urut = (as) *   (              x   *uz*ux - y   *uy*uz)
!!   upur = (aras) * (-s*s *ux*uy + y*z *uz*ux + x*z* uy*uz)
!!   utup = (ar) *   (   z *ux*uy + y   *uz*ux + x   *uy*uz)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  numnod   Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!@n @param  xx(numnod) position in Cartesian coordinate
!!@n @param  yy(numnod) position in Cartesian coordinate
!!@n @param  zz(numnod) position in Cartesian coordinate
!!@n @param  r(numnod)    radius
!!@n @param  s(numnod)    cylindrical radius
!!@n @param  a_r(numnod)  1 / r
!!@n @param  a_s(numnod)  1 / s
!!
!!@n @param  tensor(numnod,3)
!!                    anti-symmetric tensor in Cartesian coordinate
!!@n @param  tsph(numnod,3)
!!                    anti-symmetric tensor in spherical coordinate
!!@n @param  t_rt(numnod)
!!                    @f$ T_{r\theta} @f$ in spherical coordinate
!!@n @param  t_pr(numnod)
!!                    @f$ T_{\phi r} @f$  in spherical coordinate
!!@n @param  t_tp(numnod)
!!                     @f$ T_{\theta \phi} @f$ in spherical coordinate
!
      module cvt_xyz_asym_t_2_sph_smp
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
     &          tensor, tsph, xx, yy, zz, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
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
!$omp do private(inod,ist,ied,txy,tzx,tyz)
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
           else if ( s(inod).eq.0.0 .and. zz(inod) .gt. 0) then
             tsph(inod,1) =   tzx
             tsph(inod,2) =   tyz
             tsph(inod,3) =   txy
           else if ( s(inod).eq.0.0 .and. zz(inod) .lt. 0) then
             tsph(inod,1) =   tzx
             tsph(inod,2) = - tyz
             tsph(inod,3) = - txy
           else
             tsph(inod,1) = ( tzx * xx(inod)                            &
     &                      - tyz * yy(inod) ) * a_s(inod)
!
             tsph(inod,2) = (-txy *  s(inod)  * s(inod)                 &
     &                      + tzx * yy(inod)*zz(inod)                   &
     &                      + tyz * xx(inod)*zz(inod) )                 &
     &                     * a_r(inod) * a_s(inod)
!
             tsph(inod,3) = ( txy * zz(inod)                            &
     &                      + tzx * yy(inod)                            &
     &                      + tyz * xx(inod) ) * a_r(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_sph_asym_t_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_sph_asym_t_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, xx, yy, zz, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
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
!$omp do private(inod,ist,ied,txy,tzx,tyz)
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
           else if ( s(inod).eq.0.0 .and. zz(inod) .gt. 0) then
             tensor(inod,1) =   tzx
             tensor(inod,2) =   tyz
             tensor(inod,3) =   txy
           else if ( s(inod).eq.0.0 .and. zz(inod) .lt. 0) then
             tensor(inod,1) =   tzx
             tensor(inod,2) = - tyz
             tensor(inod,3) = - txy
           else
             tensor(inod,1) = ( tzx * xx(inod)                          &
     &                        - tyz * yy(inod) ) * a_s(inod)
!
             tensor(inod,2) = (-txy *  s(inod)  * s(inod)               &
     &                        + tzx * yy(inod)*zz(inod)                 &
     &                        + tyz * xx(inod)*zz(inod) )               &
     &                       * a_r(inod) * a_s(inod)
!
             tensor(inod,3) = ( txy * zz(inod)                          &
     &                        + tzx * yy(inod)                          &
     &                        + tyz * xx(inod) ) * a_r(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine overwrite_sph_asym_t_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rt_asym_t_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_rt, xx, yy, zz, r, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_rt(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txy, tzx, tyz
!
!
!$omp do private(inod,ist,ied,txy,tzx,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txy = tensor(inod,1)
           tzx = tensor(inod,2)
           tyz = tensor(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             t_rt(inod) =     tzx
           else if ( s(inod).eq.0.0 .and. zz(inod) .gt. 0) then
             t_rt(inod) =     tzx
           else if ( s(inod).eq.0.0 .and. zz(inod) .lt. 0) then
             t_rt(inod) =     tzx
           else
             t_rt(inod) =   ( tzx * xx(inod)                            &
     &                      - tyz * yy(inod) ) * a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_rt_asym_t_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_pr_asym_t_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_pr, xx, yy, zz, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_pr(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txy, tzx, tyz
!
!
!$omp do private(inod,ist,ied,txy,tzx,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txy = tensor(inod,1)
           tzx = tensor(inod,2)
           tyz = tensor(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             t_pr(inod) =     tyz
           else if ( s(inod).eq.0.0 .and. zz(inod) .gt. 0) then
             t_pr(inod) =     tyz
           else if ( s(inod).eq.0.0 .and. zz(inod) .lt. 0) then
             t_pr(inod) =   - tyz
           else
             t_pr(inod) =   (-txy *  s(inod)  * s(inod)                 &
     &                      + tzx * yy(inod)*zz(inod)                   &
     &                      + tyz * xx(inod)*zz(inod) )                 &
     &                     * a_r(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_pr_asym_t_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_tp_asym_t_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_tp, xx, yy, zz, r, s, a_r)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
!
       real(kind=kreal), intent(inout) :: t_tp(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txy, tzx, tyz
!
!
!$omp do private(inod,ist,ied,txy,tzx,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txy = tensor(inod,1)
           tzx = tensor(inod,2)
           tyz = tensor(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             t_tp(inod) =     txy
           else if ( s(inod).eq.0.0 .and. zz(inod) .gt. 0) then
             t_tp(inod) =     txy
           else if ( s(inod).eq.0.0 .and. zz(inod) .lt. 0) then
             t_tp(inod) =   - txy
           else
             t_tp(inod) =   ( txy * zz(inod)                            &
     &                      + tzx * yy(inod)                            &
     &                      + tyz * xx(inod) ) * a_r(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_tp_asym_t_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_xyz_asym_t_2_sph_smp
