!cvt_cyl_tensor_2_xyz_smp.f90
!      module cvt_cyl_tensor_2_xyz_smp
!
!      Written by H. Matsui on March, 2009
!
!
!      subroutine cal_xyz_tensor_by_cyl_smp(np_smp, numnod,             &
!     &          inod_smp_stack, tensor, txyz, xx, s, a_s)
!
!      subroutine overwrite_xyz_tensor_by_cyl_smp(np_smp, numnod,       &
!     &          inod_smp_stack, tensor, xx, s, a_s)
!
!      subroutine cal_xx_tensor_by_cyl_smp(np_smp, numnod,              &
!     &          inod_smp_stack, tensor, txyz, xx, s, a_s)
!      subroutine cal_xy_tensor_by_cyl_smp(np_smp, numnod,              &
!     &          inod_smp_stack, tensor, txyz, xx, s, a_s)
!      subroutine cal_xz_tensor_by_cyl_smp(np_smp, numnod,              &
!     &          inod_smp_stack, tensor, txyz, xx, s, a_s)
!      subroutine cal_yy_tensor_by_cyl_smp(np_smp, numnod,              &
!     &          inod_smp_stack, tensor, txyz, xx, s, a_s)
!      subroutine cal_yz_tensor_by_cyl_smp(np_smp, numnod,              &
!     &          inod_smp_stack, tensor, txyz, xx, s, a_s)
!      subroutine cal_zz_tensor_by_cyl_smp(np_smp, numnod,              &
!     &          inod_smp_stack, tensor, txyz)
!
!   uxux = (as)^2 *(x*x *us*us - x*y *us*up
!                 - x*y *us*up + y*y *up*up)
!   uyuy = (as)^2 *(x*y *us*us + x*x *us*up
!                 - y*y *us*up + x*x *up*up)
!   uxuz = (as)^2 *(x*s *us*uz - y*s *up*uz)
!   uyuy = (as)^2 *(y*y *us*us + x*y *us*up
!                 + x*y *us*up + x*x *up*up)
!   uyuz = (as)^2 *(y*s *us*uz + x*s *up*uz)
!   uzuz = (as)^2 * s*s* uz*uz
!
!
      module cvt_cyl_tensor_2_xyz_smp
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
      subroutine cal_xyz_tensor_by_cyl_smp(np_smp, numnod,              &
     &          inod_smp_stack, tensor, txyz, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: txyz(numnod,6)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tss, tsp, tsz, tpp, tpz, tzz
!
!
!$omp parallel do private(inod,ist,ied,tss,tsp,tsz,tpp,tpz,tzz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tss = tensor(inod,1)
           tsp = tensor(inod,2)
           tsz = tensor(inod,3)
           tpp = tensor(inod,4)
           tpz = tensor(inod,5)
           tzz = tensor(inod,6)
!
           if ( s(inod).eq.0.0 ) then
             txyz(inod,1) =   tss
             txyz(inod,2) =   tsp
             txyz(inod,3) =   tsz
             txyz(inod,4) =   tpp
             txyz(inod,5) =   tpz
             txyz(inod,6) =   tzz
           else
             txyz(inod,1) = ( tss * xx(inod,1)*xx(inod,1)               &
     &                  - two*tsp * xx(inod,1)*xx(inod,2)               &
     &                  +     tpp * xx(inod,2)*xx(inod,2) )             &
     &                   * a_s(inod)*a_s(inod)
!
             txyz(inod,2) = ( tss * xx(inod,1)*xx(inod,2)               &
     &                  +     tsp *(xx(inod,1)*xx(inod,1)               &
     &                            - xx(inod,2)*xx(inod,2))              &
     &                  +     tpp * xx(inod,1)*xx(inod,1) )             &
     &                   * a_s(inod)*a_s(inod)
!
             txyz(inod,3) = ( tsz * xx(inod,1)                          &
     &                  -     tpz * xx(inod,2) )                        &
     &                   * a_s(inod)
!
             txyz(inod,4) = ( tss * xx(inod,2)*xx(inod,2)               &
     &                  + two*tsp * xx(inod,1)*xx(inod,2)               &
     &                  +     tpp * xx(inod,1)*xx(inod,1) )             &
     &                   * a_s(inod)*a_s(inod)
!
             txyz(inod,5) = ( tsz * xx(inod,2)                          &
     &                  +     tpz * xx(inod,1) )                        &
     &                   * a_s(inod)
!
             txyz(inod,6) = tzz
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_xyz_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_xyz_tensor_by_cyl_smp(np_smp, numnod,        &
     &          inod_smp_stack, tensor, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: tensor(numnod,6)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tss, tsp, tsz, tpp, tpz, tzz
!
!
!$omp parallel do private(inod,ist,ied,tss,tsp,tsz,tpp,tpz,tzz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tss = tensor(inod,1)
           tsp = tensor(inod,2)
           tsz = tensor(inod,3)
           tpp = tensor(inod,4)
           tpz = tensor(inod,5)
           tzz = tensor(inod,6)
!
           if ( s(inod).eq.0.0 ) then
             tensor(inod,1) =   tss
             tensor(inod,2) =   tsp
             tensor(inod,3) =   tsz
             tensor(inod,4) =   tpp
             tensor(inod,5) =   tpz
             tensor(inod,6) =   tzz
           else
             tensor(inod,1) = ( tss * xx(inod,1)*xx(inod,1)             &
     &                    - two*tsp * xx(inod,1)*xx(inod,2)             &
     &                    +     tpp * xx(inod,2)*xx(inod,2) )           &
     &                     * a_s(inod)*a_s(inod)
!
             tensor(inod,2) = ( tss * xx(inod,1)*xx(inod,2)             &
     &                    +     tsp *(xx(inod,1)*xx(inod,1)             &
     &                              - xx(inod,2)*xx(inod,2))            &
     &                    +     tpp * xx(inod,1)*xx(inod,1) )           &
     &                     * a_s(inod)*a_s(inod)
!
             tensor(inod,3) = ( tsz * xx(inod,1)                        &
     &                    -     tpz * xx(inod,2) )                      &
     &                     * a_s(inod)
!
             tensor(inod,4) = ( tss * xx(inod,2)*xx(inod,2)             &
     &                    + two*tsp * xx(inod,1)*xx(inod,2)             &
     &                    +     tpp * xx(inod,1)*xx(inod,1) )           &
     &                     * a_s(inod)*a_s(inod)
!
             tensor(inod,5) = ( tsz * xx(inod,2)                        &
     &                    +     tpz * xx(inod,1) )                      &
     &                     * a_s(inod)
!
             tensor(inod,6) = tzz
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine overwrite_xyz_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_xx_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, txyz, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: txyz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tss, tsp, tpp
!
!
!$omp parallel do private(inod,ist,ied,tss,tsp,tpp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tss = tensor(inod,1)
           tsp = tensor(inod,2)
           tpp = tensor(inod,4)
!
           if ( s(inod).eq.0.0 ) then
             txyz(inod) =   tss
           else
             txyz(inod) =   ( tss * xx(inod,1)*xx(inod,1)               &
     &                  - two*tsp * xx(inod,1)*xx(inod,2)               &
     &                  +     tpp * xx(inod,2)*xx(inod,2) )             &
     &                   * a_s(inod)*a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_xx_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_xy_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, txyz, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: txyz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tss, tsp, tpp
!
!
!$omp parallel do private(inod,ist,ied,tss,tsp,tpp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tss = tensor(inod,1)
           tsp = tensor(inod,2)
           tpp = tensor(inod,4)
!
           if ( s(inod).eq.0.0 ) then
             txyz(inod) =   tsp
           else
             txyz(inod) =   ( tss * xx(inod,1)*xx(inod,2)               &
     &                  +     tsp *(xx(inod,1)*xx(inod,1)               &
     &                            - xx(inod,2)*xx(inod,2))              &
     &                  +     tpp * xx(inod,1)*xx(inod,1) )             &
     &                   * a_s(inod)*a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_xy_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_xz_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, txyz, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: txyz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tsz, tpz
!
!
!$omp parallel do private(inod,ist,ied,tsz,tpz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tsz = tensor(inod,3)
           tpz = tensor(inod,5)
!
           if ( s(inod).eq.0.0 ) then
             txyz(inod) =   tsz
           else
             txyz(inod) =   ( tsz * xx(inod,1)                          &
     &                  -     tpz * xx(inod,2) )                        &
     &                   * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_xz_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_yy_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, txyz, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: txyz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tss, tsp, tpp
!
!
!$omp parallel do private(inod,ist,ied,tss,tsp,tpp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tss = tensor(inod,1)
           tsp = tensor(inod,2)
           tpp = tensor(inod,4)
!
           if ( s(inod).eq.0.0 ) then
             txyz(inod) =   tpp
           else
             txyz(inod) =   ( tss * xx(inod,2)*xx(inod,2)               &
     &                  + two*tsp * xx(inod,1)*xx(inod,2)               &
     &                  +     tpp * xx(inod,1)*xx(inod,1) )             &
     &                   * a_s(inod)*a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_yy_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_yz_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, txyz, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: txyz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tsz, tpz
!
!
!$omp parallel do private(inod,ist,ied,tsz,tpz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tsz = tensor(inod,3)
           tpz = tensor(inod,5)
!
           if ( s(inod).eq.0.0 ) then
             txyz(inod) =   tpz
           else
             txyz(inod) =   ( tsz * xx(inod,2)                          &
     &                  +     tpz * xx(inod,1) )                        &
     &                   * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_yz_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_zz_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, txyz)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
!
       real(kind=kreal), intent(inout) :: txyz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txyz(inod) = tensor(inod,6)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_zz_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_cyl_tensor_2_xyz_smp
