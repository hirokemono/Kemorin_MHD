!cvt_tensor_2_cylinder_smp.f90
!      module cvt_tensor_2_cylinder_smp
!
!      Written by H. Matsui on March, 2009
!
!      subroutine cal_cyl_tensor_smp(np_smp, numnod, inod_smp_stack,    &
!     &          tensor, tcyl, xx, s, a_s)
!
!      subroutine overwrite_cyl_tensor_smp(np_smp, numnod,              &
!     &          inod_smp_stack, tensor, xx, s, a_s)
!
!      subroutine cal_ss_tensor_smp(np_smp, numnod, inod_smp_stack,     &
!     &          tensor, v_ss, xx, s, a_s)
!      subroutine cal_sp_tensor_smp(np_smp, numnod, inod_smp_stack,     &
!     &          tensor, v_sp, xx, s, a_s)
!      subroutine cal_sz_tensor_smp(np_smp, numnod, inod_smp_stack,     &
!     &          tensor, v_sz, xx, s, a_s)
!      subroutine cal_pp_cyl_tensor_smp(np_smp, numnod, inod_smp_stack, &
!     &          tensor, v_pp, xx, s, a_s)
!      subroutine cal_pz_tensor_smp(np_smp, numnod, inod_smp_stack,     &
!     &          tensor, v_pz, xx, s, a_s)
!      subroutine cal_zz_tensor_smp(np_smp, numnod, inod_smp_stack,     &
!     &          tensor, v_zz)
!
      module cvt_tensor_2_cylinder_smp
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
      subroutine cal_cyl_tensor_smp(np_smp, numnod, inod_smp_stack,     &
     &          tensor, tcyl, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: tcyl(numnod,6)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txx, txy, txz, tyy, tyz, tzz
!
!
!$omp parallel do private(inod,ist,ied,txx,txy,txz,tyy,tyz,tzz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           txz = tensor(inod,3)
           tyy = tensor(inod,4)
           tyz = tensor(inod,5)
           tzz = tensor(inod,6)
!
           if ( s(inod).eq.0.0 ) then
             tcyl(inod,1) =       tensor(inod,1)
             tcyl(inod,2) =       tensor(inod,2)
             tcyl(inod,3) =       tensor(inod,3)
             tcyl(inod,4) =       tensor(inod,4)
             tcyl(inod,5) =       tensor(inod,5)
             tcyl(inod,6) =       tensor(inod,6)
           else
             tcyl(inod,1) =   (  txx * xx(inod,1)*xx(inod,1)            &
     &                     + two*txy * xx(inod,1)*xx(inod,2)            &
     &                     +     tyy * xx(inod,2)*xx(inod,2) )          &
     &                      * a_s(inod) * a_s(inod)
!
             tcyl(inod,2) =   ( -txx * xx(inod,1)*xx(inod,2)            &
     &                     +     txy * (xx(inod,1)*xx(inod,1)           &
     &                                - xx(inod,2)*xx(inod,2) )         &
     &                     +     tyy*xx(inod,1)*xx(inod,2) )            &
     &                      * a_s(inod) * a_s(inod)
!
             tcyl(inod,3) =   (  txz * xx(inod,1)                       &
     &                     +     tyz * xx(inod,2) ) * a_s(inod)
!
             tcyl(inod,4) =   (  txx * xx(inod,2)*xx(inod,2)            &
     &                     - two*txy * xx(inod,1)*xx(inod,2)            &
     &                     +     tyy * xx(inod,1)*xx(inod,1) )          &
     &                      * a_s(inod) * a_s(inod)
!
             tcyl(inod,5) =   ( -txz * xx(inod,2)                       &
     &                         + tyz * xx(inod,1) ) * a_s(inod)
!
             tcyl(inod,6) = tzz
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_cyl_tensor_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_cyl_tensor_smp(np_smp, numnod,               &
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
       real(kind=kreal) :: txx, txy, txz, tyy, tyz, tzz
!
!
!$omp parallel do private(inod,ist,ied,txx,txy,txz,tyy,tyz,tzz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           txz = tensor(inod,3)
           tyy = tensor(inod,4)
           tyz = tensor(inod,5)
           tzz = tensor(inod,6)
!
           if ( s(inod).eq.0.0 ) then
             tensor(inod,1) =       txx
             tensor(inod,2) =       txy
             tensor(inod,3) =       txz
             tensor(inod,4) =       tyy
             tensor(inod,5) =       tyz
             tensor(inod,6) =       tzz
           else
             tensor(inod,1) = (  txx * xx(inod,1)*xx(inod,1)            &
     &                     + two*txy * xx(inod,1)*xx(inod,2)            &
     &                     +     tyy * xx(inod,2)*xx(inod,2) )          &
     &                      * a_s(inod) * a_s(inod)
!
             tensor(inod,2) = ( -txx * xx(inod,1)*xx(inod,2)            &
     &                     +     txy * (xx(inod,1)*xx(inod,1)           &
     &                                - xx(inod,2)*xx(inod,2) )         &
     &                     +     tyy*xx(inod,1)*xx(inod,2) )            &
     &                      * a_s(inod) * a_s(inod)
!
             tensor(inod,3) = (  txz * xx(inod,1)                       &
     &                     +     tyz * xx(inod,2) ) * a_s(inod)
!
             tensor(inod,4) = (  txx * xx(inod,2)*xx(inod,2)            &
     &                     - two*txy * xx(inod,1)*xx(inod,2)            &
     &                     +     tyy * xx(inod,1)*xx(inod,1) )          &
     &                      * a_s(inod) * a_s(inod)
!
             tensor(inod,5) = ( -txz * xx(inod,2)                       &
     &                         + tyz * xx(inod,1) ) * a_s(inod)
!
             tensor(inod,6) = tzz
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine overwrite_cyl_tensor_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_ss_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, v_ss, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_ss(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txx, txy, tyy
!
!
!$omp parallel do private(inod,ist,ied,txx,txy,tyy)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           tyy = tensor(inod,4)
!
           if ( s(inod).eq.0.0 ) then
             v_ss(inod) = txx
           else
             v_ss(inod) =     (  txx * xx(inod,1)*xx(inod,1)            &
     &                     + two*txy * xx(inod,1)*xx(inod,2)            &
     &                     +     tyy * xx(inod,2)*xx(inod,2) )          &
     &                      * a_s(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_ss_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_sp_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, v_sp, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_sp(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txx, txy, tyy
!
!
!$omp parallel do private(inod,ist,ied,txx,txy,tyy)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           tyy = tensor(inod,4)
!
           if ( s(inod).eq.0.0 ) then
             v_sp(inod) = txy
           else
             v_sp(inod) =     ( -txx * xx(inod,1)*xx(inod,2)            &
     &                     +     txy * (xx(inod,1)*xx(inod,1)           &
     &                                - xx(inod,2)*xx(inod,2) )         &
     &                     +     tyy*xx(inod,1)*xx(inod,2) )            &
     &                      * a_s(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sp_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_sz_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, v_sz, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_sz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txz, tyz
!
!
!$omp parallel do private(inod,ist,ied,txz,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txz = tensor(inod,3)
           tyz = tensor(inod,5)
!
           if ( s(inod).eq.0.0 ) then
             v_sz(inod) = txz
           else
             v_sz(inod) =     (  txz * xx(inod,1)                       &
     &                     +     tyz * xx(inod,2) ) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sz_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_pp_cyl_tensor_smp(np_smp, numnod, inod_smp_stack,  &
     &          tensor, v_pp, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_pp(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txx, txy, tyy
!
!
!$omp parallel do private(inod,ist,ied,txx,txy,tyy)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           tyy = tensor(inod,4)
!
           if ( s(inod).eq.0.0 ) then
             v_pp(inod) = tyy
           else
             v_pp(inod) =     (  txx * xx(inod,2)*xx(inod,2)            &
     &                     - two*txy * xx(inod,1)*xx(inod,2)            &
     &                     +     tyy * xx(inod,1)*xx(inod,1) )          &
     &                      * a_s(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_pp_cyl_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_pz_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, v_pz, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_pz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txz, tyz
!
!
!$omp parallel do private(inod,ist,ied,txz,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txz = tensor(inod,3)
           tyz = tensor(inod,5)
!
           if ( s(inod).eq.0.0 ) then
             v_pz(inod) = tyz
           else
             v_pz(inod) =     ( -txz * xx(inod,2)                       &
     &                         + tyz * xx(inod,1) ) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_pz_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_zz_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, v_zz)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
!
       real(kind=kreal), intent(inout) :: v_zz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           v_zz(inod) = tensor(inod,6)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_zz_tensor_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_tensor_2_cylinder_smp
