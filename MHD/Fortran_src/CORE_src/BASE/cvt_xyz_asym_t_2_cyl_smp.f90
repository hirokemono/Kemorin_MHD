!>@file   cvt_xyz_asym_t_2_cyl_smp.f90
!!@brief  module cvt_xyz_asym_t_2_cyl_smp
!!
!!@author H. Matsui
!!@date Programmed in March, 2009
!
!>@brief Convert anti-symmetric tensor from Cartesian coordinate
!!       to cylindrical coordinate
!!
!!@verbatim
!!      subroutine cal_cyl_asym_t_smp(np_smp, numnod, inod_smp_stack,   &
!!     &          tensor, tcyl, xx, s, a_s)
!!
!!      subroutine overwrite_cyl_asym_t_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, xx, s, a_s)
!!
!!      subroutine cal_sp_asym_t_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_sp)
!!      subroutine cal_zs_asym_t_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_zs, xx, s, a_s)
!!      subroutine cal_pz_asym_t_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_pz, xx, s, a_s)
!!
!!   usup =         ux*uy
!!   uzus = (as) * (      x* uz*ux - y* uy*uz)
!!   upuz = (as) * (      y* uz*ux + x* uy*uz)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  numnod   Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!@n @param  xx(numnod,3) position in Cartesian coordinate
!!@n @param  s(numnod)    cylindrical radius
!!@n @param  a_s(numnod)  1 / s
!!
!!@n @param  tensor(numnod,3)
!!                    anti-symmetric tensor in Cartesian coordinate
!!@n @param  tcyl(numnod,3)
!!                    anti-symmetric tensor in Cylindrical coordinate
!!@n @param  t_sp(numnod)
!!                    @f$ T_{s\phi} @f$ in Cylindrical coordinate
!!@n @param  t_zs(numnod)
!!                    @f$ T_{zs} @f$  in Cylindrical coordinate
!!@n @param  t_pz(numnod)
!!                     @f$ T_{\phi z} @f$ in Cylindrical coordinate
!!
!
      module cvt_xyz_asym_t_2_cyl_smp
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
      subroutine cal_cyl_asym_t_smp(np_smp, numnod, inod_smp_stack,     &
     &          tensor, tcyl, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: tcyl(numnod,3)
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
           if ( s(inod).eq.0.0 ) then
             tcyl(inod,1) =   txy
             tcyl(inod,2) =   tzx
             tcyl(inod,3) =   tyz
           else
             tcyl(inod,1) =   txy
!
             tcyl(inod,2) = ( tzx * xx(inod,1)                          &
     &                      - tyz * xx(inod,2) ) * a_s(inod)
!
             tcyl(inod,3) = ( tzx * xx(inod,2)                          &
     &                      + tyz * xx(inod,1) ) * a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_cyl_asym_t_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_cyl_asym_t_smp(np_smp, numnod,               &
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
           if ( s(inod).eq.0.0 ) then
             tensor(inod,1) =   txy
             tensor(inod,2) =   tzx
             tensor(inod,3) =   tyz
           else
             tensor(inod,1) =   txy
!
             tensor(inod,2) = ( tzx * xx(inod,1)                        &
     &                        - tyz * xx(inod,2) ) * a_s(inod)
!
             tensor(inod,3) = ( tzx * xx(inod,2)                        &
     &                        + tyz * xx(inod,1) ) * a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine overwrite_cyl_asym_t_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sp_asym_t_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_sp)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,3)
!
       real(kind=kreal), intent(inout) :: t_sp(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           t_sp(inod) =   tensor(inod,1)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_sp_asym_t_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_zs_asym_t_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_zs, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_zs(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tzx, tyz
!
!
!$omp do private(inod,ist,ied,tzx,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tzx = tensor(inod,2)
           tyz = tensor(inod,3)
!
           if ( s(inod).eq.0.0 ) then
             t_zs(inod) =     tzx
           else
             t_zs(inod) =   ( tzx * xx(inod,1)                          &
     &                      - tyz * xx(inod,2) ) * a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_zs_asym_t_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_pz_asym_t_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_pz, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_pz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tzx, tyz
!
!
!$omp do private(inod,ist,ied,tzx,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tzx = tensor(inod,2)
           tyz = tensor(inod,3)
!
           if ( s(inod).eq.0.0 ) then
             t_pz(inod) =     tyz
           else
             t_pz(inod) =   ( tzx * xx(inod,2)                          &
     &                      + tyz * xx(inod,1) ) * a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_pz_asym_t_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_xyz_asym_t_2_cyl_smp
