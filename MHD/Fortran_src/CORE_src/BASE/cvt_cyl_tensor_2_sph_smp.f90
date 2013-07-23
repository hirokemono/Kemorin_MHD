!>@file   cvt_cyl_tensor_2_sph_smp.f90
!!@brief  module cvt_cyl_tensor_2_sph_smp
!!
!!@author H. Matsui
!!@date Programmed in March, 2009
!
!>@brief Convert symmetric tensor from cylindrical coordinate
!!       to spherical coordinate
!!
!!@verbatim
!!      subroutine cal_sph_tensor_by_cyl_smp(np_smp, numnod,            &
!!     &          inod_smp_stack, tensor, tsph, theta)
!!
!!      subroutine overwrite_sph_tensor_by_cyl_smp(np_smp, numnod,      &
!!     &          inod_smp_stack, tensor, theta)
!!
!!      subroutine cal_rr_tensor_by_cyl_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, t_rr, theta)
!!      subroutine cal_rt_tensor_by_cyl_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, t_rt, theta)
!!      subroutine cal_rp_tensor_by_cyl_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, t_rp, theta)
!!      subroutine cal_tt_tensor_by_cyl_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, t_tt, theta)
!!      subroutine cal_tp_tensor_by_cyl_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, t_tp, theta)
!!      subroutine cal_pp_tensor_by_cyl_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, t_pp)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  numnod   Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!@n @param  theta(numnod)    colatitude
!!
!!@n @param  tensor(numnod,6)
!!                    symmetric tensor in Cartesian coordinate
!!@n @param  txyz(numnod,6)
!!                    symmetric tensor in spherical coordinate
!!@n @param  t_rr(numnod)
!!                    @f$ T_{rr} @f$ in spherical coordinate
!!@n @param  t_rt(numnod)
!!                    @f$ T_{r \theta} @f$ in spherical coordinate
!!@n @param  t_rp(numnod)
!!                    @f$ T_{r \phi} @f$ in spherical coordinate
!!@n @param  t_tt(numnod)
!!                    @f$ T_{\theta \theta} @f$ in spherical coordinate
!!@n @param  t_tp(numnod)
!!                    @f$ T_{\theta \phi} @f$ in spherical coordinate
!!@n @param  t_pp(numnod)
!!                    @f$ T_{\phi \phi} @f$ in spherical coordinate
!
      module cvt_cyl_tensor_2_sph_smp
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
      subroutine cal_sph_tensor_by_cyl_smp(np_smp, numnod,              &
     &          inod_smp_stack, tensor, tsph, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: tsph(numnod,6)
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
             tsph(inod,1)                                               &
     &          =    tss * sin( theta(inod) ) * sin( theta(inod) )      &
     &         + two*tsz * sin( theta(inod) ) * cos( theta(inod) )      &
     &         +     tzz * cos( theta(inod) ) * cos( theta(inod) )
!
             tsph(inod,2)                                               &
     &          =    tss * sin( theta(inod) ) * cos( theta(inod) )      &
     &         +     tsz * ( cos( theta(inod) )*cos( theta(inod) )      &
     &                     - sin( theta(inod) )*sin( theta(inod) ) )    &
     &         -     tzz * cos( theta(inod) ) * sin( theta(inod) )
!
             tsph(inod,3)                                               &
     &          =    tsp * sin( theta(inod) )                           &
     &         +     tpz * cos( theta(inod) )
!
             tsph(inod,4)                                               &
     &          =    tss * cos( theta(inod) ) * cos( theta(inod) )      &
     &         - two*tsz * cos( theta(inod) ) * sin( theta(inod) )      &
     &         +     tzz * sin( theta(inod) ) * sin( theta(inod) )
!
             tsph(inod,5)                                               &
     &          =    tsp * cos( theta(inod) )                           &
     &         -     tpz * sin( theta(inod) )
!
             tsph(inod,6) = tpp
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sph_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_sph_tensor_by_cyl_smp(np_smp, numnod,        &
     &          inod_smp_stack, tensor, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: theta(numnod)
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
           tensor(inod,1)                                               &
     &          =    tss * sin( theta(inod) ) * sin( theta(inod) )      &
     &         + two*tsz * sin( theta(inod) ) * cos( theta(inod) )      &
     &         +     tzz * cos( theta(inod) ) * cos( theta(inod) )
!
           tensor(inod,2)                                               &
     &          =    tss * sin( theta(inod) ) * cos( theta(inod) )      &
     &         +     tsz * ( cos( theta(inod) )*cos( theta(inod) )      &
     &                     - sin( theta(inod) )*sin( theta(inod) ) )    &
     &         -     tzz * cos( theta(inod) ) * sin( theta(inod) )
!
           tensor(inod,3)                                               &
     &          =    tsp * sin( theta(inod) )                           &
     &         +     tpz * cos( theta(inod) )
!
           tensor(inod,4)                                               &
     &          =    tss * cos( theta(inod) ) * cos( theta(inod) )      &
     &         - two*tsz * cos( theta(inod) ) * sin( theta(inod) )      &
     &         +     tzz * sin( theta(inod) ) * sin( theta(inod) )
!
           tensor(inod,5)                                               &
     &          =    tsp * cos( theta(inod) )                           &
     &         -     tpz * sin( theta(inod) )
!
           tensor(inod,6) = tpp
        end do
      end do
!$omp end parallel do
!
      end subroutine overwrite_sph_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rr_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_rr, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: t_rr(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tss, tsz, tzz
!
!
!$omp parallel do private(inod,ist,ied,tss,tsz,tzz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tss = tensor(inod,1)
           tsz = tensor(inod,3)
           tzz = tensor(inod,6)
!
             t_rr(inod)                                                 &
     &          =    tss * sin( theta(inod) ) * sin( theta(inod) )      &
     &         + two*tsz * sin( theta(inod) ) * cos( theta(inod) )      &
     &         +     tzz * cos( theta(inod) ) * cos( theta(inod) )
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_rr_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_rt_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_rt, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: t_rt(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tss, tsz, tzz
!
!
!$omp parallel do private(inod,ist,ied,tss,tsz,tzz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tss = tensor(inod,1)
           tsz = tensor(inod,3)
           tzz = tensor(inod,6)
!
             t_rt(inod)                                                 &
     &          =    tss * sin( theta(inod) ) * cos( theta(inod) )      &
     &         +     tsz * ( cos( theta(inod) )*cos( theta(inod) )      &
     &                     - sin( theta(inod) )*sin( theta(inod) ) )    &
     &         -     tzz * cos( theta(inod) ) * sin( theta(inod) )
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_rt_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_rp_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_rp, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: t_rp(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tsp, tpz
!
!
!$omp parallel do private(inod,ist,ied,tsp,tpz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tsp = tensor(inod,2)
           tpz = tensor(inod,5)
!
             t_rp(inod)                                                 &
     &          =    tsp * sin( theta(inod) )                           &
     &         +     tpz * cos( theta(inod) )
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_rp_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_tt_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_tt, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: t_tt(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tss, tsz, tzz
!
!
!$omp parallel do private(inod,ist,ied,tss,tsz,tzz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tss = tensor(inod,1)
           tsz = tensor(inod,3)
           tzz = tensor(inod,6)
!
             t_tt(inod)                                                 &
     &          =    tss * cos( theta(inod) ) * cos( theta(inod) )      &
     &         - two*tsz * cos( theta(inod) ) * sin( theta(inod) )      &
     &         +     tzz * sin( theta(inod) ) * sin( theta(inod) )
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_tt_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_tp_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_tp, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: t_tp(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tsp, tpz
!
!
!$omp parallel do private(inod,ist,ied,tsp,tpz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tsp = tensor(inod,2)
           tpz = tensor(inod,5)
!
             t_tp(inod)                                                 &
     &          =    tsp * cos( theta(inod) )                           &
     &         -     tpz * sin( theta(inod) )
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_tp_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_pp_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_pp)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
!
       real(kind=kreal), intent(inout) :: t_pp(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           t_pp(inod) = tensor(inod,4)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_pp_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_cyl_tensor_2_sph_smp
