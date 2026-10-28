!>@file   cvt_sph_asym_t_2_cyl_smp.f90
!!        module cvt_sph_asym_t_2_cyl_smp
!!
!!@author H. Matsui
!!@date Programmed in March, 2009
!!
!>@brief Convert anti-symmetric tensor from spherical coordinate
!!       to cyrindrical coordinate
!!
!!@verbatim
!!      subroutine cal_cyl_asym_tsr_by_sph(np_smp, numnod,              &
!!     &          inod_smp_stack, asym_t, tsph, theta)
!!         integer (kind = kint), intent(in) :: np_smp, numnod
!!         integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!!         real(kind=kreal), intent(in) :: asym_t(numnod,3)
!!         real(kind=kreal), intent(in) :: theta(numnod)
!!         real(kind=kreal), intent(inout) :: tsph(numnod,3)
!!
!!      subroutine overwrite_cyl_asym_tsr_by_sph(np_smp, numnod,        &
!!     &          inod_smp_stack, asym_t, theta)
!!         integer (kind = kint), intent(in) :: np_smp, numnod
!!         integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!!         real(kind=kreal), intent(in) :: theta(numnod)
!!         real(kind=kreal), intent(inout) :: asym_t(numnod,3)
!!
!!      subroutine cal_sp_asym_tsr_by_cyl                               &
!!     &         (np_smp, numnod, inod_smp_stack, asym_t, t_sp, theta)
!!      subroutine cal_pr_asym_tsr_by_cyl                               &
!!     &         (np_smp, numnod, inod_smp_stack, asym_t, t_zs)
!!      subroutine cal_pz_asym_tsr_by_cyl                               &
!!     &         (np_smp, numnod, inod_smp_stack, asym_t, t_pz, theta)
!!         integer (kind = kint), intent(in) :: np_smp, numnod
!!         integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!!         real(kind=kreal), intent(in) :: theta(numnod)
!!         real(kind=kreal), intent(in) :: asym_t(numnod,3)
!!         real(kind=kreal), intent(inout) :: t_sp(numnod)
!!         real(kind=kreal), intent(inout) :: t_zs(numnod)
!!         real(kind=kreal), intent(inout) :: t_pz(numnod)
!!
!!   urut =         uzus
!!   upur = (as) * ( z* upuz - s* usup)
!!   utup = (as) * ( s* upuz + z* usup)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  numnod   Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!@n @param  theta(numnod)    colatitude
!!
!!@n @param  asym_t(numnod,3)
!!                    anti-symmetric asym_t in Cartesian coordinate
!!@n @param  txyz(numnod,3)
!!                    anti-symmetric asym_t in cylindeical coordinate
!!@n @param  t_sp(numnod)
!!                    @f$ T_{s \phi} @f$ in cylindeical coordinate
!!@n @param  t_zs(numnod)
!!                    @f$ T_{\phi r} @f$ in cylindeical coordinate
!!@n @param  t_pz(numnod)
!!                    @f$ T_{\theta \phi} @f$ in cylindeical coordinate
!
      module cvt_sph_asym_t_2_cyl_smp
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
      subroutine cal_cyl_asym_tsr_by_sph(np_smp, numnod,                &
     &          inod_smp_stack, asym_t, tsph, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: asym_t(numnod,3)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: tsph(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trt, tpr, ttp
!
!
!$omp parallel do private(inod,ist,ied,trt,tpr,ttp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trt = asym_t(inod,1)
           tpr = asym_t(inod,2)
           ttp = asym_t(inod,3)
!
           tsph(inod,1) = -tpr * cos(theta(inod))                       &
     &                   + ttp * sin(theta(inod))
           tsph(inod,2) =  trt
           tsph(inod,3) =  tpr * sin(theta(inod))                       &
     &                   + ttp * cos(theta(inod))
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_cyl_asym_tsr_by_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_cyl_asym_tsr_by_sph(np_smp, numnod,          &
     &          inod_smp_stack, asym_t, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: asym_t(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trt, tpr, ttp
!
!
!$omp parallel do private(inod,ist,ied,trt,tpr,ttp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trt = asym_t(inod,1)
           tpr = asym_t(inod,2)
           ttp = asym_t(inod,3)
!
           asym_t(inod,1) = -tpr * cos(theta(inod))                     &
     &                     + ttp * sin(theta(inod))
           asym_t(inod,2) =  trt
           asym_t(inod,3) =  tpr * sin(theta(inod))                     &
     &                     + ttp * cos(theta(inod))
        end do
      end do
!$omp end parallel do
!
      end subroutine overwrite_cyl_asym_tsr_by_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sp_asym_tsr_by_cyl                                 &
     &         (np_smp, numnod, inod_smp_stack, asym_t, t_sp, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: theta(numnod)
       real(kind=kreal), intent(in) :: asym_t(numnod,3)
!
       real(kind=kreal), intent(inout) :: t_sp(numnod)
!
       integer(kind = kint) :: ip, inod, ist, ied
       real(kind = kreal) :: tpr, ttp
!
!
!$omp parallel do private(inod,ist,ied,tpr,ttp)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
           tpr = asym_t(inod,2)
           ttp = asym_t(inod,3)
           t_sp(inod) = -tpr * cos(theta(inod))                         &
     &                 + ttp * sin(theta(inod))
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sp_asym_tsr_by_cyl
!
! -----------------------------------------------------------------------
!
      subroutine cal_pr_asym_tsr_by_cyl                                 &
     &         (np_smp, numnod, inod_smp_stack, asym_t, t_pr)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: asym_t(numnod,3)
!
       real(kind=kreal), intent(inout) :: t_pr(numnod)
!
       integer(kind = kint) :: ist, ied
!
!
      ist = inod_smp_stack(0)
      ied = inod_smp_stack(np_smp)
!$omp parallel workshare
      t_pr(ist:ied) = asym_t(ist:ied,1)
!$omp end parallel workshare
!
      end subroutine cal_pr_asym_tsr_by_cyl
!
! -----------------------------------------------------------------------
!
      subroutine cal_pz_asym_tsr_by_cyl                                 &
     &         (np_smp, numnod, inod_smp_stack, asym_t, t_pz, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: theta(numnod)
       real(kind=kreal), intent(in) :: asym_t(numnod,3)
!
       real(kind=kreal), intent(inout) :: t_pz(numnod)
!
       integer(kind = kint) :: ip, inod, ist, ied
       real(kind = kreal) :: tpr, ttp
!
!
!$omp parallel do private(inod,ist,ied,tpr,ttp)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
           tpr = asym_t(inod,2)
           ttp = asym_t(inod,3)
           t_pz(inod) =  tpr * sin(theta(inod))                         &
     &                 + ttp * cos(theta(inod))
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_pz_asym_tsr_by_cyl
!
! -----------------------------------------------------------------------
!
      end module cvt_sph_asym_t_2_cyl_smp
