!>@file   cvt_cyl_asym_t_2_sph_smp.f90
!!        module cvt_cyl_asym_t_2_sph_smp
!!
!!@author H. Matsui
!!@date Programmed in March, 2009
!!
!>@brief Convert anti-symmetric tensor from cylindrical coordinate
!!       to spherical coordinate
!!
!!@verbatim
!!      subroutine cal_sph_asym_tsr_by_cyl(np_smp, numnod,              &
!!     &          inod_smp_stack, asym_t, tsph, theta)
!!         integer (kind = kint), intent(in) :: np_smp, numnod
!!         integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!!         real(kind=kreal), intent(in) :: asym_t(numnod,3)
!!         real(kind=kreal), intent(in) :: theta(numnod)
!!         real(kind=kreal), intent(inout) :: tsph(numnod,3)
!!
!!      subroutine overwrite_sph_asym_tsr_by_cyl(np_smp, numnod,        &
!!     &          inod_smp_stack, asym_t, theta)
!!         integer (kind = kint), intent(in) :: np_smp, numnod
!!         integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!!         real(kind=kreal), intent(in) :: theta(numnod)
!!         real(kind=kreal), intent(inout) :: asym_t(numnod,3)
!!
!!      subroutine cal_rt_asym_tsr_by_cyl                               &
!!     &         (np_smp, numnod, inod_smp_stack, asym_t, t_rt)
!!      subroutine cal_pr_asym_tsr_by_cyl                               &
!!     &         (np_smp, numnod, inod_smp_stack, asym_t, t_pr, theta)
!!      subroutine cal_tp_asym_tsr_by_cyl                               &
!!     &         (np_smp, numnod, inod_smp_stack, asym_t, t_tp, theta)
!!         integer (kind = kint), intent(in) :: np_smp, numnod
!!         integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!!         real(kind=kreal), intent(in) :: theta(numnod)
!!         real(kind=kreal), intent(in) :: asym_t(numnod,3)
!!         real(kind=kreal), intent(inout) :: t_rt(numnod)
!!         real(kind=kreal), intent(inout) :: t_pr(numnod)
!!         real(kind=kreal), intent(inout) :: t_tp(numnod)
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
!!                    anti-symmetric asym_t in spherical coordinate
!!@n @param  t_rt(numnod)
!!                    @f$ T_{r \theta} @f$ in spherical coordinate
!!@n @param  t_pr(numnod)
!!                    @f$ T_{\phi r} @f$ in spherical coordinate
!!@n @param  t_tp(numnod)
!!                    @f$ T_{\theta \phi} @f$ in spherical coordinate
!
      module cvt_cyl_asym_t_2_sph_smp
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
      subroutine cal_sph_asym_tsr_by_cyl(np_smp, numnod,                &
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
       real(kind=kreal) :: tsp, tzs, tpz
!
!
!$omp parallel do private(inod,ist,ied,tsp,tzs,tpz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tsp = asym_t(inod,1)
           tzs = asym_t(inod,2)
           tpz = asym_t(inod,3)
!
           tsph(inod,1) =  tzs
           tsph(inod,2) =  tpz * cos(theta(inod))                       &
     &                   - tsp * sin(theta(inod))
           tsph(inod,3) =  tpz * sin(theta(inod))                       &
     &                   + tsp * cos(theta(inod))
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sph_asym_tsr_by_cyl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_sph_asym_tsr_by_cyl(np_smp, numnod,          &
     &          inod_smp_stack, asym_t, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: asym_t(numnod,3)
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
           tsp = asym_t(inod,1)
           tzs = asym_t(inod,2)
           tpz = asym_t(inod,3)
!
           asym_t(inod,1) =  tzs
           asym_t(inod,2) =  tpz * cos(theta(inod))                     &
     &                     - tsp * sin(theta(inod))
           asym_t(inod,3) =  tpz * sin(theta(inod))                     &
     &                     + tsp * cos(theta(inod))
        end do
      end do
!$omp end parallel do
!
      end subroutine overwrite_sph_asym_tsr_by_cyl
!
! -----------------------------------------------------------------------
!
      subroutine cal_rt_asym_tsr_by_cyl                                 &
     &         (np_smp, numnod, inod_smp_stack, asym_t, t_rt)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: asym_t(numnod,3)
!
       real(kind=kreal), intent(inout) :: t_rt(numnod)
!
       integer(kind = kint) :: ist, ied
!
!
      ist = inod_smp_stack(0)
      ied = inod_smp_stack(np_smp)
!$omp parallel workshare
      t_rt(ist:ied) = asym_t(ist:ied,2)
!$omp end parallel workshare
!
      end subroutine cal_rt_asym_tsr_by_cyl
!
! -----------------------------------------------------------------------
!
      subroutine cal_pr_asym_tsr_by_cyl                                 &
     &         (np_smp, numnod, inod_smp_stack, asym_t, t_pr, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: theta(numnod)
       real(kind=kreal), intent(in) :: asym_t(numnod,3)
!
       real(kind=kreal), intent(inout) :: t_pr(numnod)
!
       integer(kind = kint) :: ip, inod, ist, ied
       real(kind = kreal) :: tsp, tpz
!
!
!$omp parallel do private(inod,ist,ied,tsp,tpz)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          tsp = asym_t(inod,1)
          tpz = asym_t(inod,3)
          t_pr(inod) = tpz * cos(theta(inod)) - tsp * sin(theta(inod))
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_pr_asym_tsr_by_cyl
!
! -----------------------------------------------------------------------
!
      subroutine cal_tp_asym_tsr_by_cyl                                 &
     &         (np_smp, numnod, inod_smp_stack, asym_t, t_tp, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: theta(numnod)
       real(kind=kreal), intent(in) :: asym_t(numnod,3)
!
       real(kind=kreal), intent(inout) :: t_tp(numnod)
!
       integer(kind = kint) :: ip, inod, ist, ied
       real(kind = kreal) :: tsp, tpz
!
!
!$omp parallel do private(inod,ist,ied,tsp,tpz)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          tsp = asym_t(inod,1)
          tpz = asym_t(inod,3)
          t_tp(inod) = tpz * sin(theta(inod)) + tsp * cos(theta(inod))
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_tp_asym_tsr_by_cyl
!
! -----------------------------------------------------------------------
!
      end module cvt_cyl_asym_t_2_sph_smp
