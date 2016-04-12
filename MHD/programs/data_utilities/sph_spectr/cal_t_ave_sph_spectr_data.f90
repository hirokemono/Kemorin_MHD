!cal_t_ave_sph_spectr_data.f90
!      module cal_t_ave_sph_spectr_data
!
      module cal_t_ave_sph_spectr_data
!
!      Written by H. Matsui on Dec., 2008
!
!      subroutine allocate_d_rj_tmp
!      subroutine deallocate_d_rj_tmp
!
!!      subroutine sum_sph_spectr_data(ntot_phys_rj, d_rj)
!!      subroutine sum_deviation_sph_spectr(ntot_phys_rj, d_rj)
!!
!!      subroutine t_ave_sph_spectr_data                                &
!!     &         (ist_step, ied_step, ntot_phys_rj, d_rj)
!!      subroutine sdev_sph_spectr_data                                 &
!!     &         (ist_step, ied_step, ntot_phys_rj, d_rj)
!
      use m_precision
      use m_constants
!
      implicit none
!
      real(kind= kreal), allocatable :: d_rj_ave(:,:)
      real(kind= kreal), allocatable :: d_rj_dev(:,:)
!
      private :: d_rj_ave, d_rj_dev
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_d_rj_tmp
!
      use m_sph_spectr_data
      use m_spheric_parameter
!
      allocate( d_rj_dev(nnod_rj,ntot_phys_rj) )
      allocate( d_rj_ave(nnod_rj,ntot_phys_rj) )
!
      if(nnod_rj*ntot_phys_rj .le. 0) return
      d_rj_dev = 0.0d0
      d_rj_ave = 0.0d0
!
      end subroutine allocate_d_rj_tmp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_d_rj_tmp
!
      deallocate(d_rj_dev,  d_rj_ave)
!
      end subroutine deallocate_d_rj_tmp
!
!-----------------------------------------------------------------------
!
      subroutine sum_sph_spectr_data(ntot_phys_rj, d_rj)
!
      use m_spheric_parameter
      use calypso_mpi
!
      integer(kind = kint), intent(in) ::  ntot_phys_rj
      real (kind=kreal), intent(in) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: nd, inod
!
!$omp parallel do private(inod)
      do nd = 1, ntot_phys_rj
        do inod = 1, nnod_rj
          d_rj_ave(inod,nd) = d_rj_ave(inod,nd) + d_rj(inod,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine sum_sph_spectr_data
!
!-----------------------------------------------------------------------
!
      subroutine sum_deviation_sph_spectr(ntot_phys_rj, d_rj)
!
      use m_spheric_parameter
      use calypso_mpi
!
      integer(kind = kint), intent(in) ::  ntot_phys_rj
      real (kind=kreal), intent(in) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: nd, inod
!
!$omp parallel do private(inod)
      do nd = 1, ntot_phys_rj
        do inod = 1, nnod_rj
          d_rj_dev(inod,nd) = d_rj_dev(inod,nd)                         &
     &                       + (d_rj(inod,nd) - d_rj_ave(inod,nd))**2
        end do
      end do
!$omp end parallel do
!
      end subroutine sum_deviation_sph_spectr
!
!-----------------------------------------------------------------------
!
      subroutine t_ave_sph_spectr_data                                  &
     &         (ist_step, ied_step, ntot_phys_rj, d_rj)
!
      use m_spheric_parameter
!
!
      integer(kind = kint), intent(in) :: ist_step, ied_step
      integer(kind = kint), intent(in) ::  ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: nd, inod
      real(kind =  kreal) :: anum
!
!
      anum = one / dble(ied_step-ist_step+1)
!
!$omp parallel do private(inod)
      do nd = 1, ntot_phys_rj
        do inod = 1, nnod_rj
          d_rj(inod,nd) = d_rj_ave(inod,nd) * anum
        end do
      end do
!$omp end parallel do
!
!      call deallocate_d_rj_tmp
!
      end subroutine t_ave_sph_spectr_data
!
!-----------------------------------------------------------------------
!
      subroutine sdev_sph_spectr_data                                   &
     &         (ist_step, ied_step, ntot_phys_rj, d_rj)
!
      use m_spheric_parameter
!
!
      integer(kind = kint), intent(in) :: ist_step, ied_step
      integer(kind = kint), intent(in) ::  ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: nd, inod
      real(kind =  kreal) :: anum
!
!
      anum = one / dble(ied_step-ist_step+1)
!
!$omp parallel do private(inod)
      do nd = 1, ntot_phys_rj
        do inod = 1, nnod_rj
          d_rj(inod,nd) = sqrt(d_rj_dev(inod,nd)) * anum
        end do
      end do
!$omp end parallel do
!
      end subroutine sdev_sph_spectr_data
!
!-----------------------------------------------------------------------
!
      end module cal_t_ave_sph_spectr_data
