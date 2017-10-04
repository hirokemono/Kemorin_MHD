!cal_t_ave_sph_spectr_data.f90
!      module cal_t_ave_sph_spectr_data
!
!      Written by H. Matsui on Dec., 2008
!
!!      subroutine allocate_d_rj_tmp(rj_fld, ave_WK)
!!      subroutine deallocate_d_rj_tmp(ave_WK)
!!
!!      subroutine sum_sph_spectr_data(rj_fld, ave_WK)
!!      subroutine sum_deviation_sph_spectr(rj_fld, ave_WK)
!!
!!      subroutine t_ave_sph_spectr_data                                &
!!     &         (ist_step, ied_step, ave_WK, rj_fld)
!!      subroutine sdev_sph_spectr_data                                 &
!!     &         (ist_step, ied_step, ave_WK, rj_fld)
!
      module cal_t_ave_sph_spectr_data
!
!
      use m_precision
      use m_constants
      use t_phys_data
!
      implicit none
!
      type sph_average_work
        real(kind= kreal), allocatable :: d_rj_ave(:,:)
        real(kind= kreal), allocatable :: d_rj_dev(:,:)
      end type sph_average_work
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_d_rj_tmp(rj_fld, ave_WK)
!
      type(phys_data), intent(in) :: rj_fld
      type(sph_average_work), intent(inout) :: ave_WK
!
!
      allocate( ave_WK%d_rj_dev(rj_fld%n_point,rj_fld%ntot_phys) )
      allocate( ave_WK%d_rj_ave(rj_fld%n_point,rj_fld%ntot_phys) )
!
      if(rj_fld%n_point*rj_fld%ntot_phys .le. 0) return
      ave_WK%d_rj_dev = 0.0d0
      ave_WK%d_rj_ave = 0.0d0
!
      end subroutine allocate_d_rj_tmp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_d_rj_tmp(ave_WK)
!
      type(sph_average_work), intent(inout) :: ave_WK
!
      deallocate(ave_WK%d_rj_dev,  ave_WK%d_rj_ave)
!
      end subroutine deallocate_d_rj_tmp
!
!-----------------------------------------------------------------------
!
      subroutine sum_sph_spectr_data(rj_fld, ave_WK)
!
      use calypso_mpi
!
      type(phys_data), intent(in) :: rj_fld
      type(sph_average_work), intent(inout) :: ave_WK
!
      integer(kind = kint) :: nd, inod
!
!$omp parallel do private(inod)
      do nd = 1, rj_fld%ntot_phys
        do inod = 1, rj_fld%n_point
          ave_WK%d_rj_ave(inod,nd) = ave_WK%d_rj_ave(inod,nd)           &
     &                       + rj_fld%d_fld(inod,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine sum_sph_spectr_data
!
!-----------------------------------------------------------------------
!
      subroutine sum_deviation_sph_spectr(rj_fld, ave_WK)
!
      use calypso_mpi
!
      type(phys_data), intent(in) :: rj_fld
      type(sph_average_work), intent(inout) :: ave_WK
!
!
      integer(kind = kint) :: nd, inod
!
!$omp parallel do private(inod)
      do nd = 1, rj_fld%ntot_phys
        do inod = 1, rj_fld%n_point
          ave_WK%d_rj_dev(inod,nd) = ave_WK%d_rj_dev(inod,nd)           &
     &         + (rj_fld%d_fld(inod,nd) - ave_WK%d_rj_ave(inod,nd))**2
        end do
      end do
!$omp end parallel do
!
      end subroutine sum_deviation_sph_spectr
!
!-----------------------------------------------------------------------
!
      subroutine t_ave_sph_spectr_data                                  &
     &         (ist_step, ied_step, ave_WK, rj_fld)
!
!
      integer(kind = kint), intent(in) :: ist_step, ied_step
      type(sph_average_work), intent(in) :: ave_WK
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: nd, inod
      real(kind =  kreal) :: anum
!
!
      anum = one / dble(ied_step-ist_step+1)
!
!$omp parallel do private(inod)
      do nd = 1, rj_fld%ntot_phys
        do inod = 1, rj_fld%n_point
          rj_fld%d_fld(inod,nd) = ave_WK%d_rj_ave(inod,nd) * anum
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
     &         (ist_step, ied_step, ave_WK, rj_fld)
!
!
      integer(kind = kint), intent(in) :: ist_step, ied_step
      type(sph_average_work), intent(in) :: ave_WK
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: nd, inod
      real(kind =  kreal) :: anum
!
!
      anum = one / dble(ied_step-ist_step+1)
!
!$omp parallel do private(inod)
      do nd = 1, rj_fld%ntot_phys
        do inod = 1, rj_fld%n_point
          rj_fld%d_fld(inod,nd) = sqrt(ave_WK%d_rj_dev(inod,nd)) * anum
        end do
      end do
!$omp end parallel do
!
      end subroutine sdev_sph_spectr_data
!
!-----------------------------------------------------------------------
!
      end module cal_t_ave_sph_spectr_data
