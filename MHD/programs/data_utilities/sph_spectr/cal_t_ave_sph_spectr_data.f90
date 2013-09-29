!cal_t_ave_sph_spectr_data.f90
!      module cal_t_ave_sph_spectr_data
!
      module cal_t_ave_sph_spectr_data
!
!      Written by H. Matsui on Dec., 2008
!
      use m_precision
      use m_constants
!
      implicit none
!
      real(kind= kreal), allocatable :: d_rj_tmp(:,:)
!
      private :: deallocate_d_rj_tmp
!
!      subroutine allocate_d_rj_tmp
!
!      subroutine sum_sph_spectr_data
!      subroutine t_ave_sph_spectr_data(ist_step, ied_step)
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
      allocate( d_rj_tmp(nnod_rj,ntot_phys_rj) )
      d_rj_tmp = 0.0d0
!
      end subroutine allocate_d_rj_tmp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_d_rj_tmp
!
      deallocate( d_rj_tmp )
!
      end subroutine deallocate_d_rj_tmp
!
!-----------------------------------------------------------------------
!
      subroutine sum_sph_spectr_data
!
      use m_sph_spectr_data
      use m_spheric_parameter
      use calypso_mpi
!
!
      integer(kind = kint) :: nd, inod
!
!$omp parallel do private(inod)
      do nd = 1, ntot_phys_rj
        do inod = 1, nnod_rj
          d_rj_tmp(inod,nd) = d_rj_tmp(inod,nd) + d_rj(inod,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine sum_sph_spectr_data
!
!-----------------------------------------------------------------------
!
      subroutine t_ave_sph_spectr_data(ist_step, ied_step)
!
      use m_sph_spectr_data
      use m_spheric_parameter
!
!
      integer(kind = kint), intent(in) :: ist_step, ied_step
      integer(kind = kint) :: nd, inod
      real(kind =  kreal) :: anum
!
!
      anum = one / dble(ied_step-ist_step+1)
!
!$omp parallel do private(inod)
      do nd = 1, ntot_phys_rj
        do inod = 1, nnod_rj
          d_rj(inod,nd) = d_rj_tmp(inod,nd) * anum
        end do
      end do
!$omp end parallel do
!
      call deallocate_d_rj_tmp
!
      end subroutine t_ave_sph_spectr_data
!
!-----------------------------------------------------------------------
!
      end module cal_t_ave_sph_spectr_data
