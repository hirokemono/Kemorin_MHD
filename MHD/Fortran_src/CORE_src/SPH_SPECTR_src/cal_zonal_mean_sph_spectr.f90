!cal_zonal_mean_sph_spectr.f90
!      module cal_zonal_mean_sph_spectr
!
!      Written by H. Matsui on Dec., 2008
!
!      subroutine take_zonal_mean_sph_spectr
!
!      subroutine pick_order_sph_spectr(id_order)
!      subroutine pick_degree_sph_spectr(num_degree, ipick_degree)
!
      module cal_zonal_mean_sph_spectr
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine take_zonal_mean_sph_spectr
!
      integer(kind = kint), parameter :: m_zero(1) = (/izero/)
!
!
      call pick_order_sph_spectr(ione, m_zero)
!
      end subroutine take_zonal_mean_sph_spectr
!
!-----------------------------------------------------------------------
!
      subroutine pick_order_sph_spectr(num_order, ipick_order)
!
      use m_sph_spectr_data
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: num_order
      integer(kind = kint), intent(in) :: ipick_order(num_order)
!
      integer(kind = kint) :: nd, j, kr, inod, inum, iflag
!
!
!$omp parallel
      do j = 1, nidx_rj(2)
        iflag = 0
        do inum = 1, num_order
          if(ipick_order(inum) .eq. idx_gl_1d_rj_j(j,3)) then
            iflag = 1
            exit
          end if
        end do
!
        if (iflag .eq. 0) then
!$omp do private(nd,kr,inod)
          do nd = 1, ntot_phys_rj
            do kr = 1, nidx_rj(1)
              inod = j + (kr-1)*nidx_rj(2)
              d_rj(inod,nd) = zero
            end do
          end do
!$omp end do
        end if
!
      end do
!$omp end parallel
!
      end subroutine pick_order_sph_spectr
!
!-----------------------------------------------------------------------
!
      subroutine pick_degree_sph_spectr(num_degree, ipick_degree)
!
      use m_sph_spectr_data
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: num_degree
      integer(kind = kint), intent(in) :: ipick_degree(num_degree)
!
      integer(kind = kint) :: nd, j, kr, inod, inum, iflag
!
!
!$omp parallel
      do j = 1, nidx_rj(2)
        iflag = 0
        do inum = 1, num_degree
          if(ipick_degree(inum) .eq. idx_gl_1d_rj_j(j,2)) then
            iflag = 1
            exit
          end if
        end do
!
        if (iflag .eq. 0) then
!$omp do private(nd,kr,inod)
          do nd = 1, ntot_phys_rj
            do kr = 1, nidx_rj(1)
              inod = j + (kr-1)*nidx_rj(2)
              d_rj(inod,nd) = zero
            end do
          end do
!$omp end do
        end if
!
      end do
!$omp end parallel
!
      end subroutine pick_degree_sph_spectr
!
!-----------------------------------------------------------------------
!
      end module cal_zonal_mean_sph_spectr
