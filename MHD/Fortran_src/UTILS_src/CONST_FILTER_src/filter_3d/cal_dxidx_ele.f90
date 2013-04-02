!
!      module cal_dxidx_ele
!
!        programmed by H.Matsui on Nov., 2008
!
!      subroutine s_cal_dxidx_ele
!
      module cal_dxidx_ele
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
      subroutine s_cal_dxidx_ele
!
      use m_geometry_parameter
      use m_machine_parameter
      use m_fem_gauss_int_coefs
      use m_jacobians
      use m_dxi_dxes_3d_node
!
      integer (kind = kint) :: ii, ix, i0
      integer (kind=kint) :: iele, ip, ist, ied
!
!
!$omp parallel do private(ist,ied,iele)
      do ip = 1, np_smp
        ist = iele_smp_stack(ip-1) + 1
        ied = iele_smp_stack(ip)
!cdir nodep noloopchg
        do iele = ist, ied
          dxidx_ele(iele) = zero
          deidx_ele(iele) = zero
          dzidx_ele(iele) = zero
!
          dxidy_ele(iele) = zero
          deidy_ele(iele) = zero
          dzidy_ele(iele) = zero
!
          dxidz_ele(iele) = zero
          deidz_ele(iele) = zero
          dzidz_ele(iele) = zero
        end do
      end do
!
      i0 = max_int_point
        do ii = 1, i0*i0*i0
!
          ix = int_start3(i0) + ii
!$omp parallel do private(ist,ied,iele)
          do ip = 1, np_smp
            ist = iele_smp_stack(ip-1) + 1
            ied = iele_smp_stack(ip)
!
!cdir nodep noloopchg
            do iele = ist, ied
              dxidx_ele(iele) = dxidx_ele(iele)                         &
     &                         + dxidx_1(iele,ix,1,1)*owe3d(ix)*r125
              deidx_ele(iele) = deidx_ele(iele)                         &
     &                         + dxidx_1(iele,ix,2,1)*owe3d(ix)*r125
              dzidx_ele(iele) = dzidx_ele(iele)                         &
     &                         + dxidx_1(iele,ix,3,1)*owe3d(ix)*r125
!
              dxidy_ele(iele) = dxidy_ele(iele)                         &
     &                         + dxidx_1(iele,ix,1,2)*owe3d(ix)*r125
              deidy_ele(iele) = deidy_ele(iele)                         &
     &                         + dxidx_1(iele,ix,2,2)*owe3d(ix)*r125
              dzidy_ele(iele) = dzidy_ele(iele)                         &
     &                         + dxidx_1(iele,ix,3,2)*owe3d(ix)*r125
!
              dxidz_ele(iele) = dxidz_ele(iele)                         &
     &                         + dxidx_1(iele,ix,1,3)*owe3d(ix)*r125
              deidz_ele(iele) = deidz_ele(iele)                         &
     &                         + dxidx_1(iele,ix,2,3)*owe3d(ix)*r125
              dzidz_ele(iele) = dzidz_ele(iele)                         &
     &                         + dxidx_1(iele,ix,3,3)*owe3d(ix)*r125
            end do
          end do
!$omp end parallel do
!
        end do
!
      end subroutine s_cal_dxidx_ele
!
!-----------------------------------------------------------------------
!
      end module cal_dxidx_ele
