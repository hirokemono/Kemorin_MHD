!
!      module cal_dxidx_ele
!
!        programmed by H.Matsui on Nov., 2008
!
!      subroutine cal_dxidx_ele_type(dx_ele)
!!
      module cal_dxidx_ele
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: s_cal_dxidx_ele
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_dxidx_ele_type(dx_ele)
!
      use m_geometry_data
      use t_filter_dxdxi
!
      type(dxidx_direction_type), intent(inout) :: dx_ele
!
!
      call s_cal_dxidx_ele(ele1%numele,                                 &
     &      dx_ele%dxi%df_dx, dx_ele%dxi%df_dy, dx_ele%dxi%df_dz,       &
     &      dx_ele%dei%df_dx, dx_ele%dei%df_dy, dx_ele%dei%df_dz,       &
     &      dx_ele%dzi%df_dx, dx_ele%dzi%df_dy, dx_ele%dzi%df_dz)
!
      end subroutine cal_dxidx_ele_type
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_dxidx_ele(nele,                                  &
     &          dxidx_ele, dxidy_ele, dxidz_ele,                        &
     &          deidx_ele, deidy_ele, deidz_ele,                        &
     &          dzidx_ele, dzidy_ele, dzidz_ele)
!
      use m_geometry_data
      use m_machine_parameter
      use m_fem_gauss_int_coefs
      use m_jacobians
!
      integer(kind = kint), intent(in) :: nele
!
!
      real(kind=kreal), intent(inout) :: dxidx_ele(nele)
      real(kind=kreal), intent(inout) :: deidx_ele(nele)
      real(kind=kreal), intent(inout) :: dzidx_ele(nele)
      real(kind=kreal), intent(inout) :: dxidy_ele(nele)
      real(kind=kreal), intent(inout) :: deidy_ele(nele)
      real(kind=kreal), intent(inout) :: dzidy_ele(nele)
      real(kind=kreal), intent(inout) :: dxidz_ele(nele)
      real(kind=kreal), intent(inout) :: deidz_ele(nele)
      real(kind=kreal), intent(inout) :: dzidz_ele(nele)
!
      integer (kind = kint) :: ii, ix, i0
      integer (kind=kint) :: iele, ip, ist, ied
!
!
!$omp parallel do private(ist,ied,iele)
      do ip = 1, np_smp
        ist = ele1%istack_ele_smp(ip-1) + 1
        ied = ele1%istack_ele_smp(ip)
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
            ist = ele1%istack_ele_smp(ip-1) + 1
            ied = ele1%istack_ele_smp(ip)
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
