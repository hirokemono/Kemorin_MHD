!
!     module set_minmax_4_each_2nd_ele
!
      module set_minmax_4_each_2nd_ele
!
!     written by H. Matsui on Aug., 2006
!
      use m_precision
!
      implicit  none
!
!      subroutine s_set_minmax_sph_each_2nd_ele
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_minmax_sph_each_2nd_ele
!
      use m_constants
      use m_machine_parameter
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_data_4_interpolate_org
!
      integer(kind = kint) :: ip, ist, ied, iele, inod, k1
      real(kind = kreal) :: rtmp
      real(kind = kreal) :: pi
!
      pi = four * atan(one)
!
!
!$omp parallel do private(ist,ied,iele,inod,k1,rtmp)
      do ip = 1, np_smp
        ist = ele_2nd%istack_ele_smp(ip-1) + 1
        ied = ele_2nd%istack_ele_smp(ip)
        do k1 = 2, ele_2nd%nnod_4_ele
          do iele = ist, ied
            inod = ele_2nd%ie(iele,k1)
            if (     xx_2nd(inod,1) .ge. 0.0d0                          &
     &         .and. xx_2nd(inod,2) .ge. 0.0d0) then
              iflag_meridian_x(iele) = iflag_meridian_x(iele) + 1
            else if (xx_2nd(inod,1) .ge. 0.0d0                          &
     &         .and. xx_2nd(inod,2) .lt. 0.0d0) then
              iflag_meridian_x(iele) = iflag_meridian_x(iele) + 100
            end if
          end do
        end do
!
        do iele = ist, ied
          if (    iflag_meridian_x(iele) .gt. 100                       &
     &      .and. mod(iflag_meridian_x(iele),100) .gt. 0) then
            iflag_meridian_x(iele) = 1
          else
            iflag_meridian_x(iele) = 0
          end if
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(ist,ied,iele,inod,k1,rtmp)
      do ip = 1, np_smp
        ist = ele_2nd%istack_ele_smp(ip-1) + 1
        ied = ele_2nd%istack_ele_smp(ip)
!
        do iele = ist, ied
          inod = ele_2nd%ie(iele,1)
          min_sph_each_ele(iele,1) = radius_2nd(inod)
          max_sph_each_ele(iele,1) = radius_2nd(inod)
          min_sph_each_ele(iele,2) = theta_2nd(inod)
          max_sph_each_ele(iele,2) = theta_2nd(inod)
          min_sph_each_ele(iele,3) = phi_2nd(inod)
          max_sph_each_ele(iele,3) = phi_2nd(inod)
        end do
!
!
        do k1 = 2, ele_2nd%nnod_4_ele
          do iele = ist, ied
            inod = ele_2nd%ie(iele,k1)
            min_sph_each_ele(iele,1)                                    &
     &         = min(min_sph_each_ele(iele,1),radius_2nd(inod) )
            max_sph_each_ele(iele,1)                                    &
     &         = max(max_sph_each_ele(iele,1),radius_2nd(inod) )
            min_sph_each_ele(iele,2)                                    &
     &         = min(min_sph_each_ele(iele,2),theta_2nd(inod) )
            max_sph_each_ele(iele,2)                                    &
     &         = max(max_sph_each_ele(iele,2),theta_2nd(inod) )
            min_sph_each_ele(iele,3)                                    &
     &         = min(min_sph_each_ele(iele,3),phi_2nd(inod) )
            max_sph_each_ele(iele,3)                                    &
     &         = max(max_sph_each_ele(iele,3),phi_2nd(inod) )
          end do
        end do
!
        do iele = ist, ied
          if(ele_2nd%theta_ele(iele).lt.min_sph_each_ele(iele,2)) then
            min_sph_each_ele(iele,2) = 0.0d0
            min_sph_each_ele(iele,3) = 0.0d0
            max_sph_each_ele(iele,3) = two * pi
          else if                                                       &
     &       (ele_2nd%theta_ele(iele).gt.max_sph_each_ele(iele,2)) then
            max_sph_each_ele(iele,2) = pi
            min_sph_each_ele(iele,3) = 0.0d0
            max_sph_each_ele(iele,3) = two * pi
          end if
!
          if (iflag_meridian_x(iele) .eq. 1) then
            min_sph_each_ele(iele,3) = 0.0d0
            max_sph_each_ele(iele,3) = two * pi
          end if
!
        end do
!
      end do
!$omp end parallel do
!
      end subroutine s_set_minmax_sph_each_2nd_ele
!
!-----------------------------------------------------------------------
!
      end module set_minmax_4_each_2nd_ele
