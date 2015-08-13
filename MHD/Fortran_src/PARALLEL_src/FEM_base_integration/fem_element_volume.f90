!
!     module fem_element_volume
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on Aug., 2006
!        Modified by H. Matsui on June, 2007
!
!       subroutine fem_element_volume_pg(n_int)
!
      module fem_element_volume
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
       subroutine fem_element_volume_pg(n_int)
!
      use m_machine_parameter
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_jacobians
!
      integer (kind=kint), intent(in) :: n_int
!
      integer (kind=kint) :: ip, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp workshare
      volume_ele(1:ele1%numele) = 0.0d0
!$omp end workshare
!
!$omp parallel do private(iele,ii,ix,istart,iend) 
       do ip = 1, np_smp
         istart = ele1%istack_ele_smp(ip-1)+1
         iend =   ele1%istack_ele_smp(ip)
!
         do ii=1, n_int * n_int * n_int
           ix = int_start3(n_int) + ii
           do iele = istart, iend
             volume_ele(iele) = volume_ele(iele)                        &
     &                          + xjac(iele,ix)*owe3d(ix)
           end do
         end do
!
!
         do iele = istart, iend
           if (volume_ele(iele).eq.0.0d0) then
             a_vol_ele(iele) = 1.0d60
           else
             a_vol_ele(iele) = 1.0d0 / volume_ele(iele)
           end if
         end do
       end do
!$omp end parallel do
!
       end subroutine fem_element_volume_pg
!
!-----------------------------------------------------------------------
!
      end module fem_element_volume
