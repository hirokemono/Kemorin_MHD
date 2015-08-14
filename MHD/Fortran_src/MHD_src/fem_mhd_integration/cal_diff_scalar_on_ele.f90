!
!     module cal_diff_scalar_on_ele
!
!     Written by H.Matsui
!
!      subroutine diff_temp_on_ele
!      subroutine diff_filter_t_on_ele
!
      module cal_diff_scalar_on_ele
!
      use m_precision
!
      use m_int_vol_data
!
      implicit none
!
      private :: diff_scalar_on_ele
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine diff_temp_on_ele
!
      use m_geometry_data
      use m_node_phys_address
!
      call diff_scalar_on_ele(ele1%numele, ele1%nnod_4_ele, ele1%ie,    &
     &    a_vol_ele, ele1%istack_ele_smp, i_dtx, iphys%i_temp)
!
      end subroutine diff_temp_on_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine diff_filter_t_on_ele
!
      use m_geometry_data
      use m_node_phys_address
!
      call diff_scalar_on_ele(ele1%numele, ele1%nnod_4_ele, ele1%ie,    &
     &    a_vol_ele, ele1%istack_ele_smp, i_dftx, iphys%i_filter_temp)
!
      end subroutine diff_filter_t_on_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine diff_scalar_on_ele(numele, nnod_4_ele, ie, a_vol_ele,  &
     &          iele_fsmp_stack, i_diff, i_scalar)
!
      use m_control_parameter
      use m_machine_parameter
      use m_node_phys_data
      use m_fem_gauss_int_coefs
      use m_jacobians
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_diff, i_scalar
      real(kind = kreal), intent(in) :: a_vol_ele(numele)
!
      integer(kind = kint) :: iproc, inod, iele, k1, ii, ix
      integer(kind = kint) :: istart, iend, nd, n_int
!
!
       n_int = intg_point_t_evo
!
       do nd = 1, 3
!$omp parallel do
        do iele = 1, numele
         dvx(iele,i_diff+nd-1) = 0.0d0
        end do
!$omp end parallel do
       end do
!
! --------- lead average gradient of magntic field in a element
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend) 
      do iproc = 1, np_smp
       istart = iele_fsmp_stack(iproc-1)+1
       iend = iele_fsmp_stack(iproc)
!
        do k1 = 1, nnod_4_ele
         do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
!
!poption parallel
!voption, indep, vec
            do iele = istart, iend
              inod = ie(iele,k1)
!
              dvx(iele,i_diff  ) = dvx(iele,i_diff  )                   &
     &          +   dwx(iele,k1,ix,1) * d_nod(inod,i_scalar)            &
     &           * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
              dvx(iele,i_diff+1) = dvx(iele,i_diff+1)                   &
     &          +   dwx(iele,k1,ix,2) * d_nod(inod,i_scalar)            &
     &           * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
              dvx(iele,i_diff+2) = dvx(iele,i_diff+2)                   &
     &          +   dwx(iele,k1,ix,3) * d_nod(inod,i_scalar)            &
     &           * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
            end do
          end do
!
         end do
       end do
!$omp end parallel do
!
      end subroutine diff_scalar_on_ele
!
! -----------------------------------------------------------------------
!
      end module cal_diff_scalar_on_ele
