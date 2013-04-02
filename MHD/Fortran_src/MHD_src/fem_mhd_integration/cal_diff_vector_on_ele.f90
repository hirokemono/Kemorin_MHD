!
!     module cal_diff_vector_on_ele
!
!     Written by H.Matsui
!
!      subroutine diff_velocity_on_ele
!      subroutine diff_magne_on_ele
!      subroutine diff_filter_v_on_ele
!      subroutine diff_filter_b_on_ele
!
      module cal_diff_vector_on_ele
!
      use m_precision
!
      use m_int_vol_data
!
      implicit none
!
      private :: diff_vector_on_ele
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine diff_velocity_on_ele
!
      use m_geometry_data_MHD
      use m_node_phys_address
!
      call diff_vector_on_ele(iele_fl_smp_stack, i_dvx, iphys%i_velo)
!
      end subroutine diff_velocity_on_ele
!
! -----------------------------------------------------------------------
!
      subroutine diff_magne_on_ele
!
      use m_geometry_data_MHD
      use m_node_phys_address
!
      call diff_vector_on_ele(iele_cd_smp_stack, i_dbx, iphys%i_magne)
!
      end subroutine diff_magne_on_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine diff_filter_v_on_ele
!
      use m_geometry_data_MHD
      use m_node_phys_address
!
      call diff_vector_on_ele(iele_fl_smp_stack, i_dfvx,                &
     &    iphys%i_filter_velo)
!
      end subroutine diff_filter_v_on_ele
!
! -----------------------------------------------------------------------
!
      subroutine diff_filter_b_on_ele
!
      use m_geometry_data_MHD
      use m_node_phys_address
!
      call diff_vector_on_ele(iele_fl_smp_stack, i_dfbx,                &
     &    iphys%i_filter_magne)
!
      return
      end subroutine diff_filter_b_on_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine diff_vector_on_ele(iele_fsmp_stack, i_diff, i_vec)
!
      use m_control_parameter
      use m_geometry_parameter
      use m_machine_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_finite_element_matrix
      use m_fem_gauss_int_coefs
      use m_jacobians
!
       integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
       integer(kind = kint), intent(in) :: i_diff, i_vec
!
       integer(kind = kint) :: iproc, inod, iele, k1, ii, ix
       integer(kind = kint) :: istart, iend, nd, n_int
!
!
       n_int = intg_point_t_evo
!
       do nd = 1, 9
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
!cdir concur
!cdir nodep
!voption, indep, vec
           do iele = istart, iend
            inod = ie(iele,k1)
!
            dvx(iele,i_diff  ) = dvx(iele,i_diff  )                     &
     &      +   dwx(iele,k1,ix,1) * d_nod(inod,i_vec  )                 &
     &      * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
            dvx(iele,i_diff+1) = dvx(iele,i_diff+1)                     &
     &      +   dwx(iele,k1,ix,2) * d_nod(inod,i_vec  )                 &
     &      * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
            dvx(iele,i_diff+2) = dvx(iele,i_diff+2)                     &
     &      +   dwx(iele,k1,ix,3) * d_nod(inod,i_vec  )                 &
     &      * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
!
            dvx(iele,i_diff+3) = dvx(iele,i_diff+3)                     &
     &      +   dwx(iele,k1,ix,1) * d_nod(inod,i_vec+1)                 &
     &      * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
            dvx(iele,i_diff+4) = dvx(iele,i_diff+4)                     &
     &      +   dwx(iele,k1,ix,2) * d_nod(inod,i_vec+1)                 &
     &      * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
            dvx(iele,i_diff+5) = dvx(iele,i_diff+5)                     &
     &      +   dwx(iele,k1,ix,3) * d_nod(inod,i_vec+1)                 &
     &      * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
!
            dvx(iele,i_diff+6) = dvx(iele,i_diff+6)                     &
     &      +   dwx(iele,k1,ix,1) * d_nod(inod,i_vec+2)                 &
     &      * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
            dvx(iele,i_diff+7) = dvx(iele,i_diff+7)                     &
     &      +   dwx(iele,k1,ix,2) * d_nod(inod,i_vec+2)                 &
     &      * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
            dvx(iele,i_diff+8) = dvx(iele,i_diff+8)                     &
     &      +   dwx(iele,k1,ix,3) * d_nod(inod,i_vec+2)                 &
     &      * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
           end do
          end do
!
         end do
       end do
!$omp end parallel do
!
      end subroutine diff_vector_on_ele
!
! -----------------------------------------------------------------------
!
      end module cal_diff_vector_on_ele
