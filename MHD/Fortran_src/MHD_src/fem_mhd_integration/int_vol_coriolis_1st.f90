!
!     module int_vol_coriolis_1st
!
!     numerical integration for finite elememt equations of momentum
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine int_vol_coriolis_pg(iele_fsmp_stack, n_int)
!      subroutine int_vol_coriolis_upw(iele_fsmp_stack, n_int, ie_upw)
!
      module int_vol_coriolis_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_phys_constants
!
      use m_node_phys_address
      use m_physical_property
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_coriolis_pg(iele_fsmp_stack, n_int)
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use nodal_fld_cst_to_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_nonlinear_1st
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, nnod_4_ele
        call vector_cst_phys_2_each_ele(k2, iphys%i_velo,               &
     &      coef_cor, velo_1)
        call fem_skv_coriolis_1st(iele_fsmp_stack, n_int, k2,           &
     &      velo_1, angular, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_coriolis_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_coriolis_upw(iele_fsmp_stack, n_int, ie_upw)
!
      use m_element_phys_data
      use m_finite_element_matrix
      use m_int_vol_data
!
      use nodal_fld_cst_to_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_nonlinear_upw_1st
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: ie_upw
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, nnod_4_ele
          call vector_cst_phys_2_each_ele(k2, iphys%i_velo,             &
     &        coef_cor, velo_1)
          call fem_skv_coriolis_upw_1st(iele_fsmp_stack, n_int, k2,     &
     &         velo_1, angular, d_ele(1,ie_upw), sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_coriolis_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_coriolis_1st
