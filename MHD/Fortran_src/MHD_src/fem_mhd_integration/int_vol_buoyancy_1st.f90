!
!     module int_vol_buoyancy_1st
!
!     numerical integration for finite elememt equations of momentum
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!        modified by H. Matsui on Aug., 2012
!
!      subroutine int_vol_buoyancy_pg(iele_fsmp_stack, n_int,           &
!     &          i_source, ak_buo)
!      subroutine int_vol_buoyancy_upw(iele_fsmp_stack, n_int,          &
!     &          i_source, ak_buo, ie_upw)
!
      module int_vol_buoyancy_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_buoyancy_pg(iele_fsmp_stack, n_int,            &
     &          i_source, ak_buo)
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use gravity_vec_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_nodal_field_1st
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_source
      real(kind = kreal), intent(in) :: ak_buo(numele)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, nnod_4_ele
        call set_gravity_vec_each_ele_1st(k2, i_source,                 &
     &      ak_buo, vect_e)
        call fem_skv_vector_1st(iele_fsmp_stack, n_int, k2,             &
     &     vect_e, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_buoyancy_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_buoyancy_upw(iele_fsmp_stack, n_int,           &
     &          i_source, ak_buo, ie_upw)
!
      use m_element_phys_data
      use m_finite_element_matrix
      use m_int_vol_data
!
      use gravity_vec_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_nodal_fld_upw_1st
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_source, ie_upw
      real(kind = kreal), intent(in) :: ak_buo(numele)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, nnod_4_ele
        call set_gravity_vec_each_ele_1st(k2, i_source,                 &
     &      ak_buo, vect_e)
        call fem_skv_vector_field_upw_1st(iele_fsmp_stack, n_int, k2,   &
     &      d_ele(1,ie_upw), vect_e, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_buoyancy_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_buoyancy_1st
