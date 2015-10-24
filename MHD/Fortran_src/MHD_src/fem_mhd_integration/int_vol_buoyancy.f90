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
!     &          i_source, ak_buo, ncomp_ele, ie_upw, d_ele)
!
      module int_vol_buoyancy_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
!
      use m_sorted_node
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
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
      use gravity_vec_each_ele_1st
      use cal_skv_to_ff_smp
      use fem_skv_nodal_field_type
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_source
      real(kind = kreal), intent(in) :: ak_buo(ele1%numele)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call set_gravity_vec_each_ele_1st(k2, i_source,                 &
     &      ak_buo, fem1_wk%vector_1)
        call fem_skv_vector_type(iele_fsmp_stack, n_int, k2,            &
     &      ele1, jac1_3d_q, fem1_wk%vector_1, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_buoyancy_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_buoyancy_upw(iele_fsmp_stack, n_int,           &
     &          i_source, ak_buo, ncomp_ele, ie_upw, d_ele)
!
      use gravity_vec_each_ele_1st
      use cal_skv_to_ff_smp
      use fem_skv_nodal_fld_upw_type
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_source
      real(kind = kreal), intent(in) :: ak_buo(ele1%numele)
!
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call set_gravity_vec_each_ele_1st(k2, i_source,                 &
     &      ak_buo, fem1_wk%vector_1)
        call fem_skv_vector_field_upwind(iele_fsmp_stack, n_int, k2,    &
     &      d_ele(1,ie_upw), ele1, jac1_3d_q,                           &
     &      fem1_wk%vector_1, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_buoyancy_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_buoyancy_1st
