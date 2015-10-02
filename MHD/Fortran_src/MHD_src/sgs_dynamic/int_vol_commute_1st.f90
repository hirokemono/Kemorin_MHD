!
!     module int_vol_commute_1st
!
!     Written by H. Matsui
!
!      subroutine int_vol_commute_grad(iele_fsmp_stack, n_int,          &
!     &          i_filter, i_scalar)
!      subroutine int_vol_commute_div(iele_fsmp_stack, n_int,           &
!     &          i_filter, i_vect)
!      subroutine int_vol_commute_rot(iele_fsmp_stack, n_int,           &
!     &          i_filter, i_vect)
!
!      subroutine int_vol_commute_div_v_flux(iele_fsmp_stack, n_int,    &
!     &          i_filter, i_flux, i_vect, i_scalar)
!      subroutine int_vol_commute_div_m_flux(iele_fsmp_stack, n_int,    &
!     &          i_filter, i_flux, i_vect)
!      subroutine int_vol_commute_induct_t(iele_fsmp_stack, n_int,      &
!     &          i_filter, i_flux, i_v, i_b)
!
      module int_vol_commute_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
      use m_phys_constants
      use m_finite_element_matrix
      use m_int_vol_data
!
      use fem_skv_commute_err_diffs_1
      use cal_skv_to_ff_smp_1st
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_commute_grad(iele_fsmp_stack, n_int,           &
     &          i_filter, i_scalar)
!
      use nodal_fld_2_each_ele_1st
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_filter, n_int
      integer(kind = kint), intent(in) :: i_scalar
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
       do k2 = 1, ele1%nnod_4_ele
!
! --------- set temperature at each node in an element
!
        call scalar_phys_2_each_element(k2, i_scalar, fem1_wk%scalar_1)
!
        call fem_skv_commute_err_grad_1(iele_fsmp_stack,                &
     &      n_int, k2, i_filter, fem1_wk%scalar_1, fem1_wk%sk6)
       end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_commute_grad
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_commute_div(iele_fsmp_stack, n_int,            &
     &          i_filter, i_vect)
!
      use nodal_fld_2_each_ele_1st
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_filter, n_int
      integer(kind = kint), intent(in) :: i_vect
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_scalar, fem1_wk%sk6)
      do k2 = 1, ele1%nnod_4_ele
!
! --------- set temperature at each node in an element
!
        call vector_phys_2_each_element(k2, i_vect, vect_e)
        call fem_skv_commute_err_div_1(iele_fsmp_stack,                 &
     &      n_int, k2, i_filter, vect_e, fem1_wk%sk6)
       end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_commute_div
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_commute_rot(iele_fsmp_stack, n_int,            &
     &          i_filter, i_vect)
!
      use nodal_fld_2_each_ele_1st
!
       integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
       integer(kind = kint), intent(in) :: i_filter, n_int
       integer(kind = kint), intent(in) :: i_vect
!
       integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, fem1_wk%sk6)
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element(k2, i_vect, vect_e)
        call fem_skv_commute_err_rot_1(iele_fsmp_stack,                 &
     &      n_int, k2, i_filter, vect_e, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_commute_rot
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_commute_div_v_flux(iele_fsmp_stack, n_int,     &
     &          i_filter, i_flux, i_vect, i_scalar)
!
      use m_node_phys_data
      use sgs_terms_2_each_ele
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_filter, n_int
      integer(kind = kint), intent(in) :: i_flux, i_vect, i_scalar
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_scalar, fem1_wk%sk6)
      do k2 = 1, ele1%nnod_4_ele
        call SGS_v_flux_2_each_element                                  &
     &     (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,        &
     &      ele1%istack_ele_smp, k2, nod_fld1%ntot_phys,                &
     &      i_vect, i_scalar, i_flux, nod_fld1%d_fld, vect_e)
        call fem_skv_commute_err_div_1(iele_fsmp_stack,                 &
     &      n_int, k2, i_filter, vect_e, fem1_wk%sk6)
       end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_commute_div_v_flux
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_commute_div_m_flux(iele_fsmp_stack, n_int,     &
     &          i_filter, i_flux, i_vect)
!
      use m_node_phys_data
      use sgs_terms_2_each_ele
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_filter, n_int
      integer(kind = kint), intent(in) :: i_flux, i_vect
!
      integer(kind=kint) :: k2
!
! -------- loop for shape function for the phsical values
!
      call reset_sk6(n_vector, fem1_wk%sk6)
      do k2 = 1, ele1%nnod_4_ele
        call SGS_m_flux_2_each_element                                  &
     &     (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,        &
     &      ele1%istack_ele_smp, k2, nod_fld1%ntot_phys,                &
     &      i_vect, i_flux, nod_fld1%d_fld, tensor_e)
        call fem_skv_commute_err_div_tsr_1(iele_fsmp_stack,             &
     &      n_int, k2, i_filter, tensor_e, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_commute_div_m_flux
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_commute_induct_t(iele_fsmp_stack, n_int,       &
     &          i_filter, i_flux, i_v, i_b)
!
      use m_node_phys_data
      use sgs_terms_2_each_ele
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_filter, n_int
      integer(kind = kint), intent(in) :: i_flux, i_v, i_b
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, fem1_wk%sk6)
      do k2 = 1, ele1%nnod_4_ele
        call SGS_induct_2_each_element                                  &
     &     (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,        &
     &      ele1%istack_ele_smp, k2, nod_fld1%ntot_phys,                &
     &      i_b, i_v, i_flux, nod_fld1%d_fld, vect_e)
        call fem_skv_commute_err_div_ast_1(iele_fsmp_stack,             &
     &      n_int, k2, i_filter, vect_e, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_commute_induct_t
!
!-----------------------------------------------------------------------
!
      end module int_vol_commute_1st
