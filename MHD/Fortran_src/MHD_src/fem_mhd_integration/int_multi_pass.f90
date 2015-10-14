!
!      module int_multi_pass
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on Oct. 2005
!
!      subroutine int_multi_pass_vector
!      subroutine int_multi_pass_scalar
!      subroutine int_multi_pass_vector_fl
!      subroutine int_multi_pass_scalar_fl
!      subroutine int_multi_pass_vector_cd
!      subroutine int_multi_pass_scalar_cd
!      subroutine int_multi_pass_vector_ins
!      subroutine int_multi_pass_scalar_ins
!
      module int_multi_pass
!
      use m_precision
      use m_constants
!
      use m_control_parameter
      use m_geometry_data
      use m_phys_constants
      use m_sorted_node
      use m_finite_element_matrix
!
      use cal_ff_smp_to_ffs
      use nodal_vector_send_recv
!
      implicit none
!
      private :: int_vol_multi_pass_scalar
      private :: int_vol_multi_pass_vector
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_vector
!
      use m_sorted_node
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(node1%numnod, f1_nl)
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_vector(node1, rhs_tbl1,                       &
     &      ff_nl_smp(1,1,1), ml, n_vector, ione, f1_nl%ff)
        call nod_vector_send_recv(f1_nl%ff)
!
        call int_vol_multi_pass_vector(ele1%istack_ele_smp)
      end do
!
      end subroutine int_multi_pass_vector
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_scalar
!
      use m_sorted_node
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(node1%numnod, f1_nl)
      do imulti = 2, num_multi_pass
!
        call cal_ff_smp_2_scalar(node1, rhs_tbl1,                       &
     &      ff_nl_smp(1,1,1), ml, n_vector, ione, f1_nl%ff)
        call nod_scalar_send_recv( f1_nl%ff(1:node1%numnod,1) )
!
        call int_vol_multi_pass_scalar(ele1%istack_ele_smp)
      end do
!
      end subroutine int_multi_pass_scalar
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_vector_fl
!
      use m_geometry_data_MHD
      use m_sorted_node
      use m_int_vol_data
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(node1%numnod, f1_nl)
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_vector(node1, rhs_tbl1,                       &
     &      ff_nl_smp(1,1,1), mhd_fem1_wk%ml_fl, n_vector,              &
     &      ione, f1_nl%ff)
        call nod_vector_send_recv(f1_nl%ff)
!
        call int_vol_multi_pass_vector(iele_fl_smp_stack)
      end do
!
      end subroutine int_multi_pass_vector_fl
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_scalar_fl
!
      use m_geometry_data_MHD
      use m_sorted_node
      use m_int_vol_data
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(node1%numnod, f1_nl)
      do imulti = 2, num_multi_pass
!
        call cal_ff_smp_2_scalar(node1, rhs_tbl1,                       &
     &      ff_nl_smp(1,1,1), mhd_fem1_wk%ml_fl, n_vector,              &
     &      ione, f1_nl%ff)
        call nod_scalar_send_recv( f1_nl%ff(1:node1%numnod,1) )
!
        call int_vol_multi_pass_scalar(iele_fl_smp_stack)
      end do
!
      end subroutine int_multi_pass_scalar_fl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_vector_cd
!
      use m_geometry_data_MHD
      use m_sorted_node
      use m_int_vol_data
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(node1%numnod, f1_nl)
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_vector(node1, rhs_tbl1,                       &
     &      ff_nl_smp(1,1,1), mhd_fem1_wk%ml_cd, n_vector,              &
     &      ione, f1_nl%ff)
        call nod_vector_send_recv(f1_nl%ff)
!
        call int_vol_multi_pass_vector(iele_cd_smp_stack)
      end do
!
!
      end subroutine int_multi_pass_vector_cd
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_scalar_cd
!
      use m_geometry_data_MHD
      use m_sorted_node
      use m_int_vol_data
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(node1%numnod, f1_nl)
      do imulti = 2, num_multi_pass
!
        call cal_ff_smp_2_scalar(node1, rhs_tbl1,                       &
     &      ff_nl_smp(1,1,1), mhd_fem1_wk%ml_cd, n_vector,              &
     &      ione, f1_nl%ff)
        call nod_scalar_send_recv( f1_nl%ff(1:node1%numnod,1) )
!
        call int_vol_multi_pass_scalar(iele_cd_smp_stack)
      end do
!
      end subroutine int_multi_pass_scalar_cd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_vector_ins
!
      use m_geometry_data_MHD
      use m_sorted_node
      use m_int_vol_data
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(node1%numnod, f1_nl)
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_vector(node1, rhs_tbl1, ff_nl_smp(1,1,1),     &
     &      mhd_fem1_wk%ml_ins, n_vector, ione, f1_nl%ff)
        call nod_vector_send_recv(f1_nl%ff)
!
        call int_vol_multi_pass_vector(iele_ins_smp_stack)
      end do
!
      end subroutine int_multi_pass_vector_ins
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_scalar_ins
!
      use m_geometry_data_MHD
      use m_sorted_node
      use m_int_vol_data
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(node1%numnod, f1_nl)
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_scalar(node1, rhs_tbl1, ff_nl_smp(1,1,1),     &
     &      mhd_fem1_wk%ml_ins, n_vector, ione, f1_nl%ff)
        call nod_scalar_send_recv( f1_nl%ff(1:node1%numnod,1) )
!
        call int_vol_multi_pass_scalar(iele_ins_smp_stack)
      end do
!
      end subroutine int_multi_pass_scalar_ins
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_multi_pass_scalar(iele_fsmp_stack)
!
      use m_jacobians
      use m_int_vol_data
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use cal_for_ffs
      use fem_skv_nodal_field_type
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
      do k2 = 1, ele1%nnod_4_ele
        call scalar_2_each_element(node1, ele1,                         &
     &      k2, f1_nl%ff(1:node1%numnod,1), fem1_wk%scalar_1)
        call fem_skv_scalar_type(iele_fsmp_stack, intg_point_t_evo, k2, &
     &      ele1, jac1_3d_q, fem1_wk%scalar_1, fem1_wk%sk6)
      end do
!
      call sub1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_nl_smp)
      call cal_multi_pass_2_ff_smp                                      &
     &   (node1%max_nod_smp, node1%istack_nod_smp,                      &
     &    n_scalar, ff_nl_smp, mhd_fem1_wk%ff_m_smp)
!
      end subroutine int_vol_multi_pass_scalar
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_multi_pass_vector(iele_fsmp_stack)
!
      use m_jacobians
      use m_int_vol_data
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use cal_for_ffs
      use fem_skv_nodal_field_type
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_2_each_element(node1, ele1,                         &
     &      k2, f1_nl%ff, fem1_wk%vector_1)
        call fem_skv_vector_type(iele_fsmp_stack, intg_point_t_evo, k2, &
     &      ele1, jac1_3d_q, fem1_wk%vector_1, fem1_wk%sk6)
      end do
!
      call sub3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_nl_smp)
      call cal_multi_pass_2_ff_smp                                      &
     &   (node1%max_nod_smp, node1%istack_nod_smp,                      &
     &    n_vector, ff_nl_smp, mhd_fem1_wk%ff_m_smp)
!
      end subroutine int_vol_multi_pass_vector
!
!-----------------------------------------------------------------------
!
      end module int_multi_pass
