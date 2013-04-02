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
!
      use m_control_parameter
      use m_geometry_parameter
      use m_phys_constants
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
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
      do imulti = 2, num_multi_pass
!
        call cal_ff_smp_2_vector(ff_nl(1,1), ff_nl_smp(1,1,1), ml)
        call nod_vector_send_recv( ff_nl(1,1) )
!
        call int_vol_multi_pass_vector(iele_smp_stack)
      end do
!
      end subroutine int_multi_pass_vector
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_scalar
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
      do imulti = 2, num_multi_pass
!
        call cal_ff_smp_2_scalar(ff_nl(1,1), ff_nl_smp(1,1,1), ml)
        call nod_scalar_send_recv( ff_nl(1,1) )
!
        call int_vol_multi_pass_scalar(iele_smp_stack)
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
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
      do imulti = 2, num_multi_pass
!
        call cal_ff_smp_2_vector(ff_nl(1,1), ff_nl_smp(1,1,1), ml_fl)
        call nod_vector_send_recv( ff_nl(1,1) )
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
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
      do imulti = 2, num_multi_pass
!
        call cal_ff_smp_2_scalar(ff_nl(1,1), ff_nl_smp(1,1,1), ml_fl)
        call nod_scalar_send_recv( ff_nl(1,1) )
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
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
      do imulti = 2, num_multi_pass
!
        call cal_ff_smp_2_vector(ff_nl(1,1), ff_nl_smp(1,1,1), ml_cd)
        call nod_vector_send_recv( ff_nl(1,1) )
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
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
      do imulti = 2, num_multi_pass
!
        call cal_ff_smp_2_scalar(ff_nl(1,1), ff_nl_smp(1,1,1), ml_cd)
        call nod_scalar_send_recv( ff_nl(1,1) )
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
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_vector(ff_nl(1,1), ff_nl_smp(1,1,1), ml_ins)
        call nod_vector_send_recv( ff_nl(1,1) )
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
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_scalar(ff_nl(1,1), ff_nl_smp(1,1,1), ml_ins)
        call nod_scalar_send_recv( ff_nl(1,1) )
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
      use m_int_vol_data
      use nodal_fld_2_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use cal_for_ffs
      use fem_skv_nodal_field_1st
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind = kint) :: k2
!
!
      call reset_sk6(n_scalar)
!
      do k2 = 1, nnod_4_ele
        call scalar_2_each_element(k2, ff_nl(1,1), phi_e)
        call fem_skv_scalar_1st(iele_fsmp_stack, intg_point_t_evo, k2,  &
     &      phi_e, sk6)
      end do
!
      call sub1_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
      call cal_multi_pass_2_ff_smp(n_scalar, ff_nl_smp, ff_m_smp)
!
      end subroutine int_vol_multi_pass_scalar
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_multi_pass_vector(iele_fsmp_stack)
!
      use m_int_vol_data
      use nodal_fld_2_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use cal_for_ffs
      use fem_skv_nodal_field_1st
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
      do k2 = 1, nnod_4_ele
        call vector_2_each_element(k2, ff_nl(1,1), vect_e)
        call fem_skv_vector_1st(iele_fsmp_stack, intg_point_t_evo, k2,  &
     &      vect_e, sk6)
      end do
!
      call sub3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
      call cal_multi_pass_2_ff_smp(n_vector, ff_nl_smp, ff_m_smp)
!
      end subroutine int_vol_multi_pass_vector
!
!-----------------------------------------------------------------------
!
      end module int_multi_pass
