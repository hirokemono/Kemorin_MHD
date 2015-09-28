!
!      module int_multi_pass_upw
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on Oct. 2005
!
!      subroutine int_multi_pass_vector_upw
!      subroutine int_multi_pass_scalar_upw
!      subroutine int_multi_pass_vector_upm
!      subroutine int_multi_pass_scalar_upm
!      subroutine int_multi_pass_vector_fl_upw
!      subroutine int_multi_pass_scalar_fl_upw
!      subroutine int_multi_pass_vector_fl_upm
!      subroutine int_multi_pass_scalar_fl_upm
!      subroutine int_multi_pass_vector_cd_upm
!      subroutine int_multi_pass_scalar_cd_upm
!
      module int_multi_pass_upw
!
      use m_precision
      use m_constants
!
      use m_control_parameter
      use m_geometry_data
      use m_phys_constants
      use m_finite_element_matrix
!
      use cal_ff_smp_to_ffs
      use nodal_vector_send_recv
!
      implicit none
!
      private :: int_vol_multi_pass_scalar_upw
      private :: int_vol_multi_pass_vector_upw
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_vector_upw
!
      use m_element_phys_data
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
!
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_vector                                        &
     &     (ff_nl_smp(1,1,1), ml, n_vector, ione, ff_nl)
        call nod_vector_send_recv(ff_nl(1,1))
!
        call int_vol_multi_pass_vector_upw(ele1%istack_ele_smp,         &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld)
      end do
!
      end subroutine int_multi_pass_vector_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_scalar_upw
!
      use m_element_phys_data
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
!
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_scalar                                        &
     &     (ff_nl_smp(1,1,1), ml, n_vector, ione, ff_nl)
        call nod_scalar_send_recv( ff_nl(1,1) )
!
        call int_vol_multi_pass_scalar_upw(ele1%istack_ele_smp,         &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld)
      end do
!
      end subroutine int_multi_pass_scalar_upw
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_vector_upm
!
      use m_element_phys_data
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
!
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_vector                                        &
     &     (ff_nl_smp(1,1,1), ml, n_vector, ione, ff_nl)
        call nod_vector_send_recv( ff_nl(1,1) )
!
        call int_vol_multi_pass_vector_upw(ele1%istack_ele_smp,         &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld)
      end do
!
      end subroutine int_multi_pass_vector_upm
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_scalar_upm
!
      use m_element_phys_data
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
      do imulti = 2, num_multi_pass
!
        call cal_ff_smp_2_scalar                                        &
     &     (ff_nl_smp(1,1,1), ml, n_vector, ione, ff_nl)
        call nod_scalar_send_recv( ff_nl(1,1) )
!
        call int_vol_multi_pass_scalar_upw(ele1%istack_ele_smp,         &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld)
      end do
!
      end subroutine int_multi_pass_scalar_upm
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_vector_fl_upw
!
      use m_geometry_data_MHD
      use m_element_phys_data
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
!
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_vector                                        &
     &     (ff_nl_smp(1,1,1), ml_fl, n_vector, ione, ff_nl)
        call nod_vector_send_recv( ff_nl(1,1) )
!
        call int_vol_multi_pass_vector_upw(iele_fl_smp_stack,           &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld)
      end do
!
      end subroutine int_multi_pass_vector_fl_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_scalar_fl_upw
!
      use m_geometry_data_MHD
      use m_element_phys_data
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
!
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_scalar                                        &
     &     (ff_nl_smp(1,1,1), ml_fl, n_vector, ione, ff_nl)
        call nod_scalar_send_recv( ff_nl(1,1) )
!
        call int_vol_multi_pass_scalar_upw(iele_fl_smp_stack,           &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld)
      end do
!
      end subroutine int_multi_pass_scalar_fl_upw
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_vector_fl_upm
!
      use m_geometry_data_MHD
      use m_element_phys_data
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
!
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_vector                                        &
     &     (ff_nl_smp(1,1,1), ml_fl, n_vector, ione, ff_nl)
        call nod_vector_send_recv( ff_nl(1,1) )
!
        call int_vol_multi_pass_vector_upw(iele_fl_smp_stack,           &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld)
      end do
!
      end subroutine int_multi_pass_vector_fl_upm
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_scalar_fl_upm
!
      use m_geometry_data_MHD
      use m_element_phys_data
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
!
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_scalar                                        &
     &     (ff_nl_smp(1,1,1), ml_fl, n_vector, ione, ff_nl)
        call nod_scalar_send_recv( ff_nl(1,1) )
!
        call int_vol_multi_pass_scalar_upw(iele_fl_smp_stack,           &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld)
      end do
!
      end subroutine int_multi_pass_scalar_fl_upm
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_vector_cd_upm
!
      use m_geometry_data_MHD
      use m_element_phys_data
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_vector                                        &
     &     (ff_nl_smp(1,1,1), ml_cd, n_vector, ione, ff_nl)
        call nod_vector_send_recv( ff_nl(1,1) )
!
        call int_vol_multi_pass_vector_upw(iele_cd_smp_stack,           &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld)
      end do
!
      end subroutine int_multi_pass_vector_cd_upm
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_scalar_cd_upm
!
      use m_geometry_data_MHD
      use m_element_phys_data
!
      integer (kind = kint) :: imulti
!
!
      ff_nl = 0.0d0
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_scalar                                        &
     &     (ff_nl_smp(1,1,1), ml_cd, n_vector, ione, ff_nl)
        call nod_scalar_send_recv( ff_nl(1,1) )
!
        call int_vol_multi_pass_scalar_upw(iele_cd_smp_stack,           &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld)
      end do
!
      end subroutine int_multi_pass_scalar_cd_upm
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_multi_pass_scalar_upw                          &
     &         (iele_fsmp_stack, ncomp_ele, ie_up, d_ele)
!
      use m_jacobians
      use m_int_vol_data
      use nodal_fld_2_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use cal_for_ffs
      use fem_skv_nodal_fld_upw_type
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ncomp_ele, ie_up
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
!
      integer (kind = kint) :: k2
!
!
      call reset_sk6(n_scalar)
!
      do k2 = 1, ele1%nnod_4_ele
        call scalar_2_each_element(k2, ff_nl(1,1), phi_e)
        call fem_skv_scalar_field_upwind(iele_fsmp_stack,               &
     &      intg_point_t_evo, k2, d_ele(1,ie_up), ele1, jac1_3d_q,      &
     &      phi_e, sk6)
      end do
!
      call sub1_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
      call cal_multi_pass_2_ff_smp                                      &
     &   (node1%max_nod_smp, node1%istack_nod_smp,                      &
     &    n_scalar, ff_nl_smp, ff_m_smp)
!
      end subroutine int_vol_multi_pass_scalar_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_multi_pass_vector_upw                          &
     &         (iele_fsmp_stack, ncomp_ele, ie_up, d_ele)
!
      use m_jacobians
      use m_int_vol_data
      use nodal_fld_2_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use cal_for_ffs
      use fem_skv_nodal_fld_upw_type
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ncomp_ele, ie_up
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer (kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_2_each_element(k2, ff_nl(1,1), vect_e)
        call fem_skv_vector_field_upwind(iele_fsmp_stack,               &
     &      intg_point_t_evo, k2, d_ele(1,ie_up), ele1, jac1_3d_q,      &
     &      vect_e, sk6)
      end do
!
      call sub3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
      call cal_multi_pass_2_ff_smp                                      &
     &   (node1%max_nod_smp, node1%istack_nod_smp,                      &
     &    n_vector, ff_nl_smp, ff_m_smp)
!
      end subroutine int_vol_multi_pass_vector_upw
!
!-----------------------------------------------------------------------
!
      end module int_multi_pass_upw
