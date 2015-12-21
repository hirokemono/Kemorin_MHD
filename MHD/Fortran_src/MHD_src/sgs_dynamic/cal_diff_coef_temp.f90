!
!     module cal_diff_coef_temp
!
!     Written by H. Matsui
!
!     subroutine s_cal_diff_coef_temp(layer_tbl)
!        type(layering_tbl), intent(in) :: layer_tbl
!
      module cal_diff_coef_temp
!
      use m_precision
!
      use t_layering_ele_list
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_diff_coef_temp(layer_tbl)
!
      use m_nod_comm_table
      use m_geometry_data
      use m_machine_parameter
      use m_control_parameter
      use m_node_phys_data
      use m_SGS_address
      use m_phys_constants
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_filtering_vectors
      use cal_gradient
      use commute_error_scalar
      use cal_model_diff_coefs
      use set_boundary_scalars
      use nod_phys_send_recv
      use clear_work_4_dynamic_model
!
      type(layering_tbl), intent(in) :: layer_tbl
!
!
!    reset model coefficients
!
      call reset_diff_model_coefs(iak_diff_t, ele1%istack_ele_smp)
      call s_clear_work_4_dynamic_model
!
!   take gradient of filtered temperature (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_gradent_in_fluid',          &
     &        iphys%i_sgs_simi, iphys%i_filter_temp
      call cal_gradent_in_fluid(iflag_temp_supg,                        &
     &    iphys%i_sgs_simi, iphys%i_filter_temp)
!
!   take gradient of temperature (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_gradent_in_fluid',          &
     &                     iphys%i_sgs_grad, iphys%i_sgs_temp
      call cal_gradent_in_fluid(iflag_temp_supg,                        &
     &    iphys%i_sgs_grad, iphys%i_sgs_temp)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector(iphys%i_sgs_grad, iphys%i_sgs_grad)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_vectors(node1, nod_fld1,                      &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_vector, iphys%i_sgs_simi)
!
!    modeled commutative error by second filter ( to iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'cal_commute_error_f_temp', iphys%i_sgs_grad_f
      call cal_commute_error_f_temp(ifilter_4delta, iphys%i_sgs_grad_f)
!
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad_f, node1, nod_comm, nod_fld1)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_vector, iphys%i_sgs_grad_f)
!
!    modeled commutative error by grid filter ( to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_commute_error_temp', iphys%i_sgs_grad
      call cal_commute_error_temp(ifilter_2delta, iphys%i_sgs_grad)
!
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad, node1, nod_comm, nod_fld1)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector(iphys%i_sgs_grad, iphys%i_sgs_grad)
!
!      call check_nodal_data                                            &
!     &   (my_rank, nod_fld1, n_vector, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_vector, iak_diff_t, icomp_diff_t
      call cal_diff_coef_fluid(layer_tbl,                               &
     &    n_vector, iak_diff_t, icomp_diff_t, intg_point_t_evo)
!
      iflag_diff_coefs(iak_diff_t) = 1
!
      end subroutine s_cal_diff_coef_temp
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_temp
