!correlation_all_layerd_data.f90
!      module correlation_all_layerd_data
!
      module correlation_all_layerd_data
!
!     Written by H. Matsui on Nov., 2009
!
      use m_precision
      use m_machine_parameter
      use m_parallel_var_dof
      use m_all_layerd_correlate
      use m_layering_ele_list
      use m_work_layer_correlate
!
      implicit none
!
      private :: int_vol_rms_ave_all_layer, int_vol_dev_cor_all_layer
      private :: cal_ave_rms_ratio_layers, take_sqrt_rms_data
!
!      subroutine s_correlation_all_layerd_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_correlation_all_layerd_data
!
      use m_node_phys_data
      use cal_layerd_ave_correlate
!
!
      call int_vol_rms_ave_all_layer
      call sum_layerd_averages
!
      if(iflag_debug .gt. 0) write(*,*) 'divide_layers_ave_by_vol'
      call divide_layers_ave_by_vol(num_tot_nod_phys,                   &
     &    ave_ref(1,1), ave_tgt(1,1), rms_ref(1,1), rms_tgt(1,1),       &
     &    rms_ratio(1,1))
!
      call cal_ave_rms_ratio_layers
!
!
      if(iflag_debug .gt. 0) write(*,*) 'int_vol_dev_cor_all_layer'
      call int_vol_dev_cor_all_layer
!
      if(iflag_debug .gt. 0) write(*,*) 'sum_layerd_correlation'
      call sum_layerd_correlation
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_layered_correlation'
      call cal_layered_correlation(num_tot_nod_phys, cor_data(1,1))
!
      call take_sqrt_rms_data
!
      end subroutine s_correlation_all_layerd_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine  int_vol_rms_ave_all_layer
!
      use m_geometry_parameter
      use m_node_phys_data
      use m_2nd_geometry_param
      use m_2nd_phys_data
      use m_fem_gauss_int_coefs
      use int_rms_ave_ele_grps_1st
      use transfer_correlate_field
!
      integer(kind = kint) :: icomp, icomp_2
!
!
      do icomp = 1, num_tot_nod_phys
        icomp_2 = icomp+num_tot_nod_phys
        d_nod_trans2(1:nnod_2nd,1) = d_nod_2nd(1:nnod_2nd,icomp)
!
        call int_vol_2rms_ave_ele_grps_1st(max_int_point,               &
     &      n_layer_d, n_item_layer_d, layer_stack, item_layer,         &
     &      d_nod(1,icomp), d_nod_trans2(1,1), ave_l(1,icomp),          &
     &      rms_l(1,icomp), ave_l(1,icomp_2), rms_l(1,icomp_2))
      end do
!
      end subroutine int_vol_rms_ave_all_layer
!
!  ---------------------------------------------------------------------
!
      subroutine  int_vol_dev_cor_all_layer
!
      use m_geometry_parameter
      use m_node_phys_data
      use m_2nd_geometry_param
      use m_2nd_phys_data
      use m_fem_gauss_int_coefs
      use int_rms_ave_ele_grps_1st
      use transfer_correlate_field
!
      integer(kind = kint) :: icomp, icomp_2
!
!
      do icomp = 1, num_tot_nod_phys
        icomp_2 = icomp+num_tot_nod_phys
        d_nod_trans2(1:numnod,1) = d_nod_2nd(1:numnod,icomp)
        call int_vol_dev_cor_ele_grps_1st(max_int_point,                &
     &      n_layer_d, n_item_layer_d, layer_stack, item_layer,         &
     &      d_nod(1,icomp), d_nod_trans2(1,1),                          &
     &      ave_ref(1,icomp), ave_tgt(1,icomp),                         &
     &      sig_l(1,icomp), sig_l(1,icomp_2),  cor_l(1,icomp) )
      end do
!
      end subroutine  int_vol_dev_cor_all_layer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_correlate_data_names
!
      use m_node_phys_data
      use m_volume_average_labels
!
      integer(kind = kint) :: i_fld, ist
!
!
      do i_fld = 1, num_nod_phys
        ist = istack_nod_component(i_fld-1) + 1
        if     (num_nod_component(i_fld) .eq. 3) then
          call set_vector_label(phys_nod_name(i_fld), cor_name(ist))
        else if(num_nod_component(i_fld) .eq. 6) then
          call set_sym_tensor_label(phys_nod_name(i_fld),               &
     &        cor_name(ist))
        else
          cor_name(ist) = phys_nod_name(i_fld)
        end if
      end do
!
      end subroutine set_correlate_data_names
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_ave_rms_ratio_layers
!
      integer(kind = kint) :: icomp, i
!
!$omp parallel do private(icomp,i)
      do icomp = 1, ntot_correlate
        do i = 1, nlayer_correlate
          if(ave_ref(i,icomp) .eq. 0.0d0) then
            ave_ratio(i,icomp) = ave_tgt(i,icomp) / ave_ref(i,icomp)
          else
            ave_ratio(i,icomp) = ave_tgt(i,icomp) / ave_ref(i,icomp)
          end if
!
          if(rms_ref(i,icomp) .eq. 0.0d0) then
            rms_ratio(i,icomp) = rms_tgt(i,icomp) / rms_ref(i,icomp)
          else
            rms_ratio(i,icomp) = rms_tgt(i,icomp) / rms_ref(i,icomp)
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_ave_rms_ratio_layers
!
!  ---------------------------------------------------------------------
!
      subroutine take_sqrt_rms_data
!
      integer(kind = kint) :: icomp, i
!
!$omp parallel do private(icomp,i)
      do icomp = 1, ntot_correlate
        do i = 1, nlayer_correlate
          rms_tgt(i,icomp) = sqrt(rms_tgt(i,icomp))
          rms_ref(i,icomp) = sqrt(rms_ref(i,icomp))
          rms_ratio(i,icomp) = sqrt(rms_ratio(i,icomp))
        end do
      end do
!$omp end parallel do
!
      end subroutine take_sqrt_rms_data
!
!  ---------------------------------------------------------------------
!
      end module correlation_all_layerd_data
