!correlation_all_layerd_data.f90
!      module correlation_all_layerd_data
!
!     Written by H. Matsui on Nov., 2009
!
!      subroutine allocate_vec_transfer(numnod)
!      subroutine s_correlation_all_layerd_data(nnod_2, phys_2nd)
!
      module correlation_all_layerd_data
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
      use m_all_layerd_correlate
      use m_layering_ele_list
      use m_work_layer_correlate
!
      implicit none
!
      real(kind = kreal), allocatable :: d_nod_trans2(:,:)
!
      private :: d_nod_trans2
      private :: int_vol_rms_ave_all_layer, int_vol_dev_cor_all_layer
      private :: cal_ave_rms_ratio_layers, take_sqrt_rms_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_vec_transfer(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
!
      allocate(d_nod_trans2(numnod,6))
      d_nod_trans2 = 0.0d0
!
      end subroutine allocate_vec_transfer
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_vec_transfer
!
      deallocate(d_nod_trans2)
!
      end subroutine deallocate_vec_transfer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_correlation_all_layerd_data(nnod_2, phys_2nd)
!
      use m_node_phys_data
      use cal_layerd_ave_correlate
      use t_phys_data
!
      integer(kind = kint), intent(in) :: nnod_2
      type(phys_data), intent(in) :: phys_2nd
!
!
      call int_vol_rms_ave_all_layer(nnod_2, phys_2nd)
      call sum_layerd_averages(layer_tbl1%n_layer_d)
!
      if(iflag_debug .gt. 0) write(*,*) 'divide_layers_ave_by_vol'
      call divide_layers_ave_by_vol(layer_tbl1%n_layer_d,               &
     &    nod_fld1%ntot_phys, layer_tbl1%a_vol_layer,                   &
     &    ave_ref(1,1), ave_tgt(1,1), rms_ref(1,1), rms_tgt(1,1),       &
     &    rms_ratio(1,1))
!
      call cal_ave_rms_ratio_layers
!
!
      if(iflag_debug .gt. 0) write(*,*) 'int_vol_dev_cor_all_layer'
      call int_vol_dev_cor_all_layer(phys_2nd)
!
      if(iflag_debug .gt. 0) write(*,*) 'sum_layerd_correlation'
      call sum_layerd_correlation(layer_tbl1%n_layer_d)
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_layered_correlation'
      call cal_layered_correlation                                      &
     &   (layer_tbl1%n_layer_d, nod_fld1%ntot_phys,                     &
     &    layer_tbl1%a_vol_layer, cor_data(1,1), cov_data(1,1))
!
      call take_sqrt_rms_data
!
      end subroutine s_correlation_all_layerd_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine  int_vol_rms_ave_all_layer(nnod_2, phys_2nd)
!
      use m_node_phys_data
      use m_fem_gauss_int_coefs
      use int_rms_ave_ele_grps_1st
      use transfer_correlate_field
      use t_phys_data
!
      integer(kind = kint), intent(in) :: nnod_2
      type(phys_data), intent(in) :: phys_2nd
!
      integer(kind = kint) :: icomp, icomp_2
!
!
      do icomp = 1, nod_fld1%ntot_phys
        icomp_2 = icomp + nod_fld1%ntot_phys
        d_nod_trans2(1:nnod_2,1) = phys_2nd%d_fld(1:nnod_2,icomp)
!
        call int_vol_2rms_ave_ele_grps_1st(max_int_point,               &
     &      layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,            &
     &      layer_tbl1%layer_stack, layer_tbl1%item_layer,              &
     &      nod_fld1%ntot_phys, icomp, d_nod,                           &
     &      ione, ione, d_nod_trans2(1,1), ave_l(1,icomp),              &
     &      rms_l(1,icomp), ave_l(1,icomp_2), rms_l(1,icomp_2))
      end do
!
      end subroutine int_vol_rms_ave_all_layer
!
!  ---------------------------------------------------------------------
!
      subroutine  int_vol_dev_cor_all_layer(phys_2nd)
!
      use m_geometry_data
      use m_node_phys_data
      use m_fem_gauss_int_coefs
      use int_rms_ave_ele_grps_1st
      use transfer_correlate_field
      use t_phys_data
!
      type(phys_data), intent(in) :: phys_2nd
      integer(kind = kint) :: icomp, icomp_2
!
!
      do icomp = 1, nod_fld1%ntot_phys
        icomp_2 = icomp + nod_fld1%ntot_phys
        d_nod_trans2(1:node1%numnod,1)                                  &
     &          = phys_2nd%d_fld(1:node1%numnod,icomp)
        call int_vol_dev_cor_ele_grps_1st(max_int_point,                &
     &      layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,            &
     &      layer_tbl1%layer_stack, layer_tbl1%item_layer,              &
     &      nod_fld1%ntot_phys, icomp, d_nod,                           &
     &      ione, ione, d_nod_trans2(1,1),                              &
     &      ave_ref(1,icomp), ave_tgt(1,icomp),                         &
     &      sig_l(1,icomp), sig_l(1,icomp_2),  cov_l(1,icomp) )
      end do
!
      end subroutine  int_vol_dev_cor_all_layer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_correlate_data_names
!
      use m_phys_constants
      use m_node_phys_data
      use m_volume_average_labels
!
      integer(kind = kint) :: i_fld, ist
!
!
      do i_fld = 1, nod_fld1%num_phys
        ist = nod_fld1%istack_component(i_fld-1) + 1
        if     (nod_fld1%num_component(i_fld) .eq. n_vector) then
          call set_vector_label                                         &
     &       (nod_fld1%phys_name(i_fld), cor_name(ist))
        else if(nod_fld1%num_component(i_fld) .eq. n_sym_tensor) then
          call set_sym_tensor_label                                     &
     &       (nod_fld1%phys_name(i_fld), cor_name(ist))
        else
          cor_name(ist) = nod_fld1%phys_name(i_fld)
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
