!correlation_all_layerd_data.f90
!      module correlation_all_layerd_data
!
!     Written by H. Matsui on Nov., 2009
!
!!      subroutine allocate_vec_transfer(numnod)
!!      subroutine s_correlation_all_layerd_data(node, ele, nod_fld,    &
!!     &          jacs, layer_tbl, phys_2nd, wk_cor)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_l, jac_3d_q
!!        type(phys_data), intent(in) :: phys_2nd
!!        type(layering_tbl), intent(in) :: layer_tbl
!
      module correlation_all_layerd_data
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
      use m_all_layerd_correlate
!
      use t_geometry_data
      use t_phys_data
      use t_jacobians
      use t_jacobian_3d
      use t_layering_ele_list
      use t_work_layer_correlate
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
      subroutine s_correlation_all_layerd_data(node, ele, nod_fld,      &
     &          jacs, layer_tbl, phys_2nd, wk_cor)
!
      use cal_layerd_ave_correlate
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(inout) :: jacs
      type(phys_data), intent(in) :: phys_2nd
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
!
!
      call int_vol_rms_ave_all_layer(node, ele, nod_fld,                &
     &    jacs%g_FEM, jacs%jac_3d_l, jacs%jac_3d, layer_tbl, phys_2nd,  &
     &    wk_cor%nlayer, wk_cor%ncomp_sgl, wk_cor%ncomp_dble,           &
     &    wk_cor%ave_l, wk_cor%rms_l)
      call sum_layerd_averages(layer_tbl%e_grp%num_grp, wk_cor)
!
      if(iflag_debug .gt. 0) write(*,*) 'divide_layers_ave_by_vol'
      call divide_layers_ave_by_vol                                     &
     &   (wk_cor%nlayer, wk_cor%ncomp_sgl, wk_cor%ncomp_dble,           &
     &    nod_fld%ntot_phys, layer_tbl%a_vol_layer,                     &
     &    wk_cor%ave_les, wk_cor%rms_les, ave_ref(1,1), ave_tgt(1,1),   &
     &    rms_ref(1,1), rms_tgt(1,1), rms_ratio(1,1))
!
      call cal_ave_rms_ratio_layers
!
!
      if(iflag_debug .gt. 0) write(*,*) 'int_vol_dev_cor_all_layer'
      call int_vol_dev_cor_all_layer(node, ele, nod_fld,                &
     &    jacs%g_FEM, jacs%jac_3d_l, jacs%jac_3d, layer_tbl, phys_2nd,  &
     &    wk_cor%nlayer, wk_cor%ncomp_sgl, wk_cor%ncomp_dble,           &
     &    wk_cor%sig_l, wk_cor%cov_l)
!
      if(iflag_debug .gt. 0) write(*,*) 'sum_layerd_correlation'
      call sum_layerd_correlation(layer_tbl%e_grp%num_grp, wk_cor)
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_layered_correlation'
      call cal_layered_correlation                                      &
     &   (wk_cor%nlayer, wk_cor%ncomp_sgl, wk_cor%ncomp_dble,           &
     &    nod_fld%ntot_phys, layer_tbl%a_vol_layer,                     &
     &    wk_cor%cov_les, wk_cor%sig_les, cor_data(1,1), cov_data(1,1))
!
      call take_sqrt_rms_data
!
      end subroutine s_correlation_all_layerd_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_vol_rms_ave_all_layer(node, ele, nod_fld,          &
     &          g_FEM, jac_3d_l, jac_3d_q, layer_tbl, phys_2nd,         &
     &          n_layer, ncomp_sgl, ncomp_dble, ave_l, rms_l)
!
      use int_rms_ave_ele_grps
      use transfer_correlate_field
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_l, jac_3d_q
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_data), intent(in) :: phys_2nd
      integer(kind = kint), intent(in) :: n_layer
      integer(kind = kint), intent(in) :: ncomp_sgl, ncomp_dble
!
      real(kind=kreal), intent(inout) :: ave_l(n_layer,ncomp_dble)
      real(kind=kreal), intent(inout) :: rms_l(n_layer,ncomp_dble)
!
      integer(kind = kint) :: icomp, icomp_2
!
!
      do icomp = 1, nod_fld%ntot_phys
        icomp_2 = icomp + ncomp_sgl
        d_nod_trans2(1:phys_2nd%n_point,1)                              &
     &          = phys_2nd%d_fld(1:phys_2nd%n_point,icomp)
!
        call int_vol_2rms_ave_ele_grps(node, ele, layer_tbl%e_grp,      &
     &      g_FEM, jac_3d_q, jac_3d_l, g_FEM%max_int_point,             &
     &      nod_fld%ntot_phys, icomp, nod_fld%d_fld, ione, ione,        &
     &      d_nod_trans2(1,1), ave_l(1,icomp), rms_l(1,icomp),          &
     &      ave_l(1,icomp_2), rms_l(1,icomp_2))
      end do
!
      end subroutine int_vol_rms_ave_all_layer
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_dev_cor_all_layer(node, ele, nod_fld,          &
     &          g_FEM, jac_3d_l, jac_3d_q, layer_tbl, phys_2nd,         &
     &          n_layer, ncomp_sgl, ncomp_dble, sig_l, cov_l)
!
      use int_rms_ave_ele_grps
      use transfer_correlate_field
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_l, jac_3d_q
      type(phys_data), intent(in) :: phys_2nd
      type(layering_tbl), intent(in) :: layer_tbl
      integer(kind = kint), intent(in) :: n_layer
      integer(kind = kint), intent(in) :: ncomp_sgl, ncomp_dble
!
      real(kind=kreal), intent(inout) :: sig_l(n_layer,ncomp_dble)
      real(kind=kreal), intent(inout) :: cov_l(n_layer,ncomp_sgl)
!
      integer(kind = kint) :: icomp, icomp_2
!
!
      do icomp = 1, nod_fld%ntot_phys
        icomp_2 = icomp + nod_fld%ntot_phys
        d_nod_trans2(1:node%numnod,1)                                   &
     &          = phys_2nd%d_fld(1:node%numnod,icomp)
        call int_vol_dev_cor_ele_grps                                   &
     &     (node, ele, layer_tbl%e_grp, g_FEM, jac_3d_q, jac_3d_l,      &
     &      g_FEM%max_int_point, nod_fld%ntot_phys, icomp,              &
     &      nod_fld%d_fld, ione, ione, d_nod_trans2(1,1),               &
     &      ave_ref(1,icomp), ave_tgt(1,icomp),                         &
     &      sig_l(1,icomp), sig_l(1,icomp_2),  cov_l(1,icomp) )
      end do
!
      end subroutine int_vol_dev_cor_all_layer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_correlate_data_names(nod_fld)
!
      use m_phys_constants
      use t_phys_data
      use m_volume_average_labels
!
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint) :: i_fld, ist
!
!
      do i_fld = 1, nod_fld%num_phys
        ist = nod_fld%istack_component(i_fld-1) + 1
        if     (nod_fld%num_component(i_fld) .eq. n_vector) then
          call set_vector_label                                         &
     &       (nod_fld%phys_name(i_fld), cor_name(ist))
        else if(nod_fld%num_component(i_fld) .eq. n_sym_tensor) then
          call set_sym_tensor_label                                     &
     &       (nod_fld%phys_name(i_fld), cor_name(ist))
        else
          cor_name(ist) = nod_fld%phys_name(i_fld)
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
