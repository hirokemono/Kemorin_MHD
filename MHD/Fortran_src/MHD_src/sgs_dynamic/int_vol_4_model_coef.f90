!
!      module int_vol_4_model_coef
!
!     Written by H. Matsui on June, 2006
!
!  Volume integration: int_vol_model_coef
!      subroutine int_vol_model_coef(n_tensor, n_int)
!      subroutine int_vol_diff_coef(iele_fsmp_stack, numdir, n_int)
!      subroutine int_vol_rms_ave_dynamic(n_tensor, n_int)
!      subroutine int_vol_rms_ave_diff(iele_fsmp_stack,                 &
!     &          n_tensor, n_int)
!      subroutine int_vol_layer_correlate(n_tensor, n_int, ave_s, ave_g)
!      subroutine int_vol_diff_correlate(iele_fsmp_stack, numdir,       &
!     &          n_int, ave_s, ave_g)
!
      module int_vol_4_model_coef
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_data
      use m_node_phys_data
      use m_jacobians
      use m_fem_gauss_int_coefs
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_model_coef(n_tensor, n_int)
!
      use m_layering_ele_list
      use m_work_4_dynamic_model
      use int_vol_model_coef_smp
      use int_vol_model_coef_grpsmp
!
      integer (kind = kint), intent(in) :: n_tensor, n_int
!
!
      sgs_w(1:18) =   0.0d0
      sgs_l(1:layer_tbl1%n_layer_d,1:18) =   0.0d0
!
      if(layer_tbl1%minlayer_4_smp                                      &
     &     .gt. layer_tbl1%min_item_layer_d_smp) then
!
        if (ele1%nnod_4_ele .eq. num_t_quad) then 
          call int_vol_model_coef_q(node1%numnod,                       &
     &        ele1%numele, ele1%ie, ele1%interior_ele, n_tensor,        &
     &        jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,  &
     &        layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,          &
     &        layer_tbl1%layer_stack_smp, layer_tbl1%item_layer,        &
     &        nod_fld1%ntot_phys, d_nod, sgs_l_smp, sgs_l, sgs_w)
        else
          call int_vol_model_coef_l(node1%numnod,                       &
     &        ele1%numele, ele1%ie, ele1%interior_ele, n_tensor,        &
     &        jac1_3d_l%ntot_int, n_int, jac1_3d_l%xjac, jac1_3d_l%an,  &
     &        layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,          &
     &        layer_tbl1%layer_stack_smp, layer_tbl1%item_layer,        &
     &        nod_fld1%ntot_phys, d_nod, sgs_l_smp, sgs_l, sgs_w)
        end if
!
      else
!
        sgs_l_smp(1:np_smp,1:18) = 0.0d0
        if (ele1%nnod_4_ele .eq. num_t_quad) then
          call int_vol_model_coef_grpsmp_q(node1%numnod,                &
     &      ele1%numele, ele1%ie, ele1%interior_ele, n_tensor,          &
     &      jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,    &
     &      layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,            &
     &      layer_tbl1%layer_stack, layer_tbl1%istack_item_layer_d_smp, &
     &      layer_tbl1%item_layer, nod_fld1%ntot_phys, d_nod,           &
     &      sgs_l_smp, sgs_l, sgs_w)
        else
          call int_vol_model_coef_grpsmp_l(node1%numnod,                &
     &      ele1%numele, ele1%ie, ele1%interior_ele, n_tensor,          &
     &      jac1_3d_l%ntot_int, n_int, jac1_3d_l%xjac, jac1_3d_l%an,    &
     &      layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,            &
     &      layer_tbl1%layer_stack, layer_tbl1%istack_item_layer_d_smp, &
     &      layer_tbl1%item_layer, nod_fld1%ntot_phys, d_nod,           &
     &      sgs_l_smp, sgs_l, sgs_w)
        end if
      end if
!
      end subroutine int_vol_model_coef
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_vol_diff_coef(iele_fsmp_stack, numdir, n_int)
!
      use m_work_4_dynamic_model
      use int_vol_4_diff_coef
!
      integer(kind=kint), intent(in) :: numdir, n_int
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
      sgs_w(1:18) =              0.0d0
      sgs_l_smp(1:np_smp,1:18) = 0.0d0
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
        call int_vol_diff_coef_q(node1%numnod, ele1%numele,             &
     &      ele1%ie, ele1%interior_ele, iele_fsmp_stack, numdir,        &
     &      jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,    &
     &      nod_fld1%ntot_phys, d_nod, sgs_l_smp, sgs_w)
        else
        call int_vol_diff_coef_l(node1%numnod, ele1%numele,             &
     &      ele1%ie, ele1%interior_ele, iele_fsmp_stack, numdir,        &
     &      jac1_3d_l%ntot_int, n_int, jac1_3d_l%xjac, jac1_3d_l%an,    &
     &      nod_fld1%ntot_phys, d_nod, sgs_l_smp, sgs_w)
      end if
!
      end subroutine int_vol_diff_coef
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_vol_rms_ave_dynamic(n_tensor, n_int)
!
      use m_layering_ele_list
      use m_work_layer_correlate
      use int_vol_rms_dynamic_smp
      use int_vol_rms_dynamic_grpsmp
!
      integer (kind = kint), intent(in) :: n_tensor, n_int
!
!
      if(layer_tbl1%minlayer_4_smp                                      &
     &     .gt. layer_tbl1%min_item_layer_d_smp) then
!
        if (ele1%nnod_4_ele .eq. num_t_quad) then
          call int_vol_rms_ave_dynamic_q(node1%numnod,                  &
     &        ele1%numele, ele1%ie, ele1%interior_ele, n_tensor,        &
     &        jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,  &
     &        layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,          &
     &        layer_tbl1%layer_stack_smp, layer_tbl1%item_layer,        &
     &        nod_fld1%ntot_phys, d_nod, ncomp_correlate_2,             &
     &        ave_l_smp, rms_l_smp, ave_l, rms_l, ave_w, rms_w)
        else
          call int_vol_rms_ave_dynamic_l(node1%numnod,                  &
     &        ele1%numele, ele1%ie, ele1%interior_ele, n_tensor,        &
     &        jac1_3d_l%ntot_int, n_int, jac1_3d_l%xjac, jac1_3d_l%an,  &
     &        layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,          &
     &        layer_tbl1%layer_stack_smp, layer_tbl1%item_layer,        &
     &        nod_fld1%ntot_phys, d_nod, ncomp_correlate_2,             &
     &        ave_l_smp, rms_l_smp, ave_l, rms_l, ave_w, rms_w)
        end if
!
      else
!
        if (ele1%nnod_4_ele .eq. num_t_quad) then
          call int_vol_rms_dynamic_grpsmp_q(node1%numnod,               &
     &      ele1%numele, ele1%ie, ele1%interior_ele, n_tensor,          &
     &      jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,    &
     &      layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,            &
     &      layer_tbl1%layer_stack, layer_tbl1%istack_item_layer_d_smp, &
     &      layer_tbl1%item_layer, nod_fld1%ntot_phys, d_nod,           &
     &      ncomp_correlate_2, ave_l_smp, rms_l_smp, ave_l, rms_l,      &
     &      ave_w, rms_w)
        else
          call int_vol_rms_dynamic_grpsmp_l(node1%numnod,               &
     &      ele1%numele, ele1%ie, ele1%interior_ele, n_tensor,          &
     &      jac1_3d_l%ntot_int, n_int, jac1_3d_l%xjac, jac1_3d_l%an,    &
     &      layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,            &
     &      layer_tbl1%layer_stack, layer_tbl1%istack_item_layer_d_smp, &
     &      layer_tbl1%item_layer, nod_fld1%ntot_phys, d_nod,           &
     &      ncomp_correlate_2, ave_l_smp, rms_l_smp, ave_l, rms_l,      &
     &      ave_w, rms_w)
        end if
!
      end if
!
      end subroutine int_vol_rms_ave_dynamic
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_rms_ave_diff(iele_fsmp_stack,                  &
     &          n_tensor, n_int)
!
      use m_work_layer_correlate
      use int_vol_rms_ave_diff_smp
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_tensor, n_int
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
        call int_vol_rms_ave_d_q(node1%numnod, ele1%numele, ele1%ie,    &
     &      ele1%interior_ele, iele_fsmp_stack, n_tensor,               &
     &      jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,    &
     &      nod_fld1%ntot_phys, d_nod, ncomp_correlate_2,               &
     &      ave_l_smp, rms_l_smp, ave_w, rms_w)
      else
        call int_vol_rms_ave_d_l(node1%numnod, ele1%numele, ele1%ie,    &
     &      ele1%interior_ele, iele_fsmp_stack, n_tensor,               &
     &      jac1_3d_l%ntot_int, n_int, jac1_3d_l%xjac, jac1_3d_l%an,    &
     &      nod_fld1%ntot_phys, d_nod, ncomp_correlate_2,               &
     &      ave_l_smp, rms_l_smp, ave_w, rms_w)
      end if
!
      end subroutine int_vol_rms_ave_diff
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_layer_correlate(n_tensor, n_int, ave_s, ave_g)
!
      use m_layering_ele_list
      use m_work_layer_correlate
      use int_vol_cor_dynamic_smp
      use int_vol_layer_cor_grpsmp
!
      integer (kind = kint), intent(in) :: n_tensor, n_int
      real(kind = kreal), intent(in)                                    &
     &                   :: ave_s(layer_tbl1%n_layer_d,n_tensor)
      real(kind = kreal), intent(in)                                    &
     &                   :: ave_g(layer_tbl1%n_layer_d,n_tensor)
!
!
      if(layer_tbl1%minlayer_4_smp                                      &
     &      .gt. layer_tbl1%min_item_layer_d_smp) then
!
        if (ele1%nnod_4_ele .eq. num_t_quad) then
          call int_vol_layer_cor_q(node1%numnod,                        &
     &        ele1%numele, ele1%ie, ele1%interior_ele, n_tensor,        &
     &        jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,  &
     &        layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,          &
     &        layer_tbl1%layer_stack_smp, layer_tbl1%item_layer,        &
     &        ave_s, ave_g, nod_fld1%ntot_phys, d_nod,                  &
     &        ncomp_correlate, ncomp_correlate_2,                       &
     &        sig_l_smp, cor_l_smp, sig_l, cov_l, sig_w, cov_w)
        else
          call int_vol_layer_cor_l(node1%numnod,                        &
     &        ele1%numele, ele1%ie, ele1%interior_ele, n_tensor,        &
     &        jac1_3d_l%ntot_int, n_int, jac1_3d_l%xjac, jac1_3d_l%an,  &
     &        layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,          &
     &        layer_tbl1%layer_stack_smp, layer_tbl1%item_layer,        &
     &        ave_s, ave_g, nod_fld1%ntot_phys, d_nod,                  &
     &        ncomp_correlate, ncomp_correlate_2,                       &
     &        sig_l_smp, cor_l_smp, sig_l, cov_l, sig_w, cov_w)
        end if
!
      else
!
        if (ele1%nnod_4_ele .eq. num_t_quad) then
          call int_vol_layer_cor_grpsmp_q(node1%numnod,                 &
     &      ele1%numele, ele1%ie, ele1%interior_ele, n_tensor,          &
     &      jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,    &
     &      layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,            &
     &      layer_tbl1%layer_stack, layer_tbl1%istack_item_layer_d_smp, &
     &      layer_tbl1%item_layer, ave_s, ave_g, nod_fld1%ntot_phys,    &
     &      d_nod, ncomp_correlate, ncomp_correlate_2,                  &
     &      sig_l_smp, cor_l_smp, sig_l, cov_l, sig_w, cov_w)
        else
          call int_vol_layer_cor_grpsmp_l(node1%numnod,                 &
     &      ele1%numele, ele1%ie, ele1%interior_ele, n_tensor,          &
     &      jac1_3d_l%ntot_int, n_int, jac1_3d_l%xjac, jac1_3d_l%an,    &
     &      layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,            &
     &      layer_tbl1%layer_stack, layer_tbl1%istack_item_layer_d_smp, &
     &      layer_tbl1%item_layer, ave_s, ave_g, nod_fld1%ntot_phys,    &
     &      d_nod, ncomp_correlate, ncomp_correlate_2,                  &
     &      sig_l_smp, cor_l_smp, sig_l, cov_l, sig_w, cov_w)
        end if
!
      end if
!
      end subroutine int_vol_layer_correlate
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_diff_correlate(iele_fsmp_stack, numdir,        &
     &          n_int, ave_s, ave_g)
!
      use m_fem_gauss_int_coefs
      use m_work_layer_correlate
      use int_vol_diff_correlate_smp
!
!
      integer(kind=kint), intent(in) :: numdir, n_int
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: ave_s(numdir)
      real(kind = kreal), intent(in) :: ave_g(numdir)
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
        call int_vol_diff_correlate_q(node1%numnod, ele1%numele,        &
     &      ele1%ie, ele1%interior_ele, iele_fsmp_stack, numdir,        &
     &      jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,    &
     &      ave_s, ave_g, nod_fld1%ntot_phys, d_nod,                    &
     &      ncomp_correlate, ncomp_correlate_2,                         &
     &      sig_l_smp, cor_l_smp, sig_w, cov_w)
        else
        call int_vol_diff_correlate_l(node1%numnod, ele1%numele,        &
     &      ele1%ie, ele1%interior_ele, iele_fsmp_stack, numdir,        &
     &      jac1_3d_l%ntot_int, n_int, jac1_3d_l%xjac, jac1_3d_l%an,    &
     &      ave_s, ave_g, nod_fld1%ntot_phys, d_nod,                    &
     &      ncomp_correlate, ncomp_correlate_2,                         &
     &      sig_l_smp, cor_l_smp, sig_w, cov_w)
      end if
!
      end subroutine int_vol_diff_correlate
!
!-----------------------------------------------------------------------
!
      end module int_vol_4_model_coef
