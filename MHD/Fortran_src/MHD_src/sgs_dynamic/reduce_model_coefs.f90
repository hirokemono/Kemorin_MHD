!reduce_model_coefs.f90
!      module reduce_model_coefs
!
!     Modified by H. Matsui on June., 2012
!
!!      subroutine reduce_model_coefs_layer(SGS_factor, n_layer_d,      &
!!     &          num_kinds, iak_sgs, coef, coef_w)
!!      subroutine reduce_ele_vect_model_coefs                          &
!!     &         (ele, SGS_factor, ntot_comp_ele, ifield_ele, ak_sgs)
!!      subroutine reduce_ele_tensor_model_coefs                        &
!!     &         (ele, SGS_factor, ntot_comp_ele, ifield_ele, ak_sgs)
!!        type(element_data), intent(in) :: ele
!
      module reduce_model_coefs
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine reduce_model_coefs_layer(SGS_factor, n_layer_d,        &
     &          num_kinds, iak_sgs, coef, coef_w)
!
      integer (kind = kint), intent(in) :: n_layer_d
      integer (kind = kint), intent(in) :: num_kinds, iak_sgs
      real (kind = kreal), intent(in) :: SGS_factor
!
      real (kind = kreal), intent(inout) :: coef(n_layer_d, num_kinds)
      real (kind = kreal), intent(inout) :: coef_w(num_kinds)
!
!
!$omp parallel workshare
      coef(1:n_layer_d,iak_sgs)                                         &
     &      = coef(1:n_layer_d,iak_sgs) * SGS_factor
!$omp end parallel workshare
      coef_w(iak_sgs) = coef_w(iak_sgs) * SGS_factor
!
      end subroutine reduce_model_coefs_layer
!
!  ---------------------------------------------------------------------
!
      subroutine reduce_ele_vect_model_coefs                            &
     &         (ele, SGS_factor, ntot_comp_ele, ifield_ele, ak_sgs)
!
      use overwrite_prod_const_smp
!
      type(element_data), intent(in) :: ele
      real (kind = kreal), intent(in) :: SGS_factor
!
      integer(kind = kint), intent(in) :: ntot_comp_ele, ifield_ele
      real (kind = kreal), intent(inout)                                &
     &            :: ak_sgs(ele%numele,ntot_comp_ele)
!
!
!$omp parallel
      call ovwrt_coef_prod_vect_smp(np_smp, ele%numele,                 &
     &    ele%istack_ele_smp, SGS_factor, ak_sgs(1,ifield_ele))
!$omp end parallel
!
      end subroutine reduce_ele_vect_model_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine reduce_ele_tensor_model_coefs                          &
     &         (ele, SGS_factor, ntot_comp_ele, ifield_ele, ak_sgs)
!
      use overwrite_prod_const_smp
!
      type(element_data), intent(in) :: ele
      real (kind = kreal), intent(in) :: SGS_factor
      integer(kind = kint), intent(in) :: ntot_comp_ele, ifield_ele
      real (kind = kreal), intent(inout)                                &
     &            :: ak_sgs(ele%numele,ntot_comp_ele)
!
!
!$omp parallel
      call ovwrt_coef_prod_tensor_smp(np_smp, ele%numele,               &
     &    ele%istack_ele_smp, SGS_factor, ak_sgs(1,ifield_ele))
!$omp end parallel
!
      end subroutine reduce_ele_tensor_model_coefs
!
!  ---------------------------------------------------------------------
!
      end module reduce_model_coefs
