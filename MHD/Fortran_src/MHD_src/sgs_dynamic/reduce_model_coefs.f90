!reduce_model_coefs.f90
!      module reduce_model_coefs
!
!     Modified by H. Matsui on June., 2012
!
!      subroutine reduce_model_coefs_layer(SGS_factor, n_layer_d, coef, &
!     &          coef_w)
!      subroutine reduce_ele_vect_model_coefs(SGS_factor, ak_sgs)
!
      module reduce_model_coefs
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine reduce_model_coefs_layer(SGS_factor, n_layer_d, coef,  &
     &          coef_w)
!
      integer (kind = kint), intent(in) :: n_layer_d
      real (kind = kreal), intent(in) :: SGS_factor
!
      real (kind = kreal), intent(inout) :: coef(n_layer_d)
      real (kind = kreal), intent(inout) :: coef_w
!
!
      coef(1:n_layer_d) = coef(1:n_layer_d) * SGS_factor
      coef_w = coef_w * SGS_factor
!
      end subroutine reduce_model_coefs_layer
!
!  ---------------------------------------------------------------------
!
      subroutine reduce_ele_vect_model_coefs(SGS_factor, ak_sgs)
!
      use m_machine_parameter
      use m_geometry_data
      use overwrite_prod_const_smp
!
      real (kind = kreal), intent(in) :: SGS_factor
      real (kind = kreal), intent(inout) :: ak_sgs(ele1%numele,3)
!
!
!$omp parallel
      call ovwrt_coef_prod_vect_smp(np_smp, ele1%numele,                &
     &    iele_smp_stack, SGS_factor, ak_sgs)
!$omp end parallel
!
      end subroutine reduce_ele_vect_model_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine reduce_ele_tensor_model_coefs(SGS_factor, ak_sgs)
!
      use m_machine_parameter
      use m_geometry_data
      use overwrite_prod_const_smp
!
      real (kind = kreal), intent(in) :: SGS_factor
      real (kind = kreal), intent(inout) :: ak_sgs(ele1%numele,6)
!
!
!$omp parallel
      call ovwrt_coef_prod_tensor_smp(np_smp, ele1%numele,              &
     &    iele_smp_stack, SGS_factor, ak_sgs)
!$omp end parallel
!
      end subroutine reduce_ele_tensor_model_coefs
!
!  ---------------------------------------------------------------------
!
      end module reduce_model_coefs
