!reduce_model_coefs.f90
!      module reduce_model_coefs
!
!     Modified by H. Matsui on June., 2012
!
!!      subroutine reduce_model_coefs_layer(SGS_factor, iak_sgs, wk_sgs)
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
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
      private :: reduce_Csim_layer
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine reduce_model_coefs_layer(SGS_factor, iak_sgs, wk_sgs)
!
      use t_ele_info_4_dynamic
!
      real (kind = kreal), intent(in) :: SGS_factor
      integer (kind = kint), intent(in) :: iak_sgs
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
!
!
      call reduce_Csim_layer(SGS_factor, wk_sgs%nlayer,                 &
     &    wk_sgs%fld_clip(1,iak_sgs), wk_sgs%fld_whole_clip(iak_sgs))
!
      end subroutine reduce_model_coefs_layer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine reduce_Csim_layer(SGS_factor, n_layer_d, coef, coef_w)
!
      integer (kind = kint), intent(in) :: n_layer_d
      real (kind = kreal), intent(in) :: SGS_factor
!
      real (kind = kreal), intent(inout) :: coef(n_layer_d)
      real (kind = kreal), intent(inout) :: coef_w
!
!
!$omp parallel workshare
      coef(1:n_layer_d) = coef(1:n_layer_d) * SGS_factor
!$omp end parallel workshare
      coef_w = coef_w * SGS_factor
!
      end subroutine reduce_Csim_layer
!
!  ---------------------------------------------------------------------
!
      end module reduce_model_coefs
