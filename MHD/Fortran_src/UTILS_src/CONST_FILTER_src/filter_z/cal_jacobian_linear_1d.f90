!
!      module cal_jacobian_linear_1d
!
      module cal_jacobian_linear_1d
!
!        programmed by H. Matsui on June, 2007
!
      use m_precision
!
      implicit none
!
!      subroutine s_cal_jacobian_linear_1d(num_int)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_linear_1d(num_int)
!
      use m_fem_gauss_int_coefs
      use m_gauss_int_parameters
      use set_size_4_smp
      use set_integration_indices
      use set_gauss_int_parameters
      use cal_jacobian
!
      integer(kind = kint), intent(in) :: num_int
!
!  data allocation
!
      call count_surf_size_4_smp
      call count_edge_size_4_smp
!
      call allocate_integrate_arrays(num_int)
!
      call allocate_gauss_coef_4_fem
!
!  set constant for gauss integration with roots
!
      call init_gauss_int_parameters
!
!  set indices for gauss integration
!
      call set_integration_indices_1d_mesh
!
!  set weighting for integration
!
      call set_gauss_coefs_4_1d
!
      call cal_jacobian_edge
!
      end subroutine s_cal_jacobian_linear_1d
!
!-----------------------------------------------------------------------
!
      end module cal_jacobian_linear_1d
