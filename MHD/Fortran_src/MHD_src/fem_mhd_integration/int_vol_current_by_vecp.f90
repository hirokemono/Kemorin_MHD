!int_vol_current_by_vecp.f90
!      module int_vol_current_by_vecp
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine int_vol_current_diffuse
!
      module int_vol_current_by_vecp
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_current_diffuse
!
      use m_control_parameter
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
!
      use nodal_fld_2_each_ele_1st
      use fem_skv_vector_diff_type
      use cal_skv_to_ff_smp_1st
!
      integer (kind = kint) :: k2
!
!
      ff_nl_smp = 0.0d0
      call reset_sk6(n_vector)
!
      do k2=1, ele1%nnod_4_ele
        call vactor_phys_2_each_element(k2, iphys%i_vecp, vect_1)
        call fem_skv_rot_rot_by_laplace(ele1%istack_ele_smp,            &
     &      intg_point_poisson, k2, ele1, jac1_3d_q, vect_1,            &
     &      fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_current_diffuse
!
! ----------------------------------------------------------------------
!
      end module int_vol_current_by_vecp
