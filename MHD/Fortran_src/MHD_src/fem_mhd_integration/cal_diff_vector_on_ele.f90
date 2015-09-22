!
!     module cal_diff_vector_on_ele
!
!     Written by H.Matsui
!
!!      subroutine sel_int_diff_vector_on_ele                           &
!!     &         (iele_fsmp_stack, i_fld, i_diff)
!!      subroutine sel_int_diff_scalar_on_ele                           &
!!     &         (iele_fsmp_stack, i_fld, i_diff)
!
!      subroutine diff_composition_on_ele
!      subroutine diff_filter_c_on_ele
!
      module cal_diff_vector_on_ele
!
      use m_precision
      use m_machine_parameter
!
      use m_geometry_constants
      use m_geometry_data
      use m_node_phys_data
      use m_jacobians
      use m_int_vol_data
      use int_differenciate_on_ele
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_int_diff_vector_on_ele                             &
     &         (iele_fsmp_stack, i_fld, i_diff)
!
      use m_control_parameter
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: i_fld, i_diff
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
        call int_diff_vector_on_ele(node1, ele1, jac1_3d_q,             &
     &      iele_fsmp_stack, intg_point_t_evo,                          &
     &      nod_fld1%ntot_phys, i_fld, nod_fld1%d_fld,                  &
     &      num_dvxi, i_diff, dvx)
      else
        call int_diff_vector_on_ele(node1, ele1, jac1_3d_l,             &
     &      iele_fsmp_stack, intg_point_t_evo,                          &
     &      nod_fld1%ntot_phys, i_fld, nod_fld1%d_fld,                  &
     &      num_dvxi, i_diff, dvx)
      end if
!
      end subroutine sel_int_diff_vector_on_ele
!
! -----------------------------------------------------------------------
!
      subroutine sel_int_diff_scalar_on_ele                             &
     &         (iele_fsmp_stack, i_fld, i_diff)
!
      use m_control_parameter
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: i_fld, i_diff
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
        call int_diff_scalar_on_ele(node1, ele1, jac1_3d_q,             &
     &      iele_fsmp_stack, intg_point_t_evo,                          &
     &      nod_fld1%ntot_phys, i_fld, nod_fld1%d_fld,                  &
     &      num_dvxi, i_diff, dvx)
      else
        call int_diff_scalar_on_ele(node1, ele1, jac1_3d_l,             &
     &      iele_fsmp_stack, intg_point_t_evo,                          &
     &      nod_fld1%ntot_phys, i_fld, nod_fld1%d_fld,                  &
     &      num_dvxi, i_diff, dvx)
      end if
!
      end subroutine sel_int_diff_scalar_on_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine diff_composition_on_ele
!
      use m_control_parameter
      use m_node_phys_address
!
!
      call sel_int_diff_scalar_on_ele                                   &
     &   (ele1%istack_ele_smp, iphys%i_light, i_dcx)
!
      end subroutine diff_composition_on_ele
!
! -----------------------------------------------------------------------
!
      subroutine diff_filter_c_on_ele
!
      use m_control_parameter
      use m_node_phys_address
!
!
      call sel_int_diff_scalar_on_ele                                   &
     &   (ele1%istack_ele_smp, iphys%i_filter_comp, i_dfcx)
!
      end subroutine diff_filter_c_on_ele
!
! -----------------------------------------------------------------------
!
      end module cal_diff_vector_on_ele
