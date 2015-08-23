!
!     module cal_diff_vector_on_ele
!
!     Written by H.Matsui
!
!      subroutine diff_velocity_on_ele
!      subroutine diff_magne_on_ele
!      subroutine diff_filter_v_on_ele
!      subroutine diff_filter_b_on_ele
!
!      subroutine diff_temp_on_ele
!      subroutine diff_composition_on_ele
!      subroutine diff_filter_t_on_ele
!      subroutine diff_filter_c_on_ele
!
      module cal_diff_vector_on_ele
!
      use m_precision
      use m_machine_parameter
!
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_address
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
      subroutine diff_velocity_on_ele
!
!
      call int_diff_vector_on_ele                                       &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    ele1%a_vol_ele, iele_fl_smp_stack, jac1_3d_q%ntot_int,        &
     &    intg_point_t_evo, jac1_3d_q%xjac, dwx,                        &
     &    d_nod(1,iphys%i_velo), dvx(1,i_dvx))
!
      end subroutine diff_velocity_on_ele
!
! -----------------------------------------------------------------------
!
      subroutine diff_magne_on_ele
!
!
      call int_diff_vector_on_ele                                       &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    ele1%a_vol_ele, iele_cd_smp_stack, jac1_3d_q%ntot_int,        &
     &    intg_point_t_evo, jac1_3d_q%xjac, dwx,                        &
     &    d_nod(1,iphys%i_magne), dvx(1,i_dbx))
!
      end subroutine diff_magne_on_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine diff_filter_v_on_ele
!
!
      call int_diff_vector_on_ele                                       &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    ele1%a_vol_ele, iele_fl_smp_stack, jac1_3d_q%ntot_int,        &
     &    intg_point_t_evo, jac1_3d_q%xjac, dwx,                        &
     &    d_nod(1,iphys%i_filter_velo), dvx(1,i_dfvx))
!
      end subroutine diff_filter_v_on_ele
!
! -----------------------------------------------------------------------
!
      subroutine diff_filter_b_on_ele
!
!
      call int_diff_vector_on_ele                                       &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    ele1%a_vol_ele, iele_fl_smp_stack, jac1_3d_q%ntot_int,        &
     &    intg_point_t_evo, jac1_3d_q%xjac, dwx,                        &
     &    d_nod(1,iphys%i_filter_magne), dvx(1,i_dfbx))
!
      return
      end subroutine diff_filter_b_on_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine diff_temp_on_ele
!
!
      call int_diff_scalar_on_ele                                       &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    ele1%a_vol_ele, ele1%istack_ele_smp, jac1_3d_q%ntot_int,      &
     &    intg_point_t_evo, jac1_3d_q%xjac, dwx,                        &
     &    d_nod(1,iphys%i_temp), dvx(1,i_dtx))
!
      return
      end subroutine diff_temp_on_ele
!
! -----------------------------------------------------------------------
!
      subroutine diff_composition_on_ele
!
!
      call int_diff_scalar_on_ele                                       &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    ele1%a_vol_ele, ele1%istack_ele_smp, jac1_3d_q%ntot_int,      &
     &    intg_point_t_evo, jac1_3d_q%xjac, dwx,                        &
     &    d_nod(1,iphys%i_light), dvx(1,i_dcx))
!
      return
      end subroutine diff_composition_on_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine diff_filter_t_on_ele
!
!
      call int_diff_scalar_on_ele                                       &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    ele1%a_vol_ele, ele1%istack_ele_smp, jac1_3d_q%ntot_int,      &
     &    intg_point_t_evo, jac1_3d_q%xjac, dwx,                        &
     &    d_nod(1,iphys%i_filter_temp), dvx(1,i_dftx))
!
      return
      end subroutine diff_filter_t_on_ele
!
! -----------------------------------------------------------------------
!
      subroutine diff_filter_c_on_ele
!
!
      call int_diff_scalar_on_ele                                       &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    ele1%a_vol_ele, ele1%istack_ele_smp, jac1_3d_q%ntot_int,      &
     &    intg_point_t_evo, jac1_3d_q%xjac, dwx,                        &
     &    d_nod(1,iphys%i_filter_comp), dvx(1,i_dfcx))
!
      return
      end subroutine diff_filter_c_on_ele
!
! -----------------------------------------------------------------------
!
      end module cal_diff_vector_on_ele
