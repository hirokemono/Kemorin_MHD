!
!     module cal_rotation_sgs
!
!     Written by H. Matsui
!
!      subroutine cal_rotation_sgs_all(iflag_4_supg,                    &
!     &          sgs_sf, iak_diff, i_res, i_vector)
!      subroutine cal_rotation_sgs_fluid(iflag_4_supg,                  &
!     &          sgs_sf, iak_diff, i_res, i_vector)
!      subroutine cal_rotation_sgs_conduct(iflag_4_supg,                &
!     &          sgs_sf, iak_diff, i_res, i_vector)
!!        type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!
      module cal_rotation_sgs
!
      use m_precision
!
      use m_phys_constants
      use m_nod_comm_table
      use m_geometry_data
      use m_node_phys_data
      use m_jacobians
      use m_sorted_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
      use m_SGS_model_coefs
!
      use t_surface_bc_data
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
!
      private :: choose_int_vol_rot_sgs
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_rotation_sgs_all(iflag_4_supg,                     &
     &          sgs_sf, iak_diff, i_res, i_vector)
!
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: i_vector, i_res
      integer(kind = kint), intent(in) :: iak_diff
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call choose_int_vol_rot_sgs(iflag_4_supg, ele1%istack_ele_smp,    &
     &     sgs_sf, iak_diff, i_vector)
!
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, m1_lump%ml, nod_fld1%ntot_phys,                     &
     &    i_res, nod_fld1%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_res, node1, nod_comm, nod_fld1)
!
      end subroutine cal_rotation_sgs_all
!
!-----------------------------------------------------------------------
!
      subroutine cal_rotation_sgs_fluid(iflag_4_supg,                   &
     &          sgs_sf, iak_diff, i_res, i_vector)
!
      use m_geometry_data_MHD
!
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: i_vector, i_res
      integer(kind = kint), intent(in) :: iak_diff
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call choose_int_vol_rot_sgs(iflag_4_supg, iele_fl_smp_stack,      &
     &     sgs_sf, iak_diff, i_vector)
!
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, mhd_fem1_wk%mlump_fl%ml, nod_fld1%ntot_phys,        &
     &    i_res, nod_fld1%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_res, node1, nod_comm, nod_fld1)
!
      end subroutine cal_rotation_sgs_fluid
!
!-----------------------------------------------------------------------
!
      subroutine cal_rotation_sgs_conduct(iflag_4_supg,                 &
     &          sgs_sf, iak_diff, i_res, i_vector)
!
      use m_geometry_data_MHD
!
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: i_vector, i_res
      integer(kind = kint), intent(in) :: iak_diff
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call choose_int_vol_rot_sgs(iflag_4_supg, iele_cd_smp_stack,      &
     &     sgs_sf, iak_diff, i_vector)
!
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, mhd_fem1_wk%mlump_cd%ml, nod_fld1%ntot_phys,        &
     &    i_res, nod_fld1%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_res, node1, nod_comm, nod_fld1)
!
      end subroutine cal_rotation_sgs_conduct
!
!-----------------------------------------------------------------------
!
      subroutine choose_int_vol_rot_sgs(iflag_4_supg, iele_fsmp_stack,  &
     &          sgs_sf, iak_diff, i_vector)
!
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_control_parameter
      use m_element_phys_data
      use m_sorted_node
      use m_jacobian_sf_grp
      use m_finite_element_matrix
      use m_filter_elength
!
      use int_sgs_vect_differences
      use int_sgs_vect_diff_upw
      use int_surf_rot_sgs
!
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: iak_diff, i_vector
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
       if ( iflag_4_supg .eq. id_magnetic_SUPG) then
        call int_sgs_rotation_upw                                       &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1, FEM1_elen,      &
     &      iele_fsmp_stack, intg_point_t_evo, ifilter_final,           &
     &      ak_diff(1,iak_diff), i_vector, fld_ele1%ntot_phys,          &
     &      iphys_ele%i_magne, fld_ele1%d_fld, fem1_wk, f1_nl)
       else if ( iflag_4_supg .eq. id_turn_ON) then
        call int_sgs_rotation_upw                                       &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1, FEM1_elen,      &
     &      iele_fsmp_stack, intg_point_t_evo, ifilter_final,           &
     &      ak_diff(1,iak_diff), i_vector, fld_ele1%ntot_phys,          &
     &      iphys_ele%i_velo, fld_ele1%d_fld, fem1_wk, f1_nl)
       else
        call int_sgs_rotation                                           &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1, FEM1_elen,      &
     &      iele_fsmp_stack, intg_point_t_evo, ifilter_final,           &
     &      ak_diff(1,iak_diff), i_vector, fem1_wk, f1_nl)
       end if
!
       call int_surf_rotation_sgs(node1, ele1, surf1, sf_grp1,          &
     &     nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen, sgs_sf,     &
     &     intg_point_t_evo, ifilter_final, iak_diff, i_vector,         &
     &     fem1_wk, f1_nl)
!
      end subroutine choose_int_vol_rot_sgs
!
!-----------------------------------------------------------------------
!
      end module cal_rotation_sgs
