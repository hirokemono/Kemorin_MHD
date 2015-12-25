!
!      module int_vol_coriolis_term
!
!        programmed by H.Matsui on Nov., 2008
!
!      subroutine int_coriolis_nod_exp
!      subroutine int_vol_coriolis_crank_ele
!
!      subroutine int_buoyancy_nod_exp
!      subroutine set_boussinesq_density_at_node
!        type(node_data), intent(in) :: node
!        type(phys_address), intent(in) :: iphys
!        type(phys_data), intent(inout) :: nod_fld
!
      module int_vol_coriolis_term
!
      use m_precision
!
      use m_phys_constants
      use m_physical_property
      use cal_coriolis
!
      use m_control_parameter
      use m_finite_element_matrix
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_coriolis_nod_exp
!
      use m_geometry_data
      use m_node_phys_data
      use m_int_vol_data
!
!
      if ( iflag_4_coriolis .eq. id_FORCE_at_node) then
        call cal_coriolis_nod(node1%numnod, node1%istack_nod_smp,       &
     &      angular, coef_cor, mhd_fem1_wk%mlump_fl%ml_o,               &
     &      nod_fld1%ntot_phys, iphys%i_velo, nod_fld1%d_fld, f1_nl%ff)
      else if ( iflag_4_coriolis .eq. id_Coriolis_nod_imp) then
        call cal_coriolis_nod(node1%numnod, node1%istack_nod_smp,       &
     &      angular, coef_cor, mhd_fem1_wk%mlump_fl%ml_o,               &
     &      nod_fld1%ntot_phys, iphys%i_velo, nod_fld1%d_fld, f1_l%ff)
      end if
!
      end subroutine int_coriolis_nod_exp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_coriolis_crank_ele
!
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_id_4_node
      use m_int_vol_data
      use m_jacobians
!
      use nodal_fld_cst_to_element
      use cal_skv_to_ff_smp
      use fem_skv_nonlinear_type
!
      integer(kind=kint) :: k2
!
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_cst_phys_2_each_ele(node1, ele1, nod_fld1,          &
     &      k2, iphys%i_velo, coef_cor, fem1_wk%vector_1)
        call fem_skv_coriolis_type                                      &
     &     (fluid1%istack_ele_fld_smp, intg_point_t_evo,                &
     &      k2, fem1_wk%vector_1, angular, ele1, jac1_3d_q,             &
     &      fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_l%ff_smp)
!
      end subroutine int_vol_coriolis_crank_ele
!
!-----------------------------------------------------------------------
!
      subroutine int_buoyancy_nod_exp
!
      use m_geometry_data
      use m_node_phys_data
      use m_int_vol_data
      use set_buoyancy_at_node
!
! ---------  set buoyancy at each node
!
      if(iflag_4_gravity .eq. id_FORCE_at_node                          &
     &     .and. iflag_4_composit_buo .eq. id_FORCE_at_node) then
        call set_double_gravity_2_each_node(iphys%i_temp,               &
     &      iphys%i_light, iphys%i_buoyancy, coef_buo, coef_comp_buo)
        call int_vol_buoyancy_nod(node1%numnod, node1%istack_nod_smp,   &
     &      nod_fld1%ntot_phys, iphys%i_buoyancy, nod_fld1%d_fld,       &
     &      mhd_fem1_wk%mlump_fl%ml_o, f1_nl%ff)
!
      else if (iflag_4_gravity .eq. id_FORCE_at_node) then
        call set_gravity_2_each_node(iphys%i_temp, iphys%i_buoyancy,    &
     &      coef_buo)
        call int_vol_buoyancy_nod(node1%numnod, node1%istack_nod_smp,   &
     &      nod_fld1%ntot_phys, iphys%i_buoyancy, nod_fld1%d_fld,       &
     &      mhd_fem1_wk%mlump_fl%ml_o, f1_nl%ff)
!
      else if (iflag_4_composit_buo .eq. id_FORCE_at_node) then
        call set_gravity_2_each_node(iphys%i_light, iphys%i_comp_buo,   &
     &      coef_comp_buo)
        call int_vol_buoyancy_nod(node1%numnod, node1%istack_nod_smp,   &
     &      nod_fld1%ntot_phys, iphys%i_comp_buo, nod_fld1%d_fld,       &
     &      mhd_fem1_wk%mlump_fl%ml_o, f1_nl%ff)
!
      else if (iflag_4_filter_gravity .eq. id_FORCE_at_node) then
        call set_gravity_2_each_node(iphys%i_filter_temp,               &
     &      iphys%i_filter_buo, coef_buo)
        call int_vol_buoyancy_nod(node1%numnod, node1%istack_nod_smp,   &
     &      nod_fld1%ntot_phys, iphys%i_filter_buo, nod_fld1%d_fld,     &
     &      mhd_fem1_wk%mlump_fl%ml_o, f1_nl%ff)
      end if
!
      end subroutine int_buoyancy_nod_exp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_boussinesq_density_at_node(node, iphys, nod_fld)
!
      use t_geometry_data
      use t_phys_data
      use set_buoyancy_at_node
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      call set_boussinesq_density_2_node                                &
     &   (node%numnod, node%istack_nod_smp, coef_buo, coef_comp_buo,    &
     &    nod_fld%ntot_phys, iphys%i_temp, iphys%i_light,               &
     &    iphys%i_density, nod_fld%d_fld)
!
      end subroutine set_boussinesq_density_at_node
!
!  ---------------------------------------------------------------------
!
      end module int_vol_coriolis_term
