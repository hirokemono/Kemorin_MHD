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
      use m_node_phys_address
      use m_node_phys_data
!
!
      if ( iflag_4_coriolis .eq. id_FORCE_at_node) then
        call cal_coriolis_nod(node1%numnod, node1%istack_nod_smp,       &
     &      angular, coef_cor, ml_o_fl, nod_fld1%ntot_phys,             &
     &      iphys%i_velo, nod_fld1%d_fld, ff_nl)
      else if ( iflag_4_coriolis .eq. id_Coriolis_nod_imp) then
        call cal_coriolis_nod(node1%numnod, node1%istack_nod_smp,       &
     &      angular, coef_cor, ml_o_fl, nod_fld1%ntot_phys,             &
     &      iphys%i_velo, nod_fld1%d_fld, ff)
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
      use m_geometry_data_MHD
      use m_node_phys_address
      use m_int_vol_data
      use m_jacobians
!
      use nodal_fld_cst_to_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_nonlinear_type
!
      integer(kind=kint) :: k2
!
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_cst_phys_2_each_ele(k2, iphys%i_velo,               &
     &      coef_cor, fem1_wk%vector_1)
        call fem_skv_coriolis_type(iele_fl_smp_stack, intg_point_t_evo, &
     &      k2, fem1_wk%vector_1, angular, ele1, jac1_3d_q,             &
     &      fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_smp, fem1_wk%sk6)
!
      end subroutine int_vol_coriolis_crank_ele
!
!-----------------------------------------------------------------------
!
      subroutine int_buoyancy_nod_exp
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
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
     &      ml_o_fl, ff_nl)
!
      else if (iflag_4_gravity .eq. id_FORCE_at_node) then
        call set_gravity_2_each_node(iphys%i_temp, iphys%i_buoyancy,    &
     &      coef_buo)
        call int_vol_buoyancy_nod(node1%numnod, node1%istack_nod_smp,   &
     &      nod_fld1%ntot_phys, iphys%i_buoyancy, nod_fld1%d_fld,       &
     &      ml_o_fl, ff_nl)
!
      else if (iflag_4_composit_buo .eq. id_FORCE_at_node) then
        call set_gravity_2_each_node(iphys%i_light, iphys%i_comp_buo,   &
     &      coef_comp_buo)
        call int_vol_buoyancy_nod(node1%numnod, node1%istack_nod_smp,   &
     &      nod_fld1%ntot_phys, iphys%i_comp_buo, nod_fld1%d_fld,       &
     &      ml_o_fl, ff_nl)
!
      else if (iflag_4_filter_gravity .eq. id_FORCE_at_node) then
        call set_gravity_2_each_node(iphys%i_filter_temp,               &
     &      iphys%i_filter_buo, coef_buo)
        call int_vol_buoyancy_nod(node1%numnod, node1%istack_nod_smp,   &
     &      nod_fld1%ntot_phys, iphys%i_filter_buo, nod_fld1%d_fld,     &
     &      ml_o_fl, ff_nl)
      end if
!
      end subroutine int_buoyancy_nod_exp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_boussinesq_density_at_node
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use set_buoyancy_at_node
!
!
      call set_boussinesq_density_2_node                                &
     &   (node1%numnod, node1%istack_nod_smp, coef_buo, coef_comp_buo,  &
     &    nod_fld1%ntot_phys, iphys%i_temp, iphys%i_light,              &
     &    iphys%i_density, nod_fld1%d_fld)
!
      end subroutine set_boussinesq_density_at_node
!
!  ---------------------------------------------------------------------
!
      end module int_vol_coriolis_term
