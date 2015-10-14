!
!     module cal_momentum_terms
!
!     Written by H. Matsui on June, 2005
!
!      subroutine cal_terms_4_momentum(i_field)
!      subroutine cal_viscous_diffusion
!
      module cal_momentum_terms
!
      use m_precision
!
      use m_control_parameter
      use m_finite_element_matrix
!
      use cal_multi_pass
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
      use set_velocity_boundary
      use int_surf_velo_pre
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_terms_4_momentum(i_field)
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use m_int_vol_data
!
      use int_vol_velo_monitor
!
      integer (kind=kint), intent(in) :: i_field
!
!
      call reset_ff_smps
!
      if (iflag_velo_supg .eq. id_turn_ON) then
       call int_vol_velo_monitor_upw(i_field)
      else if (iflag_velo_supg .eq. id_magnetic_SUPG) then
       call int_vol_velo_monitor_upm(i_field)
      else
       call int_vol_velo_monitor_pg(i_field)
      end if
!
      call int_surf_velo_monitor(i_field)
!
      call cal_t_evo_4_vector_fl(iflag_velo_supg)
!       call set_boundary_velo_4_rhs
!
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    ff_nl, mhd_fem1_wk%ml_fl,                                     &
     &    nod_fld1%ntot_phys, i_field, nod_fld1%d_fld)
      call vector_send_recv                                             &
     &   (nod_fld1%ntot_phys, i_field, nod_fld1%d_fld)
!
      end subroutine cal_terms_4_momentum
!
!-----------------------------------------------------------------------
!
      subroutine cal_viscous_diffusion
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
      use m_int_vol_data
!
      use int_vol_diffusion_ele
!
!
      call reset_ff_smps
      call int_vol_viscous_ele_monitor
!
      call int_surf_velo_monitor(iphys%i_v_diffuse)
!
      call set_ff_nl_smp_2_ff(node1, rhs_tbl1, n_vector)
!
      call set_boundary_velo_4_rhs
!
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_l%ff, mhd_fem1_wk%ml_fl,                                   &
     &    nod_fld1%ntot_phys, iphys%i_v_diffuse, nod_fld1%d_fld)
!
      call vector_send_recv                                             &
     &   (nod_fld1%ntot_phys, iphys%i_v_diffuse, nod_fld1%d_fld)
!
      end subroutine cal_viscous_diffusion
!
!-----------------------------------------------------------------------
!
      end module cal_momentum_terms
