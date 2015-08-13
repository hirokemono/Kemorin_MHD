!
!     module cal_magnetic_terms
!
!     Written by H. Matsui on June, 2005
!
!      subroutine cal_terms_4_magnetic(i_field)
!      subroutine cal_magnetic_diffusion
!
      module cal_magnetic_terms
!
      use m_precision
!
      use m_control_parameter
      use m_finite_element_matrix
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use cal_multi_pass
      use nod_phys_send_recv
      use set_magne_boundary
      use int_surf_magne_pre
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_terms_4_magnetic(i_field)
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
!
      use int_vol_magne_monitor
!
      integer (kind=kint), intent(in) :: i_field
!
!
      call reset_ff_smps
!
      if (iflag_mag_supg .gt. id_turn_OFF) then
       call int_vol_magne_monitor_upm(i_field)
      else
       call int_vol_magne_monitor_pg(i_field)
      end if
!
      call int_surf_magne_monitor(i_field)
!
      call cal_t_evo_4_vector_cd(iflag_mag_supg)
      call set_boundary_magne_4_rhs
!
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    d_nod(1,i_field), ff_nl, ml_cd)
      call vector_send_recv(i_field)
!
      end subroutine cal_terms_4_magnetic
!
!-----------------------------------------------------------------------
!
      subroutine cal_magnetic_diffusion
!
      use m_geometry_data
      use m_node_phys_address
      use m_phys_constants
      use m_node_phys_data
!
      use int_vol_diffusion_ele
!
!
      call reset_ff_smps
!
      call int_vol_m_diffuse_ele_monitor
!
      call int_surf_magne_monitor(iphys%i_b_diffuse)
!
      call set_ff_nl_smp_2_ff(n_vector)
!
      call set_boundary_magne_4_rhs
!
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    d_nod(1,iphys%i_b_diffuse), ff, ml)
      call vector_send_recv(iphys%i_b_diffuse)
!
      end subroutine cal_magnetic_diffusion
!
!-----------------------------------------------------------------------
!
      end module cal_magnetic_terms
