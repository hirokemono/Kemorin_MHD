!>@file   lead_physical_values.f90
!!        module lead_physical_values
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate many kind of field data
!!
!!@verbatim
!!      subroutine lead_fields_by_FEM
!!        type(layering_tbl), intent(in) :: layer_tbl
!!@endverbatim
!
      module lead_physical_values
!
      use m_precision
!
      implicit none
!
      private :: cal_energy_fluxes
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine lead_fields_by_FEM(layer_tbl)
!
      use m_machine_parameter
      use m_t_step_parameter
      use m_geometry_data
      use m_node_phys_data
!
      use t_layering_ele_list
!
      use update_after_evolution
      use itp_potential_on_edge
      use MHD_field_by_rotation
      use cal_helicities
      use output_viz_file_control
!
      type(layering_tbl), intent(in) :: layer_tbl
      integer (kind =kint) :: iflag
!
!
      call set_lead_physical_values_flag(iflag)
!
      if ( iflag.eq.0 ) then
        if (iflag_debug.gt.0) write(*,*) 'cal_potential_on_edge'
        call cal_potential_on_edge(node1, ele1, edge1, iphys, nod_fld1)
!
        if (iflag_debug.gt.0) write(*,*) 'update_fields'
        call update_fields(layer_tbl)
!
        call cal_field_by_rotation
!
        if (iflag_debug.gt.0) write(*,*) 'cal_helicity'
        call cal_helicity(node1, iphys, nod_fld1)
!
        if (iflag_debug.gt.0) write(*,*) 'cal_energy_fluxes'
        call cal_energy_fluxes
!
      end if
!
      end subroutine lead_fields_by_FEM
!
! ----------------------------------------------------------------------
!
      subroutine cal_energy_fluxes
!
      use m_machine_parameter
!
      use m_nod_comm_table
      use m_geometry_data
      use m_node_phys_data
      use m_physical_property
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_MHD_forces_4_monitor
      use cal_sgs_4_monitor
      use cal_true_sgs_terms
!
!
      call cal_true_sgs_terms_pre
!
      call cal_sgs_terms_4_monitor
!
      call cal_fluxes_4_monitor
!
      call cal_forces_4_monitor
      call cal_diff_of_sgs_terms
!
      call cal_true_sgs_terms_post
!
      call cal_work_4_forces                                            &
     &   (nod_comm, node1, ele1, iphys, jac1_3d_q, rhs_tbl1,            &
     &    mhd_fem1_wk, fem1_wk, f1_nl, nod_fld1)
!
      call cal_work_4_sgs_terms
!
      end subroutine cal_energy_fluxes
!
!  ---------------------------------------------------------------------
!
      end module lead_physical_values
