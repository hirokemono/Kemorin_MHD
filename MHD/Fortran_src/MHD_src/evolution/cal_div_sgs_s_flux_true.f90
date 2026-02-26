!>@file   cal_div_sgs_s_flux_true.f90
!!@brief  module cal_div_sgs_s_flux_true
!!
!!@author H. Matsui and H.Okuda 
!!@date Programmed in July 2000 (ver 1.1)
!!        modified by H. Matsui in Oct., 2005
!!        modified by H. Matsui in Aug., 2007
!!
!>@brief  Finite elememt integration for true SGS terms
!!
!!@verbatim
!!      subroutine cal_div_sgs_s_flux_true_pre                          &
!!     &         (iflag_supg, num_int, dt, i_div_flux_true,             &
!!     &          i_flux, i_div_flux, i_field_f, i_velo_f,              &
!!     &          FEM_prm, nod_comm, node, ele, fluid, property,        &
!!     &          Snod_bcs, iphys_ele_base, ele_fld, fem_int, mlump_fl, &
!!     &          mhd_fem_wk, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
!!      subroutine cal_div_sgs_s_flux_true_post                         &
!!     &          (i_div_flux_true, i_div_flux, i_sgs_simi,             &
!!     &           filter_param, nod_comm, node, filtering,             &
!!     &           wk_filter, nod_fld, v_sol, SR_sig, SR_r)
!!        integer(kind = kint), intent(in) :: iflag_supg, num_int
!!        integer(kind = kint), intent(in) :: i_div_flux_true
!!        integer(kind = kint), intent(in) :: i_flux, i_div_flux
!!        integer(kind = kint), intent(in) :: i_field_f, i_velo_f
!!        real(kind = kreal), intent(in) :: dt
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(scalar_property), intent(in) :: property
!!        type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!        integer(kind = kint), intent(in) :: i_div_flux, i_sgs_simi
!!        type(SGS_filtering_params), intent(in) :: filter_param
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!@endverbatim
!
      module cal_div_sgs_s_flux_true
!
      use m_precision
!
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_mesh_data
      use t_geometry_data_MHD
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_SGS_model_addresses
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_filtering_data
      use t_bc_data_MHD
      use t_surface_bc_vector
      use t_surface_bc_velocity
      use t_surface_bc_data_MHD
      use t_material_property
      use t_scalar_property
      use t_FEM_SGS_model_coefs
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_work_FEM_integration
      use t_vector_for_solver
      use t_solver_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_s_flux_true_pre                            &
     &         (iflag_supg, num_int, dt, i_div_flux_true,               &
     &          i_flux, i_div_flux, i_field_f, i_velo_f,                &
     &          FEM_prm, nod_comm, node, ele, fluid, property,          &
     &          Snod_bcs, iphys_ele_base, ele_fld, fem_int, mlump_fl,   &
     &          mhd_fem_wk, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
!
      use t_bc_data_temp
      use t_surface_bc_data
      use products_nodal_fields_smp
      use cal_terms_for_heat
      use cal_fluxes
      use copy_nodal_fields
      use cal_filtering_scalars
!
      integer(kind = kint), intent(in) :: iflag_supg, num_int
      integer(kind = kint), intent(in) :: i_div_flux_true
      integer(kind = kint), intent(in) :: i_flux, i_div_flux
      integer(kind = kint), intent(in) :: i_field_f, i_velo_f
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(finite_element_integration), intent(in) :: fem_int
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call cal_phys_scalar_product_vector                               &
     &   (i_velo_f, i_field_f, i_flux, nod_fld)
      call cal_div_of_scalar_flux                                       &
     &   (i_div_flux, i_flux, iflag_supg, num_int, dt,                  &
     &    FEM_prm, nod_comm, node, ele, fluid, property, Snod_bcs,      &
     &    iphys_ele_base, ele_fld, fem_int, mlump_fl,                   &
     &    mhd_fem_wk, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
      call copy_scalar_component(nod_fld,                               &
     &    i_div_flux, i_div_flux_true)
!
      end subroutine cal_div_sgs_s_flux_true_pre
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_s_flux_true_post                           &
     &          (i_div_flux_true, i_div_flux, i_sgs_simi,               &
     &           filter_param, nod_comm, node, filtering,               &
     &           wk_filter, nod_fld, v_sol, SR_sig, SR_r)
!
      use cal_fluxes
      use copy_nodal_fields
      use cal_filtering_scalars
!
      integer(kind = kint), intent(in) :: i_div_flux_true
      integer(kind = kint), intent(in) :: i_div_flux, i_sgs_simi
!
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call copy_scalar_component(nod_fld,                               &
     &    i_div_flux_true, i_sgs_simi)
      call cal_filtered_scalar_whole                                    &
     &   (filter_param, nod_comm, node, filtering,                      &
     &    i_div_flux_true, i_div_flux, wk_filter, nod_fld,              &
     &    v_sol, SR_sig, SR_r)
      call subtract_2_nod_scalars(nod_fld,                              &
     &    i_div_flux_true, i_sgs_simi, i_div_flux_true)
!
      end subroutine cal_div_sgs_s_flux_true_post
!
!-----------------------------------------------------------------------
!
      end module cal_div_sgs_s_flux_true
