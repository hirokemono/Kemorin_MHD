!>@file   int_vol_coriolis_term.f90
!!@brief  module int_vol_coriolis_term
!!
!!@author H. Matsui and H.Okuda 
!!@date Programmed in July 2000 (ver 1.1)
!!        modified by H. Matsui in Oct., 2005
!!        modified by H. Matsui in Aug., 2007
!!
!>@brief  Finite elememt integration for Coriolis force
!!
!!@verbatim
!!      subroutine int_coriolis_nod_exp(node, fl_prop, mlump_fl,        &
!!     &          i_velo, nod_fld, f_l, f_nl)
!!      subroutine int_vol_coriolis_ele                                 &
!!     &         (num_int, node, ele, fluid, fl_prop, g_FEM, jac_3d,    &
!!     &          rhs_tbl, i_velo, nod_fld, fem_wk, f_l)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!
!!      subroutine int_buoyancy_nod_exp(node, fl_prop, mlump_fl,        &
!!     &          iphys, nod_fld, f_nl)
!!      subroutine set_boussinesq_density_at_node                       &
!!     &         (node, fl_prop, iphys, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module int_vol_coriolis_term
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
!
      use t_physical_property
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_finite_element_mat
      use t_jacobians
!
      use cal_coriolis
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_coriolis_nod_exp(node, fl_prop, mlump_fl,          &
     &          i_velo, nod_fld, f_l, f_nl)
!
      integer(kind = kint), intent(in) :: i_velo
      type(node_data), intent(in) :: node
      type(fluid_property), intent(in) :: fl_prop
      type (lumped_mass_matrices), intent(in) :: mlump_fl
      type(phys_data), intent(in) :: nod_fld
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      if (fl_prop%iflag_4_coriolis .eq. id_FORCE_at_node) then
        call cal_coriolis_nod(node%numnod, node%istack_nod_smp,         &
     &      fl_prop%sys_rot, fl_prop%coef_cor, mlump_fl%ml_o,           &
     &      nod_fld%ntot_phys, i_velo, nod_fld%d_fld, f_nl%ff)
      else if (fl_prop%iflag_4_coriolis .eq. id_Coriolis_nod_imp) then
        call cal_coriolis_nod(node%numnod, node%istack_nod_smp,         &
     &      fl_prop%sys_rot, fl_prop%coef_cor, mlump_fl%ml_o,           &
     &      nod_fld%ntot_phys, i_velo, nod_fld%d_fld, f_l%ff)
      end if
!
      end subroutine int_coriolis_nod_exp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_coriolis_ele                                   &
     &         (num_int, node, ele, fluid, fl_prop, g_FEM, jac_3d,      &
     &          rhs_tbl, i_velo, nod_fld, fem_wk, f_l)
!
      use t_geometry_data_MHD
      use t_fem_gauss_int_coefs
      use t_jacobians
!
      use nodal_fld_cst_to_element
      use cal_skv_to_ff_smp
      use fem_skv_inertia
!
      integer(kind = kint), intent(in) :: i_velo, num_int
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind=kint) :: k2
!
!
      do k2 = 1, ele%nnod_4_ele
        call vector_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_velo, fl_prop%coef_cor, fem_wk%vector_1)
        call fem_skv_coriolis                                           &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, fluid%istack_ele_fld_smp, g_FEM%max_int_point,      &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      num_int, k2, jac_3d%ntot_int, jac_3d%xjac,                  &
     &      jac_3d%an, jac_3d%an, fem_wk%vector_1, fl_prop%sys_rot,     &
     &      fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_vol_coriolis_ele
!
!-----------------------------------------------------------------------
!
      subroutine int_buoyancy_nod_exp(node, fl_prop, mlump_fl,          &
     &          iphys, nod_fld, f_nl)
!
      use set_buoyancy_at_node
!
      type(phys_address), intent(in) :: iphys
      type(node_data), intent(in) :: node
      type(fluid_property), intent(in) :: fl_prop
      type (lumped_mass_matrices), intent(in) :: mlump_fl
      type(phys_data), intent(inout) :: nod_fld
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
      if     (fl_prop%iflag_4_filter_comp_buo .ne. id_FORCE_at_node     &
     &  .and. fl_prop%iflag_4_filter_gravity .ne.  id_FORCE_at_node     &
     &  .and. fl_prop%iflag_4_composit_buo .ne. id_FORCE_at_node        &
     &  .and. fl_prop%iflag_4_gravity .ne. id_FORCE_at_node) return
!
! ---------  set buoyancy at each node
!
      if(fl_prop%iflag_4_composit_buo .eq. id_FORCE_at_node             &
     &   .and. fl_prop%iflag_4_gravity .eq. id_FORCE_at_node) then
        call set_double_gravity_2_each_node(iphys%base%i_temp,          &
     &      iphys%base%i_light, iphys%forces%i_buoyancy,                &
     &      fl_prop%i_grav, fl_prop%coef_buo, fl_prop%coef_comp_buo,    &
     &      fl_prop%grav, node, nod_fld)
!
      else if(fl_prop%iflag_4_filter_comp_buo .eq. id_FORCE_at_node     &
     & .and. fl_prop%iflag_4_filter_gravity .eq. id_FORCE_at_node) then
        call set_double_gravity_2_each_node                             &
     &     (iphys%filter_fld%i_temp, iphys%i_filter_comp,               &
     &      iphys%forces%i_buoyancy, fl_prop%i_grav,                    &
     &      fl_prop%coef_buo, fl_prop%coef_comp_buo, fl_prop%grav,      &
     &      node, nod_fld)
!
      else if(fl_prop%iflag_4_filter_comp_buo .eq. id_FORCE_at_node     &
     & .and. fl_prop%iflag_4_gravity .eq. id_FORCE_at_node) then
        call set_double_gravity_2_each_node                             &
     &     (iphys%base%i_temp, iphys%i_filter_comp,                     &
     &      iphys%forces%i_buoyancy, fl_prop%i_grav,                    &
     &      fl_prop%coef_buo, fl_prop%coef_comp_buo, fl_prop%grav,      &
     &      node, nod_fld)
!
      else if(fl_prop%iflag_4_composit_buo .eq. id_FORCE_at_node        &
     & .and. fl_prop%iflag_4_filter_gravity .eq. id_FORCE_at_node) then
        call set_double_gravity_2_each_node                             &
     &     (iphys%filter_fld%i_temp, iphys%base%i_light,                &
     &      iphys%forces%i_buoyancy, fl_prop%i_grav,                    &
     &      fl_prop%coef_buo, fl_prop%coef_comp_buo, fl_prop%grav,      &
     &      node, nod_fld)
!
!
      else if (fl_prop%iflag_4_gravity .eq. id_FORCE_at_node) then
        call set_gravity_2_each_node                                    &
     &     (iphys%base%i_temp, iphys%forces%i_buoyancy,                 &
     &      fl_prop%i_grav, fl_prop%coef_buo, fl_prop%grav,             &
     &      node, nod_fld)
!
      else if (fl_prop%iflag_4_composit_buo .eq. id_FORCE_at_node) then
        call set_gravity_2_each_node                                    &
     &     (iphys%base%i_light, iphys%forces%i_buoyancy,                &
     &      fl_prop%i_grav, fl_prop%coef_comp_buo, fl_prop%grav,        &
     &      node, nod_fld)
!
      else if(fl_prop%iflag_4_filter_gravity .eq. id_FORCE_at_node)     &
     &    then
        call set_gravity_2_each_node                                    &
     &     (iphys%filter_fld%i_temp, iphys%forces%i_buoyancy,           &
     &      fl_prop%i_grav, fl_prop%coef_buo, fl_prop%grav,             &
     &      node, nod_fld)
      else if(fl_prop%iflag_4_filter_comp_buo .eq. id_FORCE_at_node)    &
     &    then
        call set_gravity_2_each_node                                    &
     &     (iphys%i_filter_comp, iphys%forces%i_buoyancy,               &
     &      fl_prop%i_grav, fl_prop%coef_comp_buo, fl_prop%grav,        &
     &      node, nod_fld)
      end if
!
      call int_vol_buoyancy_nod(node%numnod, node%istack_nod_smp,       &
     &    nod_fld%ntot_phys, iphys%forces%i_buoyancy, nod_fld%d_fld,    &
     &    mlump_fl%ml_o, f_nl%ff)
!
      end subroutine int_buoyancy_nod_exp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_boussinesq_density_at_node                         &
     &         (node, fl_prop, iphys, nod_fld)
!
      use set_buoyancy_at_node
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(inout) :: nod_fld
!
!
      call set_boussinesq_density_2_node                                &
     &   (node%numnod, node%istack_nod_smp,                             &
     &    fl_prop%coef_buo, fl_prop%coef_comp_buo,                      &
     &    nod_fld%ntot_phys, iphys%base%i_temp, iphys%base%i_light,     &
     &    iphys%base%i_density, nod_fld%d_fld)
!
      end subroutine set_boussinesq_density_at_node
!
!  ---------------------------------------------------------------------
!
      end module int_vol_coriolis_term
