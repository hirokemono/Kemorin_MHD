!
!     module int_vol_buoyancy
!
!     numerical integration for finite elememt equations of momentum
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!        modified by H. Matsui on Aug., 2012
!
!!      subroutine int_vol_buoyancy_pg                                  &
!!     &         (node, ele, jac_3d, fl_prop, rhs_tbl, nod_fld,         &
!!     &          iele_fsmp_stack, n_int, i_source, ak_buo,             &
!!     &          fem_wk, f_nl)
!!      subroutine int_vol_buoyancy_upw                                 &
!!     &         (node, ele, jac_3d, fl_prop, rhs_tbl, nod_fld,         &
!!     &          iele_fsmp_stack, n_int, dt, i_source, ak_buo,         &
!!     &          ncomp_ele, ie_upw, d_ele, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module int_vol_buoyancy
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_geometry_data
      use t_phys_data
      use t_phys_data
      use t_jacobians
      use t_physical_property
      use t_table_FEM_const
      use t_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_buoyancy_pg                                    &
     &         (node, ele, jac_3d, fl_prop, rhs_tbl, nod_fld,           &
     &          iele_fsmp_stack, n_int, i_source, ak_buo,               &
     &          fem_wk, f_nl)
!
      use gravity_vec_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_nodal_field_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_source
      real(kind = kreal), intent(in) :: ak_buo(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call set_gravity_vec_each_ele(node, ele, nod_fld, k2, i_source, &
     &      fl_prop%i_grav, fl_prop%grav, ak_buo, fem_wk%vector_1)
        call fem_skv_vector_type(iele_fsmp_stack, n_int, k2,            &
     &      ele, jac_3d, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_buoyancy_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_buoyancy_upw                                   &
     &         (node, ele, jac_3d, fl_prop, rhs_tbl, nod_fld,           &
     &          iele_fsmp_stack, n_int, dt, i_source, ak_buo,           &
     &          ncomp_ele, ie_upw, d_ele, fem_wk, f_nl)
!
      use gravity_vec_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_nodal_fld_upwind
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_source
      real(kind = kreal), intent(in) :: ak_buo(ele%numele)
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call set_gravity_vec_each_ele(node, ele, nod_fld, k2, i_source, &
     &      fl_prop%i_grav, fl_prop%grav, ak_buo, fem_wk%vector_1)
        call fem_skv_vector_field_upwind                                &
     &     (iele_fsmp_stack, n_int, k2, dt, d_ele(1,ie_upw),            &
     &      ele, jac_3d, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_buoyancy_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_buoyancy
