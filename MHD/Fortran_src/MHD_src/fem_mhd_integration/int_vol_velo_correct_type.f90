!
!     module int_vol_velo_correct_type
!
!      Written by H. Matsui on june, 2005
!
!      subroutine int_vol_velo_co_type
!
      module int_vol_velo_correct_type
!
      use m_precision
      use m_control_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_velo_co_type(mesh, jac_3d, jac_3d_l,           &
     &          FEM_elens, rhs_tbl, nod_fld, iele_fsmp_stack, i_scalar, &
     &          ncomp_diff, ak_diff, fem_wk, f_nl)
!
      use m_machine_parameter
      use m_geometry_constants
!
      use m_SGS_address
!
      use t_mesh_data
      use t_phys_data
      use t_jacobian_3d
      use t_filter_elength
      use t_table_FEM_const
      use t_finite_element_mat
!
      use int_vol_sgs_fractional_type
      use int_vol_fractional_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: i_scalar
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: ncomp_diff
      real(kind=kreal), intent(in)                                      &
     &              :: ak_diff(mesh%ele%numele,ncomp_diff)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
      if (iflag_commute_velo .eq. id_SGS_commute_ON) then
        call int_vol_sgs_solenoidal_co_type(mesh, jac_3d, jac_3d_l,     &
     &          FEM_elens, rhs_tbl, nod_fld, iele_fsmp_stack,           &
     &          intg_point_poisson, i_scalar, ifilter_final,            &
     &          ak_diff(1,iak_diff_v), fem_wk, f_nl)
      else
        call int_vol_solenoidal_co_type(mesh, jac_3d, jac_3d_l,         &
     &      rhs_tbl, nod_fld, iele_fsmp_stack, intg_point_poisson,      &
     &      i_scalar, fem_wk, f_nl)
      end if
!
      end subroutine int_vol_velo_co_type
!
! ----------------------------------------------------------------------
!
      end module int_vol_velo_correct_type
