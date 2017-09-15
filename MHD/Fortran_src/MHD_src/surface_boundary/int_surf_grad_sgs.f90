!
!      module int_surf_grad_sgs
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine int_surf_sgs_velo_co_ele                             &
!!     &         (node, ele, surf, sf_grp, nod_fld,                     &
!!     &          jac_sf_grp_q, jac_sf_grp_l, rhs_tbl, FEM_elens,       &
!!     &          n_int, ngrp_sf, id_grp_sf, i_filter,                  &
!!     &          ncomp_diff, iak_diff, ak_diff, i_comp,                &
!!     &          fem_wk, surf_wk, f_nl)
!!
!!      subroutine int_surf_gradient_sgs(node, ele, surf,               &
!!     &          sf_grp, nod_fld, jac_sf_grp_q, rhs_tbl, FEM_elens,    &
!!     &          n_int, ngrp_sf, id_grp_sf, i_filter,                  &
!!     &          ncomp_diff, iak_diff, ak_diff, i_scalar,              &
!!     &          fem_wk, surf_wk, f_nl)
!!      subroutine int_surf_grad_commute_sgs(node, ele, surf,           &
!!     &          sf_grp, nod_fld, jac_sf_grp_q, rhs_tbl, FEM_elens,    &
!!     &          n_int, ngrp_sf, id_grp_sf, i_filter, i_scalar,        &
!!     &          fem_wk, surf_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module int_surf_grad_sgs
!
      use m_precision
      use m_phys_constants
!
      use m_constants
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use m_fem_gauss_int_coefs
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_sgs_velo_co_ele                               &
     &         (node, ele, surf, sf_grp, nod_fld,                       &
     &          jac_sf_grp_q, jac_sf_grp_l, rhs_tbl, FEM_elens,         &
     &          n_int, ngrp_sf, id_grp_sf, i_filter,                    &
     &          ncomp_diff, iak_diff, ak_diff, i_comp,                  &
     &          fem_wk, surf_wk, f_nl)
!
      use delta_phys_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp
!
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: n_int, ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint), intent(in) :: i_comp
      integer(kind = kint), intent(in) :: ncomp_diff, iak_diff
      real(kind = kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
!  ---------  set number of integral points
!
      if (ngrp_sf.eq.0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if(num .gt. 0) then
!
          do k2 = 1, surf%nnod_4_surf
            call dlt_scl_phys_2_each_surf_cst                           &
     &         (node, ele, surf, sf_grp, nod_fld, igrp, k2,             &
     &          i_comp, dminus, surf_wk%scalar_sf)
            call fem_sf_grp_skv_sgs_velo_co_p(ele, surf, sf_grp,        &
     &          g_FEM1, jac_sf_grp_q, jac_sf_grp_l, FEM_elens, igrp,    &
     &          k2, n_int, i_filter, surf_wk%dxe_sf, surf_wk%scalar_sf, &
     &          ak_diff(1,iak_diff), fem_wk%sk6)
          end do
!
        end if
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_surf_sgs_velo_co_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_surf_gradient_sgs(node, ele, surf,                 &
     &          sf_grp, nod_fld, jac_sf_grp_q, rhs_tbl, FEM_elens,      &
     &          n_int, ngrp_sf, id_grp_sf, i_filter,                    &
     &          ncomp_diff, iak_diff, ak_diff, i_scalar,                &
     &          fem_wk, surf_wk, f_nl)
!
      use delta_phys_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp
!
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: n_int, ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_scalar, i_filter
      integer(kind = kint), intent(in) :: ncomp_diff, iak_diff
      real (kind = kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
!  ---------  set number of integral points
!
      if (ngrp_sf.eq.0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if (num .gt. 0) then
!
          do k2 = 1, surf%nnod_4_surf
            call dlt_scl_phys_2_each_surface                            &
     &         (node, ele, surf, sf_grp, nod_fld, igrp, k2,             &
     &          i_scalar, surf_wk%scalar_sf)
            call fem_sf_grp_skv_sgs_grad_p(ele, surf, sf_grp,           &
     &          g_FEM1, jac_sf_grp_q, FEM_elens, igrp, k2, n_int,       &
     &          i_filter, surf_wk%dxe_sf, surf_wk%scalar_sf,            &
     &          ak_diff(1,iak_diff), one, fem_wk%sk6)
          end do
!
        end if
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_surf_gradient_sgs
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_grad_commute_sgs(node, ele, surf,             &
     &          sf_grp, nod_fld, jac_sf_grp_q, rhs_tbl, FEM_elens,      &
     &          n_int, ngrp_sf, id_grp_sf, i_filter, i_scalar,          &
     &          fem_wk, surf_wk, f_nl)
!
      use delta_phys_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp
!
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: n_int, ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_scalar, i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
!  ---------  set number of integral points
!
      if (ngrp_sf.eq.0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if (num .gt. 0) then
!
          do k2 = 1, surf%nnod_4_surf
            call dlt_scl_phys_2_each_surface                            &
     &         (node, ele, surf, sf_grp, nod_fld, igrp, k2,             &
     &          i_scalar, surf_wk%scalar_sf)
            call fem_sf_grp_skv_grad_commute_p(ele, surf, sf_grp,       &
     &          g_FEM1, jac_sf_grp_q, FEM_elens, igrp, k2,              &
     &          n_int, i_filter, surf_wk%dxe_sf, surf_wk%scalar_sf,     &
     &          fem_wk%sk6)
          end do
!
        end if
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_surf_grad_commute_sgs
!
!-----------------------------------------------------------------------
!
      end module int_surf_grad_sgs
