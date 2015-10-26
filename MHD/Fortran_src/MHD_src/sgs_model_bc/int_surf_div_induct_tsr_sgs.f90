!
!      module int_surf_div_induct_tsr_sgs
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine int_surf_div_induct_t_sgs(node, ele, surf,          &
!!     &          sf_grp, nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,     &
!!     &          n_int, i_flux, i_filter, i_v, i_b, fem_wk, f_nl)
!!      subroutine int_surf_commute_induct_t(node, ele, surf,          &
!!     &          sf_grp, nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,     &
!!     &          n_int, i_flux, i_filter, i_v, i_b, fem_wk, f_nl)
!
      module int_surf_div_induct_tsr_sgs
!
      use m_precision
!
      use m_constants
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
!
      use m_surf_data_magne
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_div_induct_t_sgs(node, ele, surf,             &
     &          sf_grp, nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,        &
     &          n_int, i_flux, i_filter, i_v, i_b, fem_wk, f_nl)
!
      use m_SGS_model_coefs
      use m_SGS_address
      use m_int_surface_data
!
      use delta_SGS_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp
!
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer (kind = kint), intent(in) :: i_b, i_v, i_flux
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2, nd, i, igrp, num
!
!
      if (sum(ngrp_sf_sgs_magne) .eq. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do nd = 1, n_vector
        do i = 1, ngrp_sf_sgs_magne(nd)
          igrp = id_grp_sf_sgs_magne(i,nd)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
          if (num .gt.0) then
!
            do k2 = 1, surf%nnod_4_surf
              call d_SGS_induct_t_2_each_surface                        &
     &           (node, ele, surf, sf_grp, nod_fld, igrp, k2, nd,       &
     &            i_flux, i_b, i_v, vect_sf)
              call fem_sf_grp_skv_sgs_div_flux_p                        &
     &           (ele, surf, sf_grp, jac_sf_grp, FEM_elens,             &
     &            igrp, k2, nd, n_int, i_filter, dxe_sf, vect_sf,       &
     &            ak_diff(1,iak_diff_uxb), dminus, fem_wk%sk6)
            end do
!
          end if
        end do
      end do
!
      call add3_skv_to_ff_v_smp                                        &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_surf_div_induct_t_sgs
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_commute_induct_t(node, ele, surf,            &
     &          sf_grp, nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,       &
     &          n_int, i_flux, i_filter, i_v, i_b, fem_wk, f_nl)
!
      use m_int_surface_data
!
      use delta_SGS_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer (kind = kint), intent(in) :: i_b, i_v, i_flux
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2, nd, i, igrp, num
!
!
      if (sum(ngrp_sf_sgs_magne) .eq. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do nd = 1, n_vector
        do i = 1, ngrp_sf_sgs_magne(nd)
          igrp = id_grp_sf_sgs_magne(i,nd)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
          if(num .gt. 0) then
!
            do k2 = 1, surf%nnod_4_surf
              call d_SGS_induct_t_2_each_surface                        &
     &           (node, ele, surf, sf_grp, nod_fld, igrp, k2, nd,       &
     &            i_flux, i_b, i_v, vect_sf)
              call fem_sf_grp_skv_div_f_commute_p                       &
     &           (ele, surf, sf_grp, jac_sf_grp, FEM_elens,             &
     &            igrp, k2, nd, n_int, i_filter, dxe_sf, vect_sf,       &
     &            fem_wk%sk6)
            end do
!
          end if
        end do
      end do
!
      call add3_skv_to_ff_v_smp                                        &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_surf_commute_induct_t
!
!-----------------------------------------------------------------------
!
      end module int_surf_div_induct_tsr_sgs
