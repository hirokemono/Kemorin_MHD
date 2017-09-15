!
!      module int_surf_div_induct_tsr_sgs
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine int_surf_div_induct_t_sgs                            &
!!     &         (node, ele, surf, sf_grp, nod_fld,                     &
!!     &          g_FEM, jac_sf_grp, rhs_tbl, FEM_elens, sgs_sf,        &
!!     &          n_int, i_filter, ncomp_diff, iak_diff_uxb,            &
!!     &          ak_diff, i_flux, i_v, i_b, fem_wk, surf_wk, f_nl)
!!      subroutine int_surf_commute_induct_t                            &
!!     &         (node, ele, surf, sf_grp, nod_fld,                     &
!!     &          g_FEM, jac_sf_grp, rhs_tbl, FEM_elens, sgs_sf,        &
!!     &          n_int, i_flux, i_filter, i_v, i_b,                    &
!!     &          fem_wk, surf_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
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
      use t_fem_gauss_int_coefs
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_surface_bc_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_div_induct_t_sgs                              &
     &         (node, ele, surf, sf_grp, nod_fld,                       &
     &          g_FEM, jac_sf_grp, rhs_tbl, FEM_elens, sgs_sf,          &
     &          n_int, i_filter, ncomp_diff, iak_diff_uxb,              &
     &          ak_diff, i_flux, i_v, i_b, fem_wk, surf_wk, f_nl)
!
      use delta_SGS_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp
!
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer (kind = kint), intent(in) :: i_b, i_v, i_flux
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
      integer(kind = kint), intent(in) :: ncomp_diff, iak_diff_uxb
      real(kind = kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2, nd, i, igrp, num
!
!
      num =  sgs_sf(1)%ngrp_sf_dat                                      &
     &     + sgs_sf(2)%ngrp_sf_dat                                      &
     &     + sgs_sf(3)%ngrp_sf_dat
      if(num .le. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do nd = 1, n_vector
        do i = 1, sgs_sf(nd)%ngrp_sf_dat
          igrp = sgs_sf(nd)%id_grp_sf_dat(i)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
          if (num .gt.0) then
!
            do k2 = 1, surf%nnod_4_surf
              call d_SGS_induct_t_2_each_surface                        &
     &           (node, ele, surf, sf_grp, nod_fld, igrp, k2, nd,       &
     &            i_flux, i_b, i_v, surf_wk%vect_sf)
              call fem_sf_grp_skv_sgs_div_flux_p                        &
     &           (ele, surf, sf_grp, g_FEM, jac_sf_grp, FEM_elens,      &
     &            igrp, k2, nd, n_int, i_filter,                        &
     &            surf_wk%dxe_sf, surf_wk%vect_sf,                      &
     &            ak_diff(1,iak_diff_uxb), dminus, fem_wk%sk6)
            end do
!
          end if
        end do
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_surf_div_induct_t_sgs
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_commute_induct_t                              &
     &         (node, ele, surf, sf_grp, nod_fld,                       &
     &          g_FEM, jac_sf_grp, rhs_tbl, FEM_elens, sgs_sf,          &
     &          n_int, i_flux, i_filter, i_v, i_b,                      &
     &          fem_wk, surf_wk, f_nl)
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
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer (kind = kint), intent(in) :: i_b, i_v, i_flux
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2, nd, i, igrp, num
!
!
      num =  sgs_sf(1)%ngrp_sf_dat                                      &
     &     + sgs_sf(2)%ngrp_sf_dat                                      &
     &     + sgs_sf(3)%ngrp_sf_dat
      if(num .le. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do nd = 1, n_vector
        do i = 1, sgs_sf(nd)%ngrp_sf_dat
          igrp = sgs_sf(nd)%id_grp_sf_dat(i)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
          if(num .gt. 0) then
!
            do k2 = 1, surf%nnod_4_surf
              call d_SGS_induct_t_2_each_surface                        &
     &           (node, ele, surf, sf_grp, nod_fld, igrp, k2, nd,       &
     &            i_flux, i_b, i_v, surf_wk%vect_sf)
              call fem_sf_grp_skv_div_f_commute_p                       &
     &           (ele, surf, sf_grp, g_FEM, jac_sf_grp, FEM_elens,      &
     &            igrp, k2, nd, n_int, i_filter,                        &
     &            surf_wk%dxe_sf, surf_wk%vect_sf, fem_wk%sk6)
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
