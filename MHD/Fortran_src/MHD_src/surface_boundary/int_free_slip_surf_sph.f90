!>@file   int_free_slip_surf_sph.f90
!!@brief  module int_free_slip_surf_sph
!!
!!@author H. Matsui
!!@date Written  by H. Matsui on Dec., 2003
!@n      modified by H. Matsui on Aug., 2005
!
!>@brief  FEM integration for stress free boundary
!!         or pseudo vacuum boundary
!!
!!
!!@verbatim
!!      subroutine int_free_slip_surf_sph_out(node, ele, surf, sf_grp,  &
!!     &          nod_fld, g_FEM, jac_sf_grp, rhs_tbl,                  &
!!     &          n_int, ngrp_surf_outside, id_grp_outside, i_field,    &
!!     &          ak_d, fem_wk, surf_wk, f_l)
!!      subroutine int_free_slip_surf_sph_in(node, ele, surf, sf_grp,   &
!!     &          nod_fld, g_FEM, jac_sf_grp, rhs_tbl,                  &
!!     &          n_int, ngrp_surf_inside, id_grp_inside, i_field,      &
!!     &          ak_d, fem_wk, surf_wk, f_l)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!@endverbatim
!!
!@param    n_int       numbper of integration points
!@param    i_field     field address
!!
!@param    ngrp_surf_outside
!!                     Number of surface group for outer boundary
!@param    id_grp_outside(ngrp_surf_outside)
!!                     surface group ID for outer boundary
!@param    ngrp_surf_inside     field address
!!                     Number of surface group for inner boundary
!@param    id_grp_inside(ngrp_surf_inside)     field address
!!                     surface group ID for inner boundary
!
      module int_free_slip_surf_sph
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_fem_gauss_int_coefs
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
!
      use fem_surf_skv_poisson_type
      use cal_skv_to_ff_smp
      use node_phys_2_each_surface
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine int_free_slip_surf_sph_out(node, ele, surf, sf_grp,    &
     &          nod_fld, g_FEM, jac_sf_grp, rhs_tbl,                    &
     &          n_int, ngrp_surf_outside, id_grp_outside, i_field,      &
     &          ak_d, fem_wk, surf_wk, f_l)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer (kind = kint), intent(in) :: n_int, i_field
      integer (kind = kint), intent(in) :: ngrp_surf_outside
      integer (kind = kint), intent(in)                                 &
     &       :: id_grp_outside(ngrp_surf_outside)
      real(kind = kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer (kind = kint) :: k2, i, igrp, num
!
!
      if(ngrp_surf_outside .le. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do i = 1, ngrp_surf_outside
        igrp = id_grp_outside(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if (num .le.0 ) return
!
        do k2 = 1, surf%nnod_4_surf
          call vector_phys_2_each_surface                               &
     &       (node, ele, surf, sf_grp, nod_fld, igrp, k2, i_field,      &
     &        surf_wk%vect_sf)
          call fem_surf_skv_trq_sph_out                                 &
     &       (ele, surf, sf_grp, g_FEM, jac_sf_grp, igrp, k2, n_int,    &
     &        ak_d, surf_wk%xe_sf, surf_wk%vect_sf, fem_wk%sk6)
        end do
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_free_slip_surf_sph_out
!
! ----------------------------------------------------------------------
!
      subroutine int_free_slip_surf_sph_in(node, ele, surf, sf_grp,     &
     &          nod_fld, g_FEM, jac_sf_grp, rhs_tbl,                    &
     &          n_int, ngrp_surf_inside, id_grp_inside, i_field,        &
     &          ak_d, fem_wk, surf_wk, f_l)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer (kind = kint), intent(in) :: n_int, i_field
      integer (kind = kint), intent(in) ::ngrp_surf_inside
      integer (kind = kint), intent(in)                                 &
     &       :: id_grp_inside(ngrp_surf_inside)
      real(kind = kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer (kind = kint) :: k2, i, igrp, num
!
!
      if (ngrp_surf_inside .le. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do i = 1, ngrp_surf_inside
        igrp = id_grp_inside(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if (num .le.0 ) exit
!
        do k2 = 1, surf%nnod_4_surf
          call vector_phys_2_each_surf_cst                              &
     &       (node, ele, surf, sf_grp, nod_fld, igrp, k2,               &
     &        i_field, dminus, surf_wk%vect_sf)
          call fem_surf_skv_trq_sph_out                                 &
     &       (ele, surf, sf_grp, g_FEM, jac_sf_grp, igrp, k2, n_int,    &
     &        ak_d, surf_wk%xe_sf, surf_wk%vect_sf, fem_wk%sk6)
        end do
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_free_slip_surf_sph_in
!
! ----------------------------------------------------------------------
!
      end module int_free_slip_surf_sph
