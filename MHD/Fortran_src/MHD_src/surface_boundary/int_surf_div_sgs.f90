!
!      module int_surf_div_sgs
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine int_surf_sgs_div_velo_ele                            &
!!     &          (node, ele, surf, sf_grp, nod_fld,                    &
!!     &           jac_sf_grp_q, jac_sf_grp_l, rhs_tbl, FEM_elens,      &
!!     &           n_int, nmax_grp_sf, ngrp_sf, id_grp_sf, i_filter,    &
!!     &           ncomp_diff, iak_diff, ak_diff, i_vect, fem_wk, f_l)
!!
!!      subroutine int_surf_divergence_sgs(node, ele, surf, sf_grp,     &
!!     &          nod_fld, jac_sf_grp_q, rhs_tbl, FEM_elens,            &
!!     &          n_int, nmax_grp_sf, ngrp_sf, id_grp_sf, i_filter,     &
!!     &          ncomp_diff, iak_diff, ak_diff, i_vect, fem_wk, f_nl)
!!      subroutine int_surf_div_commute_sgs(node, ele, surf, sf_grp,    &
!!     &          nod_fld, jac_sf_grp_q, rhs_tbl, FEM_elens,            &
!!     &          n_int, nmax_grp_sf, ngrp_sf, id_grp_sf, i_filter,     &
!!     &          i_vect, fem_wk, f_nl)
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
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module int_surf_div_sgs
!
      use m_precision
      use m_constants
      use m_phys_constants
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
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
      subroutine int_surf_sgs_div_velo_ele                              &
     &          (node, ele, surf, sf_grp, nod_fld,                      &
     &           jac_sf_grp_q, jac_sf_grp_l, rhs_tbl, FEM_elens,        &
     &           n_int, nmax_grp_sf, ngrp_sf, id_grp_sf, i_filter,      &
     &           ncomp_diff, iak_diff, ak_diff, i_vect, fem_wk, f_l)
!
      use m_int_surface_data
!
      use delta_phys_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp
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
      integer(kind = kint), intent(in) :: n_int, nmax_grp_sf
      integer(kind = kint), intent(in) :: ngrp_sf(3)
      integer(kind = kint), intent(in) :: id_grp_sf(nmax_grp_sf,3)
      integer(kind = kint), intent(in) :: i_vect, i_filter
      integer(kind = kint), intent(in) :: ncomp_diff, iak_diff
      real(kind = kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
       integer(kind=kint) :: k2, nd, i, igrp, i_comp, num
!
!
      if(nmax_grp_sf .eq. 0) return
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! --------- set vector at each node in an element
!
      do nd = 1, n_vector
        i_comp = i_vect + nd - 1
        do i = 1, ngrp_sf(nd)
          igrp = id_grp_sf(i,nd)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
          if(num .gt. 0) then
!
            do k2 = 1, surf%nnod_4_surf
              call dlt_scl_phys_2_each_surface                          &
     &           (node, ele, surf, sf_grp, nod_fld, igrp, k2,           &
     &            i_comp, scalar_sf)
              call fem_sf_grp_skv_sgs_div_lin_p(ele, surf, sf_grp,      &
     &            jac_sf_grp_q, jac_sf_grp_l, FEM_elens,                &
     &            igrp, k2, nd, n_int, i_filter, dxe_sf, scalar_sf,     &
     &            ak_diff(1,iak_diff), fem_wk%sk6)
            end do
!
          end if
        end do
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_surf_sgs_div_velo_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_surf_divergence_sgs(node, ele, surf, sf_grp,       &
     &          nod_fld, jac_sf_grp_q, rhs_tbl, FEM_elens,              &
     &          n_int, nmax_grp_sf, ngrp_sf, id_grp_sf, i_filter,       &
     &          ncomp_diff, iak_diff, ak_diff, i_vect, fem_wk, f_nl)
!
      use m_int_surface_data
!
      use delta_phys_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp
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
      integer(kind = kint), intent(in) :: n_int, nmax_grp_sf
      integer(kind = kint), intent(in) :: ngrp_sf(3)
      integer(kind = kint), intent(in) :: id_grp_sf(nmax_grp_sf,3)
      integer(kind = kint), intent(in) :: i_vect, iak_diff, i_filter
      integer(kind = kint), intent(in) :: ncomp_diff, iak_diff
      real (kind = kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2, nd, i, igrp, i_comp, num
!
!
      if(nmax_grp_sf .eq. 0) return
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! --------- set vector at each node in an element
!
      do nd = 1, n_vector
        i_comp = i_vect + nd - 1
        do i = 1, ngrp_sf(nd)
          igrp = id_grp_sf(i,nd)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
          if (num .gt. 0) then
!
            do k2 = 1, surf%nnod_4_surf
              call dlt_scl_phys_2_each_surface                          &
     &           (node, ele, surf, sf_grp, nod_fld, igrp, k2,           &
     &            i_comp, scalar_sf)
              call fem_sf_grp_skv_sgs_vect_diff_p                       &
     &           (ele, surf, sf_grp, jac_sf_grp_q, FEM_elens,           &
     &            igrp, k2, ione, n_int, i_filter, nd,                  &
     &            dxe_sf, scalar_sf, ak_diff(1,iak_diff), one,          &
     &            fem_wk%sk6)
            end do
          end if
        end do
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_surf_divergence_sgs
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_div_commute_sgs(node, ele, surf, sf_grp,      &
     &          nod_fld, jac_sf_grp_q, rhs_tbl, FEM_elens,              &
     &          n_int, nmax_grp_sf, ngrp_sf, id_grp_sf, i_filter,       &
     &          i_vect, fem_wk, f_nl)
!
      use m_int_surface_data
!
      use delta_phys_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp
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
       integer(kind = kint), intent(in) :: n_int, nmax_grp_sf
       integer(kind = kint), intent(in) :: ngrp_sf(3)
       integer(kind = kint), intent(in) :: id_grp_sf(nmax_grp_sf,3)
       integer(kind = kint), intent(in) :: i_vect, i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
       integer(kind=kint) :: k2, nd, i, igrp, i_comp, num
!
!
! -------- loop for shape function for the phsical values
!
      if(nmax_grp_sf .eq. 0) return
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      do nd = 1, n_vector
        i_comp = i_vect + nd - 1
        do i = 1, ngrp_sf(nd)
          igrp = id_grp_sf(i,nd)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
          if (num .gt. 0) then
!
            do k2 = 1, surf%nnod_4_surf
              call dlt_scl_phys_2_each_surface                          &
     &           (node, ele, surf, sf_grp, nod_fld, igrp, k2,           &
     &            i_comp, scalar_sf)
              call fem_sf_grp_skv_commute_err_p                         &
     &           (ele, surf, sf_grp, jac_sf_grp_q, FEM_elens,           &
     &            igrp, k2, ione, n_int, i_filter, nd,                  &
     &            dxe_sf, scalar_sf, fem_wk%sk6)
            end do
!
          end if
        end do
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)

!
      end subroutine int_surf_div_commute_sgs
!
!-----------------------------------------------------------------------
!
      end module int_surf_div_sgs
