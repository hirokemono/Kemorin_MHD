!
!      module int_surf_poisson_walls
!
!      Written by H. Matsui on Sep. 2005
!
!!      subroutine int_surf_poisson_wall(node, ele, surf, sf_grp,       &
!!     &          nod_fld, jac_sf_grp_l, rhs_tbl,                       &
!!     &          n_int, ngrp_sf, id_grp_sf, i_vect,                    &
!!     &          fem_wk, surf_wk, f_l)
!!      subroutine int_surf_poisson_sph_in(node, ele, surf, sf_grp,     &
!!     &          nod_fld, jac_sf_grp_l, rhs_tbl,                       &
!!     &          n_int, ngrp_sf, id_grp_sf, i_vect,                    &
!!     &          fem_wk, surf_wk, f_l)
!!      subroutine int_surf_poisson_sph_out(node, ele, surf, sf_grp,    &
!!     &           nod_fld, jac_sf_grp_l, rhs_tbl,                      &
!!     &           n_int, ngrp_sf, id_grp_sf, i_vect,                   &
!!     &           fem_wk, surf_wk, f_l)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!
      module int_surf_poisson_walls
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
!
      use node_phys_2_each_surface
      use fem_surf_skv_poisson_type
      use cal_skv_to_ff_smp
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_poisson_wall(node, ele, surf, sf_grp,         &
     &          nod_fld, jac_sf_grp_l, rhs_tbl,                         &
     &          n_int, ngrp_sf, id_grp_sf, i_vect,                      &
     &          fem_wk, surf_wk, f_l)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: n_int, ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_vect
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! --------- set vector at each node in an element
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if (num .gt.0 ) then
!
          do k2 = 1, num_linear_sf
            call vector_phys_2_each_surface                             &
     &         (node, ele, surf, sf_grp, nod_fld, igrp, k2,             &
     &          i_vect, surf_wk%vect_sf)
            call fem_surf_skv_poisson_wall                              &
     &         (ele, surf, sf_grp, jac_sf_grp_l, igrp, k2, n_int,       &
     &          surf_wk%vect_sf, fem_wk%sk6)
          end do
!
        end if
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_surf_poisson_wall
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_poisson_sph_in(node, ele, surf, sf_grp,       &
     &          nod_fld, jac_sf_grp_l, rhs_tbl,                         &
     &          n_int, ngrp_sf, id_grp_sf, i_vect,                      &
     &          fem_wk, surf_wk, f_l)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: n_int, ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_vect
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if (num .gt.0 ) then
!
! -------- loop for shape function for the phsical values
          do k2 = 1, num_linear_sf
            call vector_phys_2_each_surf_cst                            &
     &         (node, ele, surf, sf_grp, nod_fld, igrp, k2,             &
     &          i_vect, dminus, surf_wk%vect_sf)
            call fem_surf_skv_poisson_sph_out                           &
     &         (ele, surf, sf_grp, jac_sf_grp_l, igrp, k2,              &
     &          n_int, surf_wk%xe_sf, surf_wk%vect_sf, fem_wk%sk6)
          end do
!
        end if
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_surf_poisson_sph_in
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_poisson_sph_out(node, ele, surf, sf_grp,      &
     &           nod_fld, jac_sf_grp_l, rhs_tbl,                        &
     &           n_int, ngrp_sf, id_grp_sf, i_vect,                     &
     &           fem_wk, surf_wk, f_l)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: n_int, ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_vect
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! --------- set vector at each node in an element
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if (num .gt. 0) then
!
! -------- loop for shape function for the phsical values
          do k2 = 1, num_linear_sf
            call vector_phys_2_each_surface                             &
     &         (node, ele, surf, sf_grp, nod_fld, igrp, k2,             &
     &          i_vect, surf_wk%vect_sf)
            call fem_surf_skv_poisson_sph_out                           &
     &         (ele, surf, sf_grp, jac_sf_grp_l, igrp, k2, n_int,       &
     &          surf_wk%xe_sf, surf_wk%vect_sf, fem_wk%sk6)
          end do
!
        end if
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_surf_poisson_sph_out
!
!-----------------------------------------------------------------------
!
      end module int_surf_poisson_walls
