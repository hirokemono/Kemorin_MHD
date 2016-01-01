!
!      module int_surf_div_fluxes_sgs
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine int_sf_skv_sgs_div_v_flux(node, ele, surf, sf_grp,   &
!!     &          nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,              &
!!     &          n_int, ngrp_sf, id_grp_sf, i_filter,                  &
!!     &          i_tensor, i_vect, i_scalar, ak_diff, coef_field,      &
!!     &          fem_wk, f_nl)
!!      subroutine int_sf_skv_sgs_div_t_flux(node, ele, surf, sf_grp,   &
!!     &          nod_fld, jac_sf_grp, rhs_tbl, FEM_elens, sgs_sf,      &
!!     &          n_int, i_filter, i_tensor, i_vect, i_scalar,          &
!!     &          ak_diff, coef_field, fem_wk, f_nl)
!!
!!      subroutine int_sf_skv_commute_sgs_v_flux(node, ele, surf,       &
!!     &          sf_grp, nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,      &
!!     &          n_int, sgs_sf, i_filter, i_tensor, i_vect, i_scalar,  &
!!     &          fem_wk, f_nl)
!!      subroutine int_sf_skv_commute_sgs_t_flux(node, ele, surf,       &
!!     &        sf_grp, nod_fld, jac_sf_grp, rhs_tbl, FEM_elens, sgs_sf,&
!!     &        n_int, i_filter, i_tensor, i_vect, i_scalar,            &
!!     &        fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!!
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module int_surf_div_fluxes_sgs
!
      use m_precision
!
      use m_physical_property
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
      use t_surface_bc_data
      use m_int_surface_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_skv_sgs_div_v_flux(node, ele, surf, sf_grp,     &
     &          nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,                &
     &          n_int, ngrp_sf, id_grp_sf, i_filter,                    &
     &          i_tensor, i_vect, i_scalar, ak_diff, coef_field,        &
     &          fem_wk, f_nl)
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
      integer(kind=kint), intent(in) :: ngrp_sf
      integer(kind=kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer(kind=kint), intent(in) :: i_vect, i_scalar, i_tensor
      real (kind = kreal), intent(in) :: ak_diff(ele%numele)
      real (kind = kreal), intent(in) :: coef_field
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
      if (ngrp_sf .eq. 0) return
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! --------- set vector at each node in an element
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
!
        if (num .gt. 0) then
          do k2 = 1, surf%nnod_4_surf
            call d_SGS_flux_2_each_sf_w_cst                             &
     &         (node, ele, surf, sf_grp, nod_fld, igrp, k2, ione,       &
     &          i_vect, i_scalar, i_tensor, dminus, vect_sf)
            call fem_sf_grp_skv_sgs_div_flux_p                          &
     &         (ele, surf, sf_grp, jac_sf_grp, FEM_elens,               &
     &          igrp, k2, ione, n_int, i_filter, dxe_sf, vect_sf,       &
     &          ak_diff, coef_field, fem_wk%sk6)
          end do
!
        end if
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sf_skv_sgs_div_v_flux
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_skv_sgs_div_t_flux(node, ele, surf, sf_grp,     &
     &          nod_fld, jac_sf_grp, rhs_tbl, FEM_elens, sgs_sf,        &
     &          n_int, i_filter, i_tensor, i_vect, i_scalar,            &
     &          ak_diff, coef_field, fem_wk, f_nl)
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
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer(kind=kint), intent(in) :: i_vect, i_scalar, i_tensor
      real (kind = kreal), intent(in) :: ak_diff(ele%numele)
      real (kind = kreal), intent(in) :: coef_field
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
      integer(kind=kint) :: k2, i, igrp, nd, num
!
!
      num =  sgs_sf(1)%ngrp_sf_dat                                      &
     &     + sgs_sf(2)%ngrp_sf_dat                                      &
     &     + sgs_sf(3)%ngrp_sf_dat
      if(num .le. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! --------- set vector at each node in an element
!
      do nd = 1, n_vector
        do i = 1, sgs_sf(nd)%ngrp_sf_dat
          igrp = sgs_sf(nd)%id_grp_sf_dat(i)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
!
          if (num .gt. 0) then
            do k2 = 1, surf%nnod_4_surf
              call d_SGS_flux_2_each_sf_w_cst                           &
     &           (node, ele, surf, sf_grp, nod_fld, igrp, k2, nd,       &
     &            i_vect, i_scalar, i_tensor, dminus, vect_sf)
              call fem_sf_grp_skv_sgs_div_flux_p                        &
     &           (ele, surf, sf_grp, jac_sf_grp, FEM_elens,             &
     &            igrp, k2, nd, n_int, i_filter, dxe_sf, vect_sf,       &
     &            ak_diff, coef_field, fem_wk%sk6)
            end do
!
          end if
        end do
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sf_skv_sgs_div_t_flux
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_skv_commute_sgs_v_flux(node, ele, surf,         &
     &          sf_grp, nod_fld, jac_sf_grp, rhs_tbl, FEM_elens,        &
     &          n_int, sgs_sf, i_filter, i_tensor, i_vect, i_scalar,    &
     &          fem_wk, f_nl)
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
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer(kind=kint), intent(in) :: i_vect, i_scalar, i_tensor
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
      if (sgs_sf%ngrp_sf_dat .eq. 0) return
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! --------- set vector at each node in an element
!
      do i = 1, sgs_sf%ngrp_sf_dat
        igrp = sgs_sf%id_grp_sf_dat(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
!
        if (num .gt. 0) then
          do k2 = 1, surf%nnod_4_surf
            call d_SGS_flux_2_each_surface                              &
     &         (node, ele, surf, sf_grp, nod_fld, igrp, k2, ione,       &
     &          i_vect, i_scalar, i_tensor, vect_sf)
            call fem_sf_grp_skv_div_f_commute_p                         &
     &         (ele, surf, sf_grp, jac_sf_grp, FEM_elens,               &
     &          igrp, k2, ione, n_int, i_filter, dxe_sf, vect_sf,       &
     &          fem_wk%sk6)
          end do
!
        end if
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele , rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sf_skv_commute_sgs_v_flux
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_skv_commute_sgs_t_flux(node, ele, surf,         &
     &        sf_grp, nod_fld, jac_sf_grp, rhs_tbl, FEM_elens, sgs_sf,  &
     &        n_int, i_filter, i_tensor, i_vect, i_scalar,              &
     &        fem_wk, f_nl)
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
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer(kind=kint), intent(in) :: i_vect, i_scalar, i_tensor
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2, i, igrp, nd, num
!
!
      num =  sgs_sf(1)%ngrp_sf_dat                                      &
     &     + sgs_sf(2)%ngrp_sf_dat                                      &
     &     + sgs_sf(3)%ngrp_sf_dat
      if(num .le. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! --------- set vector at each node in an element
!
      do nd = 1, n_vector
        do i = 1, sgs_sf(nd)%ngrp_sf_dat
          igrp = sgs_sf(nd)%id_grp_sf_dat(i)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
!
          if (num .gt. 0) then
            do k2 = 1, surf%nnod_4_surf
              call d_SGS_flux_2_each_surface                            &
     &           (node, ele, surf, sf_grp, nod_fld, igrp, k2,           &
     &            nd, i_vect, i_scalar, i_tensor, vect_sf)
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
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sf_skv_commute_sgs_t_flux
!
!-----------------------------------------------------------------------
!
      end module int_surf_div_fluxes_sgs
