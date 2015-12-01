!
!      module int_surf_diffuse_terms
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine int_surf_current_diffuse(node, ele, surf, sf_grp,    &
!!     &          nod_fld, jac_sf_grp, rhs_tbl, lead_sf,                &
!!     &          n_int, i_vecp, fem_wk, f_l)
!!      subroutine int_surf_diffuse_term(node, ele, surf, sf_grp,       &
!!     &          nod_fld, jac_sf_grp, rhs_tbl, lead_sf,                &
!!     &          n_int, ak_d, i_field, fem_wk, f_l)
!!      subroutine int_surf_vect_diffuse_term(node, ele, surf, sf_grp,  &
!!     &          jac_sf_grp, nod_fld, rhs_tbl, lead_sf                 &
!!     &          n_int, ak_d, i_field, fem_wk, f_l)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(scaler_surf_bc_data_type),  intent(in) :: lead_sf
!!        type(scaler_surf_bc_data_type),  intent(in) :: lead_sf(3)
!
!
      module int_surf_diffuse_terms
!
      use m_precision
      use m_control_parameter
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
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
      subroutine int_surf_current_diffuse(node, ele, surf, sf_grp,      &
     &          nod_fld, jac_sf_grp, rhs_tbl, lead_sf,                  &
     &          n_int, i_vecp, fem_wk, f_l)
!
      use m_ele_material_property
      use m_int_surface_data
!
      use delta_phys_2_each_surface
      use fem_surf_skv_poisson_type
      use cal_skv_to_ff_smp
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(scaler_surf_bc_data_type),  intent(in) :: lead_sf(3)
!
      integer(kind=kint), intent(in) :: n_int, i_vecp
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind=kint) :: k2, nd, i_comp, i, igrp, num
!
!
      num =  lead_sf(1)%ngrp_sf_dat                                     &
     &     + lead_sf(2)%ngrp_sf_dat                                     &
     &     + lead_sf(3)%ngrp_sf_dat
      if(num .le. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do nd = 1, 3
        i_comp = i_vecp + nd - 1
!
        do i = 1, lead_sf(nd)%ngrp_sf_dat
          igrp = lead_sf(nd)%id_grp_sf_dat(i)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
          if (num .gt. 0) then
!
            do k2 = 1, surf%nnod_4_surf
              call dlt_scl_phys_2_each_surface                          &
     &           (node, ele, surf, sf_grp, nod_fld, igrp, k2,           &
     &            i_comp, scalar_sf)
              call fem_surf_skv_current_by_vecp                         &
     &           (ele, surf, sf_grp, jac_sf_grp, igrp, k2, nd,          &
     &            n_int, dxe_sf, scalar_sf, fem_wk%sk6)
            end do
!
          end if
        end do
!
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_surf_current_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_diffuse_term(node, ele, surf, sf_grp,         &
     &          nod_fld, jac_sf_grp, rhs_tbl, lead_sf,                  &
     &          n_int, ak_d, i_field, fem_wk, f_l)
!
      use m_int_surface_data
!
      use delta_phys_2_each_surface
      use fem_surf_skv_poisson_type
      use cal_skv_to_ff_smp
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(scaler_surf_bc_data_type),  intent(in) :: lead_sf
!
      integer(kind = kint), intent(in) :: n_int, i_field
      real (kind = kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
      if (lead_sf%ngrp_sf_dat .eq. 0) return
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      do i = 1, lead_sf%ngrp_sf_dat
        igrp = lead_sf%id_grp_sf_dat(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if (num .gt. 0) then
!
          do k2 = 1, surf%nnod_4_surf
            call dlt_scl_phys_2_each_surface                            &
     &         (node, ele, surf, sf_grp, nod_fld, igrp, k2,             &
     &          i_field, scalar_sf)
            call fem_surf_skv_diffuse_galerkin                          &
     &         (ele, surf, sf_grp, jac_sf_grp, igrp, k2,                &
     &          ione, n_int, dxe_sf, scalar_sf, ak_d, fem_wk%sk6)
          end do
        end if
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &    (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_surf_diffuse_term
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_vect_diffuse_term(node, ele, surf, sf_grp,    &
     &          jac_sf_grp, nod_fld, rhs_tbl, lead_sf,                  &
     &          n_int, ak_d, i_field, fem_wk, f_l)
!
      use m_int_surface_data
!
      use delta_phys_2_each_surface
      use fem_surf_skv_poisson_type
      use cal_skv_to_ff_smp
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(scaler_surf_bc_data_type),  intent(in) :: lead_sf(3)
!
      integer(kind = kint), intent(in) :: n_int, i_field
      real (kind = kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind=kint) :: k2, i, igrp, nd, i_comp, num
!
!
      num =  lead_sf(1)%ngrp_sf_dat                                     &
     &     + lead_sf(2)%ngrp_sf_dat                                     &
     &     + lead_sf(3)%ngrp_sf_dat
      if(num .le. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do nd = 1, n_vector
        i_comp = i_field + nd - 1
!
        do i = 1, lead_sf(nd)%ngrp_sf_dat
          igrp = lead_sf(nd)%id_grp_sf_dat(i)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
          if (num .gt. 0) then
!
            do k2 = 1, surf%nnod_4_surf
              call dlt_scl_phys_2_each_surface                          &
     &           (node, ele, surf, sf_grp, nod_fld, igrp, k2,           &
     &            i_comp, scalar_sf)
              call fem_surf_skv_diffuse_galerkin                        &
     &           (ele, surf, sf_grp, jac_sf_grp, igrp, k2,              &
     &            nd, n_int, dxe_sf, scalar_sf, ak_d, fem_wk%sk6)
            end do
!
          end if
        end do
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_surf_vect_diffuse_term
!
!-----------------------------------------------------------------------
!
      end module int_surf_diffuse_terms
