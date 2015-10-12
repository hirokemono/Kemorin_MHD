!
!      module int_surf_diffuse_terms
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine int_surf_temp_diffuse(ele, surf, sf_grp, jac_sf_grp)
!      subroutine int_surf_velo_diffuse(ele, surf, sf_grp, jac_sf_grp)
!      subroutine int_surf_vector_p_diffuse                             &
!     &         (ele, surf, sf_grp, jac_sf_grp)
!      subroutine int_surf_magne_diffuse(ele, surf, sf_grp, jac_sf_grp)
!      subroutine int_surf_composit_diffuse                             &
!     &         (ele, surf, sf_grp, jac_sf_grp)
!      subroutine int_surf_current_diffuse                              &
!     &         (ele, surf, sf_grp, jac_sf_grp)
!
      module int_surf_diffuse_terms
!
      use m_precision
      use m_control_parameter
      use m_node_phys_address
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobian_2d
!
      implicit none
!
      private :: int_surf_diffuse_term, int_surf_vect_diffuse_term
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_temp_diffuse(ele, surf, sf_grp, jac_sf_grp)
!
      use m_ele_material_property
      use m_surf_data_temp
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
!
      call int_surf_diffuse_term(ele, surf, sf_grp, jac_sf_grp,         &
     &    intg_point_t_evo, ngrp_sf_lead_hf, id_grp_sf_lead_hf,         &
     &    ak_d_temp, iphys%i_temp)
!
      end subroutine int_surf_temp_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_velo_diffuse(ele, surf, sf_grp, jac_sf_grp)
!
      use m_ele_material_property
      use m_surf_data_torque
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
!
      call int_surf_vect_diffuse_term(ele, surf, sf_grp, jac_sf_grp,    &
     &    intg_point_t_evo, nmax_sf_lead_tq, ngrp_sf_lead_tq,           &
     &    id_grp_sf_lead_tq, ak_d_velo, iphys%i_velo)
!
      end subroutine int_surf_velo_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_vector_p_diffuse                              &
     &         (ele, surf, sf_grp, jac_sf_grp)
!
      use m_ele_material_property
      use m_surf_data_vector_p
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
!
      call int_surf_vect_diffuse_term(ele, surf, sf_grp, jac_sf_grp,    &
     &    intg_point_t_evo, nmax_sf_lead_vect_p, ngrp_sf_lead_vect_p,   &
     &    id_grp_sf_lead_vect_p, ak_d_magne, iphys%i_vecp)
!
      end subroutine int_surf_vector_p_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_magne_diffuse(ele, surf, sf_grp, jac_sf_grp)
!
      use m_ele_material_property
      use m_surf_data_magne
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
!
      call int_surf_vect_diffuse_term(ele, surf, sf_grp, jac_sf_grp,    &
     &    intg_point_t_evo,  nmax_sf_lead_b, ngrp_sf_lead_b,            &
     &    id_grp_sf_lead_b, ak_d_magne, iphys%i_magne)
!
      end subroutine int_surf_magne_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_composit_diffuse                              &
     &         (ele, surf, sf_grp, jac_sf_grp)
!
      use m_ele_material_property
      use m_surf_data_composition
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
!
      call int_surf_diffuse_term(ele, surf, sf_grp, jac_sf_grp,         &
     &    intg_point_t_evo, ngrp_sf_lead_cmg, id_grp_sf_lead_cmg,       &
     &    ak_d_composit, iphys%i_light)
!
      end subroutine int_surf_composit_diffuse
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_surf_current_diffuse                               &
     &         (ele, surf, sf_grp, jac_sf_grp)
!
      use m_sorted_node
      use m_finite_element_matrix
      use m_surf_data_vector_p
      use m_ele_material_property
      use m_int_surface_data
!
      use delta_phys_2_each_surface
      use fem_surf_skv_poisson_type
      use cal_skv_to_ff_smp
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
      integer(kind=kint) :: k2, nd, i_comp, i, igrp, num
!
!
      if(nmax_sf_lead_vect_p .eq. 0) return
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
      do nd = 1, 3
        i_comp = iphys%i_vecp + nd - 1
!
        do i = 1, ngrp_sf_lead_vect_p(nd)
          igrp = id_grp_sf_lead_vect_p(i,nd)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
          if (num .gt. 0) then
!
            do k2 = 1, surf%nnod_4_surf
              call dlt_scl_phys_2_each_surface(sf_grp, igrp, k2,        &
     &            i_comp, scalar_sf)
              call fem_surf_skv_current_by_vecp                         &
     &           (ele, surf, sf_grp, jac_sf_grp, igrp, k2, nd,          &
     &            intg_point_t_evo, dxe_sf, scalar_sf, fem1_wk%sk6)
            end do
!
          end if
        end do
!
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_smp)
!
      end subroutine int_surf_current_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_diffuse_term(ele, surf, sf_grp, jac_sf_grp,   &
     &          n_int, ngrp_sf, id_grp_sf, ak_d, i_field)
!
      use m_sorted_node
      use m_finite_element_matrix
      use m_int_surface_data
!
      use delta_phys_2_each_surface
      use fem_surf_skv_poisson_type
      use cal_skv_to_ff_smp
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
      integer(kind = kint), intent(in) :: ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: n_int, i_field
      real (kind = kreal), intent(in) :: ak_d(ele%numele)
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
      if (ngrp_sf .eq. 0) return
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if (num .gt. 0) then
!
          do k2 = 1, surf%nnod_4_surf
            call dlt_scl_phys_2_each_surface(sf_grp, igrp, k2,          &
     &          i_field, scalar_sf)
            call fem_surf_skv_diffuse_galerkin                          &
     &         (ele, surf, sf_grp, jac_sf_grp, igrp, k2,                &
     &          ione, n_int, dxe_sf, scalar_sf, ak_d, fem1_wk%sk6)
          end do
        end if
      end do
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_smp)
!
      end subroutine int_surf_diffuse_term
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_vect_diffuse_term                             &
     &         (ele, surf, sf_grp, jac_sf_grp,                          &
     &          n_int, nmax_sf, ngrp_sf, id_grp_sf, ak_d,  i_field)
!
      use m_sorted_node
      use m_finite_element_matrix
      use m_int_surface_data
!
      use delta_phys_2_each_surface
      use fem_surf_skv_poisson_type
      use cal_skv_to_ff_smp
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
      integer(kind=kint), intent(in) :: nmax_sf
      integer(kind=kint), intent(in) :: ngrp_sf(3)
      integer(kind=kint), intent(in) :: id_grp_sf(nmax_sf,3)
!
      integer(kind = kint), intent(in) :: n_int, i_field
      real (kind = kreal), intent(in) :: ak_d(ele%numele)
!
      integer(kind=kint) :: k2, i, igrp, nd, i_comp, num
!
!
      if (nmax_sf .eq. 0) return
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
      do nd = 1, n_vector
        i_comp = i_field + nd - 1
!
        do i = 1, ngrp_sf(nd)
          igrp = id_grp_sf(i,nd)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
          if (num .gt. 0) then
!
            do k2 = 1, surf%nnod_4_surf
              call dlt_scl_phys_2_each_surface(sf_grp, igrp, k2,        &
     &            i_comp, scalar_sf)
              call fem_surf_skv_diffuse_galerkin                        &
     &           (ele, surf, sf_grp, jac_sf_grp, igrp, k2,              &
     &            nd, n_int, dxe_sf, scalar_sf, ak_d, fem1_wk%sk6)
            end do
!
          end if
        end do
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_smp)
!
      end subroutine int_surf_vect_diffuse_term
!
!-----------------------------------------------------------------------
!
      end module int_surf_diffuse_terms
