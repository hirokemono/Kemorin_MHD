!
!      module int_surf_magne_pre
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine int_surf_magne_pre_ele(node, ele, surf, sf_grp)
!!      subroutine int_surf_magne_monitor                               &
!!     &         (node, ele, surf, sf_grp, i_field)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!
      module int_surf_magne_pre
!
      use m_precision
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
!
      use m_control_parameter
      use m_phys_constants
      use m_node_phys_data
      use m_element_id_4_node
      use m_jacobian_sf_grp
      use m_filter_elength
      use m_finite_element_matrix
      use m_surf_data_magne
!
      use int_surf_div_induct_tsr_sgs
      use int_surf_fixed_gradients
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_magne_pre_ele(node, ele, surf, sf_grp)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
!
!
      call int_sf_grad_velocity(node, ele, surf, sf_grp,                &
     &    jac1_sf_grp_2d_q, rhs_tbl1, sf_bc1_grad_b,                    &
     &    intg_point_t_evo, ak_d_magne, fem1_wk, f1_l)
!
       if (iflag_SGS_induction .ne. id_SGS_none                         &
     &     .and. iflag_commute_induction .eq. id_SGS_commute_ON) then
         call int_surf_div_induct_t_sgs(node, ele, surf, sf_grp,        &
     &       nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,           &
     &       sf_sgs1_grad_b, intg_point_t_evo, ifilter_final,           &
     &       iphys%i_SGS_induct_t, iphys%i_velo, iphys%i_magne,         &
     &       fem1_wk, f1_nl)
      end if
!
!      call int_free_slip_surf_sph_out(node, ele, surf, sf_grp,         &
!     &   nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, n_int,                  &
!     &   sf_bc1_pvc_out_a%ngrp_sf_dat, sf_bc1_pvc_out_a%id_grp_sf_dat, &
!     &   iphys%i_vecp, fem_wk, f_l)
!      call int_free_slip_surf_sph_in(node, ele, surf, sf_grp,          &
!     &   nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, n_int,                  &
!     &   sf_bc1_pvc_in_a%ngrp_sf_dat, sf_bc1_pvc_in_a%id_grp_sf_dat,   &
!     &   iphys%i_vecp, fem_wk, f_l)
!
      end subroutine int_surf_magne_pre_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_magne_monitor                                 &
     &         (node, ele, surf, sf_grp, i_field)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
!
      integer(kind= kint), intent(in) :: i_field
!
!
      if (i_field .eq. iphys%i_b_diffuse) then
        call int_sf_grad_velocity(node, ele, surf, sf_grp,              &
     &      jac1_sf_grp_2d_q, rhs_tbl1, sf_bc1_grad_b,                  &
     &      intg_point_t_evo, ak_d_magne, fem1_wk, f1_l)
      end if
!
      if (i_field .eq. iphys%i_SGS_induction) then
        if (iflag_SGS_induction .ne. id_SGS_none                        &
     &     .and. iflag_commute_induction .eq. id_SGS_commute_ON) then
          call int_surf_div_induct_t_sgs(node, ele, surf, sf_grp,       &
     &        nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,          &
     &        sf_sgs1_grad_b, intg_point_t_evo, ifilter_final,          &
     &        iphys%i_SGS_induct_t, iphys%i_velo, iphys%i_magne,        &
     &        fem1_wk, f1_nl)
        end if
      end if
!
!      call int_free_slip_surf_sph_out(node, ele, surf, sf_grp,         &
!     &   nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, n_int,                  &
!     &   sf_bc1_pvc_out_a%ngrp_sf_dat, sf_bc1_pvc_out_a%id_grp_sf_dat, &
!     &   iphys%i_vecp, fem_wk, f_l)
!      call int_free_slip_surf_sph_in(node, ele, surf, sf_grp,          &
!     &   nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, n_int,                  &
!     &   sf_bc1_pvc_in_a%ngrp_sf_dat, sf_bc1_pvc_in_a%id_grp_sf_dat,   &
!     &   iphys%i_vecp, fem_wk, f_l)
!
      end subroutine int_surf_magne_monitor
!
! ----------------------------------------------------------------------
!
      end module int_surf_magne_pre
