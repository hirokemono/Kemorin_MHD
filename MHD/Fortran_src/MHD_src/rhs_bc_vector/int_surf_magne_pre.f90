!
!      module int_surf_magne_pre
!
!     Written by H. Matsui on June, 2005
!
!      subroutine int_surf_magne_pre_ele
!      subroutine int_surf_magne_monitor(i_field)
!
      module int_surf_magne_pre
!
      use m_precision
!
      use m_control_parameter
      use m_geometry_data
      use m_group_data
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
      use m_sorted_node
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
      subroutine int_surf_magne_pre_ele
!
      integer(kind = kint)  :: num_int
!
!
      num_int = intg_point_t_evo
!
      call int_sf_grad_magne                                            &
     &   (node1, ele1, surf1, sf_grp1, jac1_sf_grp_2d_q, rhs_tbl1,      &
     &    num_int, fem1_wk, f1_l)
!
       if (iflag_SGS_induction .ne. id_SGS_none                         &
     &     .and. iflag_commute_induction .eq. id_SGS_commute_ON) then
         call int_surf_div_induct_t_sgs(node1, ele1, surf1, sf_grp1,    &
     &       nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,           &
     &       num_int, ifilter_final, iphys%i_SGS_induct_t,              &
     &       iphys%i_velo, iphys%i_magne, fem1_wk, f1_nl)
      end if
!
!      call int_free_slip_surf_sph_out(node1, ele1, surf1, sf_grp1,     &
!     &   nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, n_int,                  &
!     &   sf_bc1_pvc_out_a%ngrp_sf_dat, sf_bc1_pvc_out_a%id_grp_sf_dat, &
!     &   iphys%i_vecp, fem_wk, f_l)
!      call int_free_slip_surf_sph_in(node1, ele1, surf1, sf_grp1,      &
!     &   nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, n_int,                  &
!     &   sf_bc1_pvc_in_a%ngrp_sf_dat, sf_bc1_pvc_in_a%id_grp_sf_dat,   &
!     &   iphys%i_vecp, fem_wk, f_l)
!
      end subroutine int_surf_magne_pre_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_magne_monitor(i_field)
!
      integer(kind= kint), intent(in) :: i_field
      integer(kind = kint)  :: num_int
!
!
      num_int = intg_point_t_evo
!
      if (i_field .eq. iphys%i_b_diffuse) then
        if (sf_bc1_grad_b%nmax_sf_fix_fx .gt. 0) then
          call int_sf_grad_magne                                        &
     &       (node1, ele1, surf1, sf_grp1, jac1_sf_grp_2d_q, rhs_tbl1,  &
     &        num_int, fem1_wk, f1_l)
        end if
      end if
!
      if (i_field .eq. iphys%i_SGS_induction) then
        if (iflag_SGS_induction .ne. id_SGS_none                        &
     &     .and. iflag_commute_induction .eq. id_SGS_commute_ON) then
          call int_surf_div_induct_t_sgs(node1, ele1, surf1, sf_grp1,   &
     &        nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,          &
     &        num_int, ifilter_final, iphys%i_SGS_induct_t,             &
     &        iphys%i_velo, iphys%i_magne, fem1_wk, f1_nl)
        end if
      end if
!
!      call int_free_slip_surf_sph_out(node1, ele1, surf1, sf_grp1,     &
!     &   nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, n_int,                  &
!     &   sf_bc1_pvc_out_a%ngrp_sf_dat, sf_bc1_pvc_out_a%id_grp_sf_dat, &
!     &   iphys%i_vecp, fem_wk, f_l)
!      call int_free_slip_surf_sph_in(node1, ele1, surf1, sf_grp1,      &
!     &   nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, n_int,                  &
!     &   sf_bc1_pvc_in_a%ngrp_sf_dat, sf_bc1_pvc_in_a%id_grp_sf_dat,   &
!     &   iphys%i_vecp, fem_wk, f_l)
!
      end subroutine int_surf_magne_monitor
!
! ----------------------------------------------------------------------
!
      end module int_surf_magne_pre
