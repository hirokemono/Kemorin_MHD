!> @file  copy_MHD_4_pole_trans.f90
!!      module copy_MHD_4_pole_trans
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2012
!
!> @brief copy spectr data for spherical transform at poles
!!
!!@verbatim
!!      subroutine copy_snap_vec_from_pole_trans
!!@endverbatim
!
      module copy_MHD_4_pole_trans
!
      use m_precision
      use m_constants
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_snap_vec_from_pole_trans
!
      use m_control_parameter
      use m_machine_parameter
      use m_spheric_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_node_phys_address
      use m_addresses_trans_sph_snap
!
      use copy_pole_field_sph_trans
!
!
      call copy_pole_vec_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_velo), nod_fld1%ntot_phys,                 &
     &    iphys%i_velo, d_nod)
      call copy_pole_vec_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_vort), nod_fld1%ntot_phys,                 &
     &    iphys%i_vort, d_nod)
      call copy_pole_vec_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_magne), nod_fld1%ntot_phys,                &
     &    iphys%i_magne, d_nod)
      call copy_pole_vec_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_current), nod_fld1%ntot_phys,              &
     &    iphys%i_current, d_nod)
!
      call copy_pole_vec_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_v_diffuse), nod_fld1%ntot_phys,            &
     &    iphys%i_v_diffuse, d_nod)
      call copy_pole_vec_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_w_diffuse), nod_fld1%ntot_phys,            &
     &    iphys%i_w_diffuse, d_nod)
      call copy_pole_vec_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_vp_diffuse), nod_fld1%ntot_phys,           &
     &    iphys%i_vp_diffuse, d_nod)
      call copy_pole_vec_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_b_diffuse), nod_fld1%ntot_phys,            &
     &    iphys%i_b_diffuse, d_nod)
!
      call copy_pole_vec_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_induction), nod_fld1%ntot_phys,            &
     &    iphys%i_induction, d_nod)
!
      call copy_pole_vec_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_grad_t), nod_fld1%ntot_phys,               &
     &    iphys%i_grad_t, d_nod)
      call copy_pole_vec_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_grad_composit),                            &
     &    nod_fld1%ntot_phys, iphys%i_grad_composit,  d_nod)
!
!
!
      call copy_pole_scl_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_temp), nod_fld1%ntot_phys,                 &
     &    iphys%i_temp, d_nod)
      call copy_pole_scl_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_light), nod_fld1%ntot_phys,                &
     &    iphys%i_light, d_nod)
!
      call copy_pole_scl_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_press), nod_fld1%ntot_phys,                &
     &    iphys%i_press, d_nod)
      call copy_pole_scl_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_par_temp), nod_fld1%ntot_phys,             &
     &    iphys%i_par_temp, d_nod)
      call copy_pole_scl_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_t_diffuse), nod_fld1%ntot_phys,            &
     &    iphys%i_t_diffuse, d_nod)
      call copy_pole_scl_fld_from_trans                                 &
     &   (node1%numnod, node1%internal_node, node1%xx,                  &
     &    fls_pl(1,bs_trns%i_c_diffuse), nod_fld1%ntot_phys,            &
     &    iphys%i_c_diffuse, d_nod)
!
      end subroutine copy_snap_vec_from_pole_trans
!
! -----------------------------------------------------------------------
!
      end module copy_MHD_4_pole_trans
