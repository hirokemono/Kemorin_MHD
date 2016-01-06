!> @file  copy_MHD_4_pole_trans.f90
!!      module copy_MHD_4_pole_trans
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2012
!
!> @brief copy spectr data for spherical transform at poles
!!
!!@verbatim
!!      subroutine copy_snap_vec_from_pole_trans(node, iphys, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module copy_MHD_4_pole_trans
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_snap_vec_from_pole_trans(node, iphys, nod_fld)
!
      use m_control_parameter
      use m_machine_parameter
      use m_spheric_parameter
      use m_addresses_trans_sph_snap
!
      use copy_pole_field_sph_trans
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_pole_vec_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_velo), nod_fld%ntot_phys,                  &
     &    iphys%i_velo, nod_fld%d_fld)
      call copy_pole_vec_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_vort), nod_fld%ntot_phys,                  &
     &    iphys%i_vort, nod_fld%d_fld)
      call copy_pole_vec_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_magne), nod_fld%ntot_phys,                 &
     &    iphys%i_magne, nod_fld%d_fld)
      call copy_pole_vec_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_current), nod_fld%ntot_phys,               &
     &    iphys%i_current, nod_fld%d_fld)
!
      call copy_pole_vec_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_v_diffuse), nod_fld%ntot_phys,             &
     &    iphys%i_v_diffuse, nod_fld%d_fld)
      call copy_pole_vec_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_w_diffuse), nod_fld%ntot_phys,             &
     &    iphys%i_w_diffuse, nod_fld%d_fld)
      call copy_pole_vec_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_vp_diffuse), nod_fld%ntot_phys,            &
     &    iphys%i_vp_diffuse, nod_fld%d_fld)
      call copy_pole_vec_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_b_diffuse), nod_fld%ntot_phys,             &
     &    iphys%i_b_diffuse, nod_fld%d_fld)
!
      call copy_pole_vec_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_induction), nod_fld%ntot_phys,             &
     &    iphys%i_induction, nod_fld%d_fld)
!
      call copy_pole_vec_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_grad_t), nod_fld%ntot_phys,                &
     &    iphys%i_grad_t, nod_fld%d_fld)
      call copy_pole_vec_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_grad_composit),                            &
     &    nod_fld%ntot_phys, iphys%i_grad_composit, nod_fld%d_fld)
!
!
!
      call copy_pole_scl_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_temp), nod_fld%ntot_phys,                  &
     &    iphys%i_temp, nod_fld%d_fld)
      call copy_pole_scl_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_light), nod_fld%ntot_phys,                 &
     &    iphys%i_light, nod_fld%d_fld)
!
      call copy_pole_scl_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_press), nod_fld%ntot_phys,                 &
     &    iphys%i_press, nod_fld%d_fld)
      call copy_pole_scl_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_par_temp), nod_fld%ntot_phys,              &
     &    iphys%i_par_temp, nod_fld%d_fld)
      call copy_pole_scl_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_t_diffuse), nod_fld%ntot_phys,             &
     &    iphys%i_t_diffuse, nod_fld%d_fld)
      call copy_pole_scl_fld_from_trans                                 &
     &   (node%numnod, node%internal_node, node%xx,                     &
     &    fls_pl(1,bs_trns%i_c_diffuse), nod_fld%ntot_phys,             &
     &    iphys%i_c_diffuse, nod_fld%d_fld)
!
      end subroutine copy_snap_vec_from_pole_trans
!
! -----------------------------------------------------------------------
!
      end module copy_MHD_4_pole_trans
