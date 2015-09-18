!commute_error_rotation.f90
!     module commute_error_rotation
!
!     Written by H. Matsui
!
!      subroutine cal_rotation_commute(nmax_grp_sf, ngrp_sf,            &
!     &          id_grp_sf, i_filter, i_sgs, i_vect)
!      subroutine cal_rotation_commute_fluid(nmax_grp_sf, ngrp_sf,      &
!     &          id_grp_sf, i_filter, i_sgs, i_vect)
!      subroutine cal_rotation_commute_conduct(nmax_grp_sf, ngrp_sf,    &
!     &          id_grp_sf, i_filter, i_sgs, i_vect)
!
!         i_filter: ID for filter function
!
      module commute_error_rotation
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
      use m_node_phys_data
      use m_finite_element_matrix
      use m_int_vol_data
!
      use int_vol_commute_1st
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use int_surf_rot_sgs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_rotation_commute(nmax_grp_sf, ngrp_sf,             &
     &          id_grp_sf, i_filter, i_sgs, i_vect)
!
      use m_geometry_data
      use m_group_data
!
      integer(kind = kint), intent(in) :: nmax_grp_sf
      integer(kind = kint), intent(in) :: ngrp_sf(3)
      integer(kind = kint), intent(in) :: id_grp_sf(nmax_grp_sf,3)
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint), intent(in) :: i_sgs, i_vect
!
!
!  cal commute error using rotation
!
      call reset_ff_smps
!
      call int_vol_commute_rot(ele1%istack_ele_smp, intg_point_t_evo,   &
     &    i_filter, i_vect)
!
      call int_surf_rot_commute_sgs(sf_grp1, intg_point_t_evo,          &
     &    nmax_grp_sf, ngrp_sf, id_grp_sf, i_filter, i_vect)
!
      call set_ff_nl_smp_2_ff(n_vector)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    ff_nl, ml, nod_fld1%ntot_phys, i_sgs, d_nod)
!
      end subroutine cal_rotation_commute
!
!-----------------------------------------------------------------------
!
      subroutine cal_rotation_commute_fluid(nmax_grp_sf, ngrp_sf,       &
     &          id_grp_sf, i_filter, i_sgs, i_vect)
!
      use m_geometry_data
      use m_geometry_data_MHD
      use m_group_data
!
      integer(kind = kint), intent(in) :: nmax_grp_sf
      integer(kind = kint), intent(in) :: ngrp_sf(3)
      integer(kind = kint), intent(in) :: id_grp_sf(nmax_grp_sf,3)
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint), intent(in) :: i_sgs, i_vect
!
!
      call reset_ff_smps
!
      call int_vol_commute_rot(iele_fl_smp_stack, intg_point_t_evo,     &
     &    i_filter, i_vect)
      call int_surf_rot_commute_sgs(sf_grp1, intg_point_t_evo,          &
     &    nmax_grp_sf, ngrp_sf, id_grp_sf, i_filter, i_vect)
!
      call set_ff_nl_smp_2_ff(n_vector)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    ff_nl, ml_fl, nod_fld1%ntot_phys, i_sgs, d_nod)
!
      end subroutine cal_rotation_commute_fluid
!
!-----------------------------------------------------------------------
!
      subroutine cal_rotation_commute_conduct(nmax_grp_sf, ngrp_sf,     &
     &          id_grp_sf, i_filter, i_sgs, i_vect)
!
      use m_geometry_data
      use m_geometry_data_MHD
      use m_group_data
!
      integer(kind = kint), intent(in) :: nmax_grp_sf
      integer(kind = kint), intent(in) :: ngrp_sf(3)
      integer(kind = kint), intent(in) :: id_grp_sf(nmax_grp_sf,3)
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint), intent(in) :: i_sgs, i_vect
!
!
      call reset_ff_smps
!
      call int_vol_commute_rot(iele_cd_smp_stack, intg_point_t_evo,     &
     &    i_filter, i_vect)
      call int_surf_rot_commute_sgs(sf_grp1, intg_point_t_evo,          &
     &    nmax_grp_sf, ngrp_sf, id_grp_sf, i_filter, i_vect)
!
      call set_ff_nl_smp_2_ff(n_vector)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    ff_nl, ml_cd, nod_fld1%ntot_phys, i_sgs, d_nod)
!
      end subroutine cal_rotation_commute_conduct
!
!-----------------------------------------------------------------------
!
      end module commute_error_rotation
