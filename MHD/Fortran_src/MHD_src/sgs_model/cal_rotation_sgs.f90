!
!     module cal_rotation_sgs
!
!     Written by H. Matsui
!
!      subroutine cal_rotation_sgs_all(iflag_4_supg,                    &
!     &          nmax_grp_sf, ngrp_sf, id_grp_sf, iak_diff,             &
!     &          i_res, i_vector)
!      subroutine cal_rotation_sgs_fluid(iflag_4_supg,                  &
!     &          nmax_grp_sf, ngrp_sf, id_grp_sf, iak_diff,             &
!     &          i_res, i_vector)
!      subroutine cal_rotation_sgs_conduct(iflag_4_supg,                &
!     &          nmax_grp_sf, ngrp_sf, id_grp_sf, iak_diff,             &
!     &          i_res, i_vector)
!
      module cal_rotation_sgs
!
      use m_precision
!
      use m_phys_constants
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
!
      private :: choose_int_vol_rot_sgs
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_rotation_sgs_all(iflag_4_supg,                     &
     &          nmax_grp_sf, ngrp_sf, id_grp_sf, iak_diff,              &
     &          i_res, i_vector)
!
      use m_geometry_data
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: i_vector, i_res
      integer(kind = kint), intent(in) :: iak_diff
      integer(kind = kint), intent(in) :: nmax_grp_sf
      integer(kind = kint), intent(in) :: ngrp_sf(3)
      integer(kind = kint), intent(in) :: id_grp_sf(nmax_grp_sf,3)
!
!
      call reset_ff_smps
!
      call choose_int_vol_rot_sgs(iflag_4_supg, iele_smp_stack,         &
     &     nmax_grp_sf, ngrp_sf, id_grp_sf, iak_diff, i_vector)
!
      call set_ff_nl_smp_2_ff(n_vector)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    d_nod(1,i_res), ff_nl, ml)
!
! ----------   communications
!
      call vector_send_recv(i_res)
!
      end subroutine cal_rotation_sgs_all
!
!-----------------------------------------------------------------------
!
      subroutine cal_rotation_sgs_fluid(iflag_4_supg,                   &
     &          nmax_grp_sf, ngrp_sf, id_grp_sf, iak_diff,              &
     &          i_res, i_vector)
!
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: i_vector, i_res
      integer(kind = kint), intent(in) :: iak_diff
      integer(kind = kint), intent(in) :: nmax_grp_sf
      integer(kind = kint), intent(in) :: ngrp_sf(3)
      integer(kind = kint), intent(in) :: id_grp_sf(nmax_grp_sf,3)
!
!
      call reset_ff_smps
!
      call choose_int_vol_rot_sgs(iflag_4_supg, iele_fl_smp_stack,      &
     &     nmax_grp_sf, ngrp_sf, id_grp_sf, iak_diff, i_vector)
!
      call set_ff_nl_smp_2_ff(n_vector)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    d_nod(1,i_res), ff_nl, ml_fl)
!
! ----------   communications
!
      call vector_send_recv(i_res)
!
      end subroutine cal_rotation_sgs_fluid
!
!-----------------------------------------------------------------------
!
      subroutine cal_rotation_sgs_conduct(iflag_4_supg,                 &
     &          nmax_grp_sf, ngrp_sf, id_grp_sf, iak_diff,              &
     &          i_res, i_vector)
!
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: i_vector, i_res
      integer(kind = kint), intent(in) :: iak_diff
      integer(kind = kint), intent(in) :: nmax_grp_sf
      integer(kind = kint), intent(in) :: ngrp_sf(3)
      integer(kind = kint), intent(in) :: id_grp_sf(nmax_grp_sf,3)
!
!
      call reset_ff_smps
!
      call choose_int_vol_rot_sgs(iflag_4_supg, iele_cd_smp_stack,      &
     &     nmax_grp_sf, ngrp_sf, id_grp_sf, iak_diff, i_vector)
!
      call set_ff_nl_smp_2_ff(n_vector)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    d_nod(1,i_res), ff_nl, ml_cd)
!
! ----------   communications
!
      call vector_send_recv(i_res)
!
      end subroutine cal_rotation_sgs_conduct
!
!-----------------------------------------------------------------------
!
      subroutine choose_int_vol_rot_sgs(iflag_4_supg, iele_fsmp_stack,  &
     &          nmax_grp_sf, ngrp_sf, id_grp_sf, iak_diff, i_vector)
!
      use m_group_data
      use m_control_parameter
      use m_element_phys_address
      use int_sgs_vect_diff_1st
      use int_sgs_vect_diff_upw_1st
      use int_surf_rot_sgs
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: nmax_grp_sf
      integer(kind = kint), intent(in) :: ngrp_sf(3)
      integer(kind = kint), intent(in) :: id_grp_sf(nmax_grp_sf,3)
      integer(kind = kint), intent(in) :: iak_diff, i_vector
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
       if ( iflag_4_supg .eq. id_magnetic_SUPG) then
        call int_sgs_rot_upw_1st(iele_fsmp_stack, intg_point_t_evo,     &
     &      ifilter_final, iak_diff, i_vector, iphys_ele%i_magne)
       else if ( iflag_4_supg .eq. id_turn_ON) then
        call int_sgs_rot_upw_1st(iele_fsmp_stack, intg_point_t_evo,     &
     &      ifilter_final, iak_diff, i_vector, iphys_ele%i_velo)
       else
        call int_sgs_rot_1st(iele_fsmp_stack, intg_point_t_evo,         &
     &      ifilter_final, iak_diff, i_vector)
       end if
!
       call int_surf_rotation_sgs(sf_grp1, intg_point_t_evo,            &
     &     nmax_grp_sf, ngrp_sf, id_grp_sf, ifilter_final, iak_diff,    &
     &     i_vector)
!
      end subroutine choose_int_vol_rot_sgs
!
!-----------------------------------------------------------------------
!
      end module cal_rotation_sgs
