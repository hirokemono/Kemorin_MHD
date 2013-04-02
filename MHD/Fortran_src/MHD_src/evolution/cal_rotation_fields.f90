!
!     module cal_rotation_fields
!
!     Written by H. Matsui
!
!      subroutine cal_vorticity
!      subroutine cal_current_density
!      subroutine cal_magnetic_f_by_vect_p
!
      module cal_rotation_fields
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_vorticity
!
      use m_control_parameter
      use m_node_phys_data
      use m_node_phys_address
      use m_SGS_address
      use m_surf_data_torque
!
      use cal_rotation
      use cal_rotation_sgs
      use nod_phys_send_recv
!
      if ( iflag_SGS_model.ne.id_SGS_none                               &
     &      .and. iflag_commute_velo .eq. id_SGS_commute_ON) then
        call cal_rotation_sgs_fluid(nmax_sf_sgs_velo,                   &
     &      ngrp_sf_sgs_velo, id_grp_sf_sgs_velo, iak_diff_v,           &
     &      iphys%i_vort, iphys%i_velo)
      else
        call cal_rotation_in_fluid(iphys%i_vort, iphys%i_velo)
      end if
!
      call vector_send_recv(iphys%i_vort)
      iflag_nod_update(iphys%i_vort:iphys%i_vort+2) = 1
!
      end subroutine cal_vorticity
!
!-----------------------------------------------------------------------
!
      subroutine cal_current_density
!
      use m_control_parameter
      use m_node_phys_data
      use m_node_phys_address
      use m_SGS_address
      use m_surf_data_magne
!
      use cal_rotation
      use cal_rotation_sgs
      use set_vecp_boundary
      use nod_phys_send_recv
!
      if ( iflag_SGS_model .ne. id_SGS_none                             &
     &     .and. iflag_commute_magne .eq. id_SGS_commute_ON) then
        call cal_rotation_sgs_all(nmax_sf_sgs_magne, ngrp_sf_sgs_magne, &
     &      id_grp_sf_sgs_magne, iak_diff_b, iphys%i_current,           &
     &      iphys%i_magne)
!        call cal_rotation_sgs_conduct(nmax_sf_sgs_magne,               &
!     &      ngrp_sf_sgs_magne, id_grp_sf_sgs_magne, iak_diff_b,        &
!     &      iphys%i_current, iphys%i_magne)
      else
        call cal_rotation_whole(iphys%i_current, iphys%i_magne)
!        call cal_rotation_in_conduct(iphys%i_current, iphys%i_magne)
      end if
!
      call set_boundary_current
      call vector_send_recv(iphys%i_current)
      iflag_nod_update(iphys%i_current:iphys%i_current+2) = 1
!
      end subroutine cal_current_density
!
!-----------------------------------------------------------------------
!
      subroutine cal_magnetic_f_by_vect_p
!
      use m_control_parameter
      use m_node_phys_data
      use m_node_phys_address
      use m_SGS_address
      use m_surf_data_vector_p
!
      use cal_rotation
      use cal_rotation_sgs
      use set_magne_boundary
      use nod_phys_send_recv
!
      if ( iflag_SGS_model.ne.id_SGS_none                               &
     &      .and. iflag_commute_magne .eq. id_SGS_commute_ON) then
        call cal_rotation_sgs_all(nmax_sf_sgs_vect_p,                   &
     &      ngrp_sf_sgs_vect_p, id_grp_sf_sgs_vect_p, iak_diff_b,       &
     &      iphys%i_magne, iphys%i_vecp)
      else
        call cal_rotation_whole(iphys%i_magne, iphys%i_vecp)
      end if
!
      call set_boundary_magne
      call vector_send_recv(iphys%i_magne)
      iflag_nod_update(iphys%i_magne:iphys%i_magne+2) = 1
!
      end subroutine cal_magnetic_f_by_vect_p
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      end module cal_rotation_fields
