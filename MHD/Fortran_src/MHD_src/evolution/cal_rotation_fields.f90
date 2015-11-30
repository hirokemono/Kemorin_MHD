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
      use m_geometry_data
      use m_nod_comm_table
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
        call cal_rotation_sgs_fluid                                     &
     &     (iflag_velo_supg, sf_sgs1_grad_v%nmax_sf_dat,                &
     &      sf_sgs1_grad_v%ngrp_sf_dat, sf_sgs1_grad_v%id_grp_sf_dat,   &
     &      iak_diff_v, iphys%i_vort, iphys%i_velo)
      else
        call cal_rotation_in_fluid(iflag_velo_supg,                     &
     &      iphys%i_vort, iphys%i_velo)
      end if
!
      call vector_send_recv(iphys%i_vort, node1, nod_comm, nod_fld1)
      nod_fld1%iflag_update(iphys%i_vort:iphys%i_vort+2) = 1
!
      end subroutine cal_vorticity
!
!-----------------------------------------------------------------------
!
      subroutine cal_current_density
!
      use m_control_parameter
      use m_node_phys_address
      use m_node_phys_data
      use m_SGS_address
      use m_bc_data_magne
      use m_surf_data_magne
      use m_surf_data_current
!
      use cal_rotation
      use cal_rotation_sgs
      use set_boundary_scalars
      use nod_phys_send_recv
!
      if ( iflag_SGS_model .ne. id_SGS_none                             &
     &     .and. iflag_commute_magne .eq. id_SGS_commute_ON) then
        call cal_rotation_sgs_all                                       &
     &     (iflag_mag_supg, sf_sgs1_grad_b%nmax_sf_dat,                 &
     &      sf_sgs1_grad_b%ngrp_sf_dat, sf_sgs1_grad_b%id_grp_sf_dat,   &
     &      iak_diff_b, iphys%i_current, iphys%i_magne)
!        call cal_rotation_sgs_conduct                                  &
!     &     (iflag_mag_supg, sf_sgs1_grad_b%nmax_sf_dat,                &
!     &      sf_sgs1_grad_b%ngrp_sf_dat, sf_sgs1_grad_b%id_grp_sf_dat,  &
!     &      iak_diff_b, iphys%i_current, iphys%i_magne)
      else
        call cal_rotation_whole(iflag_mag_supg,                         &
     &      iphys%i_current, iphys%i_magne)
!        call cal_rotation_in_conduct(iphys%i_current, iphys%i_magne)
      end if
!
      call set_boundary_vect(nod_bc1_j, iphys%i_current, nod_fld1)
!
      call vector_send_recv(iphys%i_current, node1, nod_comm, nod_fld1)
      nod_fld1%iflag_update(iphys%i_current:iphys%i_current+2) = 1
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
      use m_bc_data_magne
!
      use cal_rotation
      use cal_rotation_sgs
      use set_boundary_scalars
      use nod_phys_send_recv
!
      if ( iflag_SGS_model.ne.id_SGS_none                               &
     &      .and. iflag_commute_magne .eq. id_SGS_commute_ON) then
        call cal_rotation_sgs_all(iflag_mag_supg,                       &
     &      sf_sgs1_grad_a%nmax_sf_dat, sf_sgs1_grad_a%ngrp_sf_dat,     &
     &      sf_sgs1_grad_a%id_grp_sf_dat, iak_diff_b,                   &
     &      iphys%i_magne, iphys%i_vecp)
      else
        call cal_rotation_whole(iflag_mag_supg,                         &
     &      iphys%i_magne, iphys%i_vecp)
      end if
!
      call set_boundary_vect(nod_bc1_b, iphys%i_magne, nod_fld1)
!
      call vector_send_recv(iphys%i_magne, node1, nod_comm, nod_fld1)
      nod_fld1%iflag_update(iphys%i_magne:iphys%i_magne+2) = 1
!
      end subroutine cal_magnetic_f_by_vect_p
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      end module cal_rotation_fields
