!>@file   MHD_field_by_rotation.f90
!!@brief  module MHD_field_by_rotation
!!
!!@author H. Matsui
!!@date Programmed...when??
!
!>@brief Evaluate vorticity and current density
!!
!!@verbatim
!!      subroutine cal_field_by_rotation
!!@endverbatim
!
      module MHD_field_by_rotation
!
      use m_precision
      use m_constants
!
      use m_geometry_data
      use m_group_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_finite_element_matrix
      use m_int_vol_data
      use m_element_phys_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_field_by_rotation
!
      use m_machine_parameter
      use m_control_parameter
      use m_nod_comm_table
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_SGS_address
      use m_bc_data_velo
      use m_bc_data_magne
      use m_surf_data_torque
      use m_surf_data_magne
      use m_surf_data_current
      use m_filter_elength
!
      use cal_rotation_sgs
!
!
      if(iphys%i_vort .gt. izero)then
        if(nod_fld1%iflag_update(iphys%i_vort) .eq. izero) then
          if (iflag_debug .ge. iflag_routine_msg)                       &
     &        write(*,*) 'cal_vorticity'
          call choose_cal_rotation_sgs                                  &
     &       (iflag_commute_velo, iflag_velo_supg,                      &
     &        iak_diff_v, iphys%i_velo, iphys%i_vort,                   &
     &        fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,          &
     &        nod_comm, node1, ele1, surf1, sf_grp1, iphys_ele, fld_ele1,         &
     &        jac1_3d_q, jac1_sf_grp_2d_q, FEM1_elen, nod_bc1_w,        &
     &        sf_sgs1_grad_v, rhs_tbl1, fem1_wk, f1_nl, nod_fld1)
        end if
      end if
!
      if(iphys%i_current .gt. izero)then
        if(nod_fld1%iflag_update(iphys%i_current) .eq.0 ) then
          if(iflag_t_evo_4_vect_p .gt. id_no_evolution) then
            if (iflag_debug .ge. iflag_routine_msg)                     &
     &        write(*,*) 'cal_current_density'
              call choose_cal_rotation_sgs                              &
     &           (iflag_commute_magne, iflag_mag_supg,                  &
     &            iak_diff_b, iphys%i_magne, iphys%i_current,           &
     &            ele1%istack_ele_smp, m1_lump, nod_comm, node1, ele1,  &
     &            surf1, sf_grp1, iphys_ele, fld_ele1, jac1_3d_q,       &
     &            jac1_sf_grp_2d_q, FEM1_elen, nod_bc1_j,               &
     &            sf_sgs1_grad_b, rhs_tbl1, fem1_wk, f1_nl, nod_fld1)
!
!             call choose_cal_rotation_sgs                              &
!     &          (iflag_commute_magne, iflag_mag_supg,                  &
!     &           iak_diff_b, iphys%i_magne, iphys%i_current,           &
!     &           conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,    &
!     &           nod_comm, node1, ele1, surf1, sf_grp1, iphys_ele, fld_ele1,     &
!     &           jac1_3d_q,  jac1_sf_grp_2d_q, FEM1_elen, nod_bc1_j,   &
!     &           sf_sgs1_grad_b, rhs_tbl1, fem1_wk, f1_nl, nod_fld1)
!             call int_current_diffuse                                  &
!     &         (nod_comm, node1, ele1, surf1, sf_grp1,                 &
!     &          iphys, jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, m1_lump, &
!     &          mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
          else
            if (iflag_debug .ge. iflag_routine_msg)                     &
     &        write(*,*) 'cal_current_density'
            call choose_cal_rotation_sgs                                &
               (iflag_commute_magne, iflag_mag_supg,                    &
     &          iak_diff_b, iphys%i_magne, iphys%i_current,             &
     &          ele1%istack_ele_smp, m1_lump, nod_comm, node1, ele1,    &
     &          surf1, sf_grp1, iphys_ele, fld_ele1, jac1_3d_q,         &
     &          jac1_sf_grp_2d_q, FEM1_elen, nod_bc1_j, sf_sgs1_grad_b, &
     &          rhs_tbl1, fem1_wk, f1_nl, nod_fld1)
!           call choose_cal_rotation_sgs                                &
!     &        (iflag_commute_magne, iflag_mag_supg,                    &
!     &         iak_diff_b, iphys%i_magne, iphys%i_current,             &
!     &         conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,      &
!     &         nod_comm, node1, ele1, surf1, sf_grp1, iphys_ele, fld_ele1,       &
!     &         jac1_3d_q, jac1_sf_grp_2d_q, FEM1_elen, nod_bc1_j,      &
!     &         sf_sgs1_grad_b, rhs_tbl1, fem1_wk, f1_nl, nod_fld1)
          end if
        end if
      end if
!
      end subroutine cal_field_by_rotation
!
! ----------------------------------------------------------------------
!
      end module MHD_field_by_rotation
