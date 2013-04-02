!collect_psf_data
!      module collect_psf_data
!
!      Written by H. Matsui on July, 2006
!
!      subroutine collect_numbers_4_psf
!      subroutine collect_mesh_4_psf
!      subroutine collect_field_4_psf
!
!      subroutine collect_numbers_4_iso
!      subroutine collect_data_4_iso
!
      module collect_psf_data
!
      use m_precision
!
      use m_constants
      use m_parallel_var_dof
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine collect_numbers_4_psf
!
      use m_control_params_4_psf
      use m_patch_data_psf
      use m_psf_outputs
      use count_numbers_collected_psf
!
!
      call count_numbers_4_psf_out(num_psf, istack_nod_psf_smp,         &
     &    ntot_nod_output_psf, nmax_nod_para_psf, nnod_para_psf,        &
     &    istack_nod_para_psf, nnod_recv_psf, istack_nod_recv_psf,      &
     &    nnod_output_psf, istack_nod_output_psf)
!
      call time_prog_barrier
!
      call count_numbers_4_psf_out(num_psf, istack_patch_psf_smp,       &
     &    ntot_ele_output_psf, nmax_ele_para_psf, nele_para_psf,        &
     &    istack_ele_para_psf, nele_recv_psf, istack_ele_recv_psf,      &
     &    nele_output_psf, istack_ele_output_psf)
!
      call time_prog_barrier
!
      end subroutine collect_numbers_4_psf
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine collect_mesh_4_psf
!
      use m_control_params_4_psf
      use m_patch_data_psf
      use m_psf_outputs
!
      use psf_send_recv
      use reconnect_psf_overlap_nod
!
      call psf_grids_send_recv(num_psf, nnod_psf_tot,                   &
     &    ntot_nod_output_psf, istack_nod_para_psf, nnod_recv_psf,      &
     &    istack_nod_recv_psf, xyz_psf,                                 &
     &    send_psf, recv_psf, xx_output_psf)
!
      call psf_hash_send_recv(num_psf, nnod_psf_tot,                    &
     &    ntot_nod_output_psf, istack_nod_para_psf, nnod_recv_psf,      &
     &    istack_nod_recv_psf, inod_hash_psf,                           &
     &    isend_psf(1), irecv_psf(1), ihash_output_psf)
!
      call set_global_psf_node_id(num_psf, ntot_nod_output_psf,         &
     &    istack_nod_output_psf, inod_output_psf)
!
      call time_prog_barrier
!
      call psf_connect_send_recv(num_psf, npatch_tot_psf_smp,           &
     &    ntot_ele_output_psf, istack_nod_para_psf,                     &
     &    istack_ele_para_psf, nele_recv_psf, istack_ele_recv_psf,      &
     &    ie_patch_psf, isend_psf(1), irecv_psf(1), ie_output_psf)
!
      call s_reconnect_psf_overlap_nod(num_psf, ntot_nod_output_psf,    &
     &    ntot_ele_output_psf, istack_nod_output_psf,                   &
     &    istack_ele_output_psf, ihash_output_psf, xx_output_psf,       &
     &    iele_output_psf, ie_output_psf)
!
      call time_prog_barrier
!
      end subroutine collect_mesh_4_psf
!
! ----------------------------------------------------------------------
!
      subroutine collect_field_4_psf
!
      use m_control_params_4_psf
      use m_patch_data_psf
      use m_psf_outputs
!
      use psf_send_recv
!
      call psf_results_send_recv(num_psf, nnod_psf_tot,                 &
     &    ntot_nod_output_psf, istack_nod_para_psf, nnod_recv_psf,      &
     &    istack_nod_recv_psf, max_ncomp_psf_out, dat_psf,              &
     &    send_psf, recv_psf, dat_output_psf)
!
      call time_prog_barrier
!
      end subroutine collect_field_4_psf
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine collect_numbers_4_iso
!
      use m_control_params_4_iso
      use m_patch_data_iso
      use m_iso_outputs
      use count_numbers_collected_psf
!
!
      call count_numbers_4_psf_out(num_iso, istack_nod_iso_smp,         &
     &    ntot_nod_output_iso, nmax_nod_para_iso, nnod_para_iso,        &
     &    istack_nod_para_iso, nnod_recv_iso, istack_nod_recv_iso,      &
     &    nnod_output_iso, istack_nod_output_iso)
!
      call time_prog_barrier
!
      call count_numbers_4_psf_out(num_iso, istack_patch_iso_smp,       &
     &    ntot_ele_output_iso, nmax_ele_para_iso, nele_para_iso,        &
     &    istack_ele_para_iso, nele_recv_iso, istack_ele_recv_iso,      &
     &    nele_output_iso, istack_ele_output_iso)
!
      call time_prog_barrier
!
      end subroutine collect_numbers_4_iso
!
! ----------------------------------------------------------------------
!
      subroutine collect_data_4_iso
!
      use m_control_params_4_iso
      use m_patch_data_iso
      use m_iso_outputs
!
      use psf_send_recv
      use reconnect_psf_overlap_nod
!
!
      call psf_grids_send_recv(num_iso, nnod_iso_tot,                   &
     &    ntot_nod_output_iso, istack_nod_para_iso, nnod_recv_iso,      &
     &    istack_nod_recv_iso, xyz_iso,                                 &
     &    send_iso, recv_iso, xx_output_iso)
!
      call psf_hash_send_recv(num_iso, nnod_iso_tot,                    &
     &    ntot_nod_output_iso, istack_nod_para_iso, nnod_recv_iso,      &
     &    istack_nod_recv_iso, inod_hash_iso,                           &
     &    isend_iso(1), irecv_iso(1), ihash_output_iso)
!
      call set_global_psf_node_id(num_iso, ntot_nod_output_iso,         &
     &    istack_nod_output_iso, inod_output_iso)
!
      call psf_connect_send_recv(num_iso, npatch_tot_iso_smp,           &
     &    ntot_ele_output_iso, istack_nod_para_iso,                     &
     &    istack_ele_para_iso, nele_recv_iso, istack_ele_recv_iso,      &
     &    ie_patch_iso, isend_iso(1), irecv_iso(1), ie_output_iso)
!
      call s_reconnect_psf_overlap_nod(num_iso, ntot_nod_output_iso,    &
     &    ntot_ele_output_iso, istack_nod_output_iso,                   &
     &    istack_ele_output_iso, ihash_output_iso, xx_output_iso,       &
     &    iele_output_iso, ie_output_iso)
!
      call time_prog_barrier
!
      call psf_results_send_recv(num_iso, nnod_iso_tot,                 &
     &    ntot_nod_output_iso, istack_nod_para_iso, nnod_recv_iso,      &
     &    istack_nod_recv_iso, max_ncomp_iso_out, dat_iso,              &
     &    send_iso, recv_iso, dat_output_iso)
!
      call time_prog_barrier
!
      end subroutine collect_data_4_iso
!
! ----------------------------------------------------------------------
!
      end module collect_psf_data
