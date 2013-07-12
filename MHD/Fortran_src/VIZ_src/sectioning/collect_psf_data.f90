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
      use m_geometry_constants
      use m_control_params_4_psf
      use m_patch_data_psf
      use m_psf_outputs
      use count_numbers_collected_psf
!
      integer(kind = kint) :: i_psf, i_fld, ist
!
!
      call count_numbers_4_psf_out(num_psf, istack_nod_psf_smp,         &
     &    ntot_nod_output_psf, nmax_nod_para_psf, nnod_para_psf,        &
     &    istack_nod_para_psf, nnod_recv_psf, istack_nod_recv_psf,      &
     &    istack_nod_output_psf)
!
      call count_numbers_4_psf_out(num_psf, istack_patch_psf_smp,       &
     &    ntot_ele_output_psf, nmax_ele_para_psf, nele_para_psf,        &
     &    istack_ele_para_psf, nele_recv_psf, istack_ele_recv_psf,      &
     &    istack_ele_output_psf)
!
      if(my_rank .gt. 0) return
      psf_out(1:num_psf)%file_prefix = psf_header(1:num_psf)
      psf_out(1:num_psf)%ifmt_file = itype_psf_file(1:num_psf)
      psf_out(1:num_psf)%nnod_4_ele = num_triangle
!
      do i_psf = 1, num_psf
        psf_out(i_psf)%nnod = istack_nod_output_psf(i_psf)              &
     &                       - istack_nod_output_psf(i_psf-1)
        psf_out(i_psf)%nele = istack_ele_output_psf(i_psf)              &
     &                       - istack_ele_output_psf(i_psf-1)
        psf_out(i_psf)%num_field = num_psf_output(i_psf)
        psf_out(i_psf)%ntot_comp = num_psf_out_comp(i_psf)
!
        call allocate_ucd_phys_name( psf_out(i_psf) )
        ist = istack_psf_output(i_psf-1)
!
        do i_fld = 1, psf_out(i_psf)%num_field
          psf_out(i_psf)%phys_name(i_fld) = name_psf_output(ist+i_fld)
          psf_out(i_psf)%num_comp(i_fld) =  ncomp_psf_output(ist+i_fld)
        end do
      end do
!
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
     &    ntot_nod_output_psf, istack_nod_para_psf,                     &
     &    istack_nod_recv_psf, xyz_psf, send_psf, recv_psf, psf_out)
!
      call psf_hash_send_recv(num_psf, nnod_psf_tot,                    &
     &    ntot_nod_output_psf, istack_nod_para_psf,                     &
     &    istack_nod_recv_psf, inod_hash_psf,                           &
     &    isend_psf(1), irecv_psf(1), ihash_output_psf)
!
      call set_global_psf_node_id(num_psf, psf_out)
!
      call psf_connect_send_recv(num_psf, npatch_tot_psf_smp,           &
     &    ntot_ele_output_psf, istack_nod_para_psf,                     &
     &    istack_ele_para_psf, istack_ele_recv_psf, ie_patch_psf,       &
     &    isend_psf(1), irecv_psf(1), psf_out)
!
      call s_reconnect_psf_overlap_nod(num_psf, ntot_nod_output_psf,    &
     &    istack_nod_output_psf, ihash_output_psf, psf_out)
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
     &    ntot_nod_output_psf, istack_nod_para_psf,                     &
     &    istack_nod_recv_psf, max_ncomp_psf_out, dat_psf,              &
     &    send_psf, recv_psf, psf_out)
!
      end subroutine collect_field_4_psf
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine collect_numbers_4_iso
!
      use m_geometry_constants
      use m_control_params_4_iso
      use m_patch_data_iso
      use m_iso_outputs
      use count_numbers_collected_psf
!
      integer(kind = kint) :: i_iso, i_fld, ist
!
!
      call count_numbers_4_psf_out(num_iso, istack_nod_iso_smp,         &
     &    ntot_nod_output_iso, nmax_nod_para_iso, nnod_para_iso,        &
     &    istack_nod_para_iso, nnod_recv_iso, istack_nod_recv_iso,      &
     &    istack_nod_output_iso)
!
      call count_numbers_4_psf_out(num_iso, istack_patch_iso_smp,       &
     &    ntot_ele_output_iso, nmax_ele_para_iso, nele_para_iso,        &
     &    istack_ele_para_iso, nele_recv_iso, istack_ele_recv_iso,      &
     &    istack_ele_output_iso)
!
      if(my_rank .gt. 0) return
      iso_out(1:num_iso)%file_prefix = iso_header(1:num_iso)
      iso_out(1:num_iso)%ifmt_file = itype_iso_file(1:num_iso)
      iso_out(1:num_iso)%nnod_4_ele = num_triangle
!
      do i_iso = 1, num_iso
        iso_out(i_iso)%nnod = istack_nod_output_iso(i_iso)              &
     &                       - istack_nod_output_iso(i_iso-1)
        iso_out(i_iso)%nele = istack_ele_output_iso(i_iso)              &
     &                       - istack_ele_output_iso(i_iso-1)
        iso_out(i_iso)%num_field = num_iso_output(i_iso)
        iso_out(i_iso)%ntot_comp = num_iso_out_comp(i_iso)
!
        call allocate_ucd_phys_name( iso_out(i_iso) )
!
        ist = istack_iso_output(i_iso-1)
        do i_fld = 1, iso_out(i_iso)%num_field
          iso_out(i_iso)%phys_name(i_fld) = name_iso_output(ist+i_fld)
          iso_out(i_iso)%num_comp(i_fld) =  ncomp_iso_output(ist+i_fld)
        end do
      end do
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
     &    ntot_nod_output_iso, istack_nod_para_iso,                     &
     &    istack_nod_recv_iso, xyz_iso, send_iso, recv_iso, iso_out)
!
      call psf_hash_send_recv(num_iso, nnod_iso_tot,                    &
     &    ntot_nod_output_iso, istack_nod_para_iso,                     &
     &    istack_nod_recv_iso, inod_hash_iso,                           &
     &    isend_iso(1), irecv_iso(1), ihash_output_iso)
!
      call set_global_psf_node_id(num_iso, iso_out)
!
      call psf_connect_send_recv(num_iso, npatch_tot_iso_smp,           &
     &    ntot_ele_output_iso, istack_nod_para_iso,                     &
     &    istack_ele_para_iso, istack_ele_recv_iso, ie_patch_iso,       &
     &    isend_iso(1), irecv_iso(1), iso_out)
!
      call s_reconnect_psf_overlap_nod(num_iso, ntot_nod_output_iso,    &
     &    istack_nod_output_iso, ihash_output_iso, iso_out)
!
      call psf_results_send_recv(num_iso, nnod_iso_tot,                 &
     &    ntot_nod_output_iso, istack_nod_para_iso,                     &
     &    istack_nod_recv_iso, max_ncomp_iso_out, dat_iso,              &
     &    send_iso, recv_iso, iso_out)
!
      end subroutine collect_data_4_iso
!
! ----------------------------------------------------------------------
!
      end module collect_psf_data
