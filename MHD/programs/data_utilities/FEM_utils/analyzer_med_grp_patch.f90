!analyzer_med_grp_patch.f90
!      module analyzer_med_grp_patch
!
!      subroutine initialize_med_grp_patch
!      subroutine analyze_med_grp_patch
!
!..................................................
!
      module analyzer_med_grp_patch
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
!      modified by H. Matsui on Nov., 2006 
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_med_grp_patch
!
      use m_phys_constants
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_array_for_send_recv
      use m_FEM_utils
      use input_control_udt_diff
      use nod_phys_send_recv
      use count_whole_num_element
      use load_mesh_data
      use const_mesh_information
      use const_element_comm_tables
!
!
      if (my_rank.eq.0) then
        write(*,*) 'diff. udt files'
        write(*,*) 'Input file: mesh data, udt data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_grp_patch'
      call s_input_control_grp_patch(ucd_FUTIL)
      if (iflag_debug.eq.1) write(*,*) 'input_mesh'
      call input_mesh                                                   &
     &   (my_rank, nod_comm, node1, ele1, nod_grp1, ele_grp1, sf_grp1,  &
     &    surf1%nnod_4_surf, edge1%nnod_4_edge)
!
!     --------------------- 
!
!      if (iflag_debug.eq.1) write(*,*) 'const_layers_4_dynamic'
!      call const_layers_4_dynamic(ele_grp1, layer_tbl1)
!
!     --------------------- 
!
!
      if (iflag_debug.ge.1 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(n_sym_tensor, node1%numnod)
      call init_send_recv(nod_comm)
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank,                                    &
     &    node1, ele1, surf1, edge1, nod_grp1, ele_grp1, sf_grp1,       &
     &    ele_grp_tbl1, sf_grp_tbl1, sf_grp_nod1)
      call const_element_comm_tbls(node1, ele1, surf1, edge1,           &
     &    nod_comm, ele_comm, surf_comm, edge_comm)
!
      if(i_debug .eq. iflag_full_msg) then
        call check_whole_num_of_elements(ele1)
      end if
!
      nod_fld1%num_phys = 1
      call alloc_phys_name_type(nod_fld1)
      nod_fld1%num_component(1) = 1
      nod_fld1%istack_component(1) = 1
      nod_fld1%phys_name(1) = 'temperature'
!
      call calypso_mpi_barrier
!
      end subroutine initialize_med_grp_patch
!
! ----------------------------------------------------------------------
!
      subroutine analyze_med_grp_patch
!
      use m_ctl_params_4_diff_udt
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_cross_section
      use m_control_data_sections
      use m_control_params_4_psf
      use set_parallel_file_name
      use t_read_control_arrays
      use set_psf_case_table
!
      integer(kind = kint) :: igrp
      integer(kind = kint),  parameter :: id_gname = 11
      character(len=kchara) :: grouping_mesh_list
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_med_patch_ele_grp',         &
     &             trim(grouping_mesh_head)
      num_psf_ctl = ele_grp1%num_grp
      call allocate_psf_ctl_stract
!
      if(my_rank .eq. 0) then
        call add_dat_extension(grouping_mesh_head,grouping_mesh_list)
        open(id_gname,file=grouping_mesh_list)
        write(id_gname,'(i16)') ele_grp1%num_grp
        do igrp = 1, ele_grp1%num_grp
          write(id_gname,'(i16,a3,a)') igrp, '   ',                     &
     &              trim(ele_grp1%grp_name(igrp))
        end do
        close(id_gname)
      end if
!
      do igrp = 1, ele_grp1%num_grp
        fname_psf_ctl(igrp) = 'NO_FILE'
        psf_ctl_struct(igrp)%i_psf_file_head = 1
        call add_int_suffix(igrp, grouping_mesh_head,                   &
     &      psf_ctl_struct(igrp)%psf_file_head_ctl)
!
        psf_ctl_struct(igrp)%i_psf_out_type = 1
        psf_ctl_struct(igrp)%psf_output_type_ctl = 'VTD'
!
        psf_ctl_struct(igrp)%section_method_ctl = 'equation'
!
        psf_ctl_struct(igrp)%psf_coefs_ctl%num = 1
        call alloc_control_array_c_r(psf_ctl_struct(igrp)%psf_coefs_ctl)
        psf_ctl_struct(igrp)%psf_coefs_ctl%c_tbl(1) = 'y'
        psf_ctl_struct(igrp)%psf_coefs_ctl%vect(1) = -1.0d0
!
        psf_ctl_struct(igrp)%psf_area_ctl%num = 1
        call alloc_control_array_chara                                  &
     &     (psf_ctl_struct(igrp)%psf_area_ctl)
        psf_ctl_struct(igrp)%psf_area_ctl%c_tbl(1)                      &
     &      = ele_grp1%grp_name(igrp)
!
        psf_ctl_struct(igrp)%psf_out_field_ctl%num = 1
        call alloc_control_array_c2                                     &
     &     (psf_ctl_struct(igrp)%psf_out_field_ctl)
        psf_ctl_struct(igrp)%psf_out_field_ctl%c1_tbl(1)                &
     &      = nod_fld1%phys_name(1)
        psf_ctl_struct(igrp)%psf_out_field_ctl%c2_tbl(1)                &
     &      = 'scalar'
      end do
!
      call set_sectioning_case_table
      call SECTIONING_initialize                                        &
     &   (node1, ele1, surf1, edge1, nod_comm, edge_comm,               &
     &    ele_grp1, sf_grp1, sf_grp_nod1, nod_fld1)
!
      end subroutine analyze_med_grp_patch
!
! ----------------------------------------------------------------------
!
      end module analyzer_med_grp_patch

