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
      use m_geometry_data
      use m_nod_comm_table
      use m_node_phys_data
      use m_ele_sf_eg_comm_tables
      use m_array_for_send_recv
      use m_FEM_utils
      use input_control_udt_diff
      use const_mesh_info
      use nod_phys_send_recv
      use count_whole_num_element
      use load_mesh_data
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
      if (iflag_debug.eq.1) write(*,*) 'input_mesh_1st'
      call input_mesh_1st(my_rank)
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
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
      call const_element_comm_tables_1st
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
      use m_group_data
      use m_node_phys_data
      use m_cross_section
      use m_control_data_sections
      use m_control_params_4_psf
      use set_parallel_file_name
      use t_read_control_arrays
      use set_psf_case_table
      use sections_for_1st
!
      integer(kind = kint) :: igrp
      integer(kind = kint),  parameter :: id_gname = 11
      character(len=kchara) :: grouping_mesh_list
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_med_patch_ele_grp',         &
     &             trim(grouping_mesh_head)
      num_psf_ctl = ele_grp1%num_grp
      num_psf = num_psf_ctl
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
      if (num_psf .gt. 0)  then
        call cross_section_init_1st
      end if
!
      end subroutine analyze_med_grp_patch
!
! ----------------------------------------------------------------------
!
      end module analyzer_med_grp_patch

