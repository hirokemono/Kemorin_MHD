!
!      module set_ctl_interpolation
!
!     Written by H. Matsui on July, 2006
!
!      subroutine set_ctl_params_interpolation
!      subroutine set_ctl_4_itp_steps
!
      module set_ctl_interpolation
!
      use m_precision
!
      use calypso_mpi
      use m_error_IDs
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_interpolation
!
      use m_machine_parameter
      use m_2nd_pallalel_vector
      use m_ctl_params_4_gen_table
      use m_read_mesh_data
      use m_ctl_data_gen_table
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
      use m_file_format_switch
      use m_field_file_format
      use itp_table_IO_select_4_zlib
      use set_control_platform_data
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def(my_rank)
      call set_control_mesh_def
!
      org_mesh_head = mesh_file_head
!
      if (new_mesh_prefix%iflag .ne. 0) then
        dest_mesh_head = new_mesh_prefix%charavalue
      end if
!
      if (i_table_head_ctl .ne. 0) then
        table_file_head = table_head_ctl
      end if
!
      if (i_single_itp_tbl .ne. 0) then
        sgl_table_file_head = single_itp_tbl_head_ctl
      end if
!
      if (iflag_debug.eq.1) then
        write(*,*) 'np_smp', np_smp, np_smp
        write(*,*) 'org_mesh_head: ',   trim(org_mesh_head)
        write(*,*) 'dest_mesh_head: ',  trim(dest_mesh_head)
        write(*,*) 'table_file_head: ', trim(table_file_head)
      end if
!
      if (i_itp_node_head_ctl .ne. 0) then
        itp_node_file_head = itp_node_head_ctl
        if (iflag_debug.eq.1)                                           &
     &   write(*,*) 'itp_node_file_head: ', trim(itp_node_file_head)
      end if
!
      if (new_restart_prefix%iflag .gt. 0) then
        itp_rst_file_head = new_restart_prefix%charavalue
      end if
        if (iflag_debug.eq.1)                                           &
     &   write(*,*) 'itp_rst_file_head: ', trim(itp_rst_file_head)
!
      if (new_field_file_prefix%iflag .gt. 0) then
        itp_udt_file_head = new_field_file_prefix%charavalue
      end if
        if (iflag_debug.eq.1)                                           &
     &   write(*,*) 'itp_udt_file_head: ', trim(itp_udt_file_head)
!
!
      if (restart_file_prefix%iflag .ne. 0) then
        org_rst_file_head = restart_file_prefix%charavalue
      end if
        if (iflag_debug.eq.1)                                           &
     &   write(*,*) 'org_rst_file_head: ', trim(org_rst_file_head)
!
      if (udt_file_head_ctl%iflag .ne. 0) then
        org_udt_file_head = udt_file_head_ctl%charavalue
      end if
        if (iflag_debug.eq.1)                                           &
     &   write(*,*) 'org_udt_file_head: ', trim(org_udt_file_head)
!
!
!
      ndomain_org = 1
      if (ndomain_ctl%iflag .gt. 0) ndomain_org = ndomain_ctl%intvalue
!
      nprocs_2nd = ndomain_org
      if (iflag_debug.eq.1)   write(*,*) 'ndomain_org', nprocs_2nd
!
      if (num_new_domain_ctl%iflag .gt. 0) then
        ndomain_dest = num_new_domain_ctl%intvalue
      else
        ndomain_dest = 1
      end if
!
      call choose_file_format(mesh_file_fmt_ctl%charavalue,             &
     &    mesh_file_fmt_ctl%iflag, ifmt_org_mesh_file)
      call choose_file_format(new_mesh_file_fmt_ctl%charavalue,         &
     &    new_mesh_file_fmt_ctl%iflag, ifmt_itp_mesh_file)
!
      call choose_file_format(ifmt_itp_table_file_ctl, i_fmt_itp_tbl,   &
     &    ifmt_itp_table_file)
!
      call choose_file_format(restart_file_fmt_ctl%charavalue,          &
     &    restart_file_fmt_ctl%iflag, ifmt_org_rst_file)
      call choose_file_format(new_rst_files_fmt_ctl%charavalue,         &
     &    new_rst_files_fmt_ctl%iflag, ifmt_itp_rst_file)
!
      call choose_ucd_file_format(udt_file_fmt_ctl%charavalue,          &
     &    udt_file_fmt_ctl%iflag, itype_org_udt_file)
      call choose_ucd_file_format(new_udt_file_fmt_ctl%charavalue,      &
     &    new_udt_file_fmt_ctl%iflag, itype_itp_udt_file)
!
!
      if (nprocs .ne. max(ndomain_org,ndomain_dest) ) then
        write(e_message,*)                                              &
     &     'Num. of rank is larger num. of orgin or destinate dom.'
        call  calypso_MPI_abort(ierr_P_MPI, e_message)
      end if
!
      end subroutine set_ctl_params_interpolation
!
!  ---------------------------------------------------------------------
!
      subroutine set_ctl_4_itp_steps
!
      use m_machine_parameter
      use m_t_step_parameter
      use m_ctl_data_4_time_steps
!
!   parameters for time evolution
!
        i_step_init   = 0
        if (i_step_init_ctl%iflag .gt. 0) then
          i_step_init   = i_step_init_ctl%intvalue
        end if
!
        if (i_step_number_ctl%iflag .eq. 0) then
          e_message = 'Set step number to finish'
            call calypso_MPI_abort(ierr_evo, e_message)
        else
          i_step_number = i_step_number_ctl%intvalue
        end if
!
        i_step_output_rst = 0
        if (i_step_rst_ctl%intvalue .gt. 0) then
          i_step_output_rst = i_step_rst_ctl%intvalue
        end if
!
        i_step_output_ucd = 0
        if (i_step_ucd_ctl%iflag .gt. 0) then
          i_step_output_ucd = i_step_ucd_ctl%intvalue
        end if
!
        if (iflag_debug.eq.1) then
          write(*,*) 'i_step_init ',i_step_init
          write(*,*) 'i_step_number ',i_step_number
          write(*,*) 'i_step_output_rst ',i_step_output_rst
          write(*,*) 'i_step_output_ucd ',i_step_output_ucd
        end if
!
      end subroutine set_ctl_4_itp_steps
!
! -----------------------------------------------------------------------
!
      end module set_ctl_interpolation
