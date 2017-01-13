!
!      module set_ctl_interpolation
!
!     Written by H. Matsui on July, 2006
!
!!      subroutine set_ctl_params_interpolation
!!      subroutine set_ctl_4_itp_steps(tctl)
!!        type(time_data_control), save :: tctl
!
      module set_ctl_interpolation
!
      use m_precision
!
      use calypso_mpi
      use m_error_IDs
!
      use t_file_IO_parameter
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
      use m_ctl_data_gen_table
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
      use m_file_format_switch
      use m_field_file_format
      use itp_table_IO_select_4_zlib
      use set_control_platform_data
!
!
      call turn_off_debug_flag_by_ctl(my_rank, plt1)
      call set_control_smp_def(my_rank, plt1)
!
      call set_control_mesh_def(plt1, itp_org_mesh_file)
!
      if (new_plt%mesh_file_prefix%iflag .ne. 0) then
        itp_dest_mesh_file%file_prefix                                  &
     &     = new_plt%mesh_file_prefix%charavalue
      end if
!
      if (table_head_ctl%iflag .ne. 0) then
        table_file_head = table_head_ctl%charavalue
      end if
!
      if (single_itp_tbl_head_ctl%iflag .ne. 0) then
        sgl_table_file_head = single_itp_tbl_head_ctl%charavalue
      end if
!
      if (iflag_debug.eq.1) then
        write(*,*) 'np_smp', np_smp, np_smp
        write(*,*) 'org_mesh_head: ',                                   &
     &            trim(itp_org_mesh_file%file_prefix)
        write(*,*) 'dest_mesh_head: ',                                  &
     &            trim(itp_dest_mesh_file%file_prefix)
        write(*,*) 'table_file_head: ', trim(table_file_head)
      end if
!
      if (itp_node_head_ctl%iflag .ne. 0) then
        itp_node_file_head = itp_node_head_ctl%charavalue
        if (iflag_debug.eq.1)                                           &
     &   write(*,*) 'itp_node_file_head: ', trim(itp_node_file_head)
      end if
!
      if(new_plt%restart_file_prefix%iflag .gt. 0) then
        itp_rst_file_head = new_plt%restart_file_prefix%charavalue
      end if
        if (iflag_debug.eq.1)                                           &
     &   write(*,*) 'itp_rst_file_head: ', trim(itp_rst_file_head)
!
      if (new_plt%field_file_prefix%iflag .gt. 0) then
        itp_udt_file_head = new_plt%field_file_prefix%charavalue
      end if
        if (iflag_debug.eq.1)                                           &
     &   write(*,*) 'itp_udt_file_head: ', trim(itp_udt_file_head)
!
!
      if (plt1%restart_file_prefix%iflag .ne. 0) then
        org_rst_file_head = plt1%restart_file_prefix%charavalue
      end if
        if (iflag_debug.eq.1)                                           &
     &   write(*,*) 'org_rst_file_head: ', trim(org_rst_file_head)
!
      if (plt1%field_file_prefix%iflag .ne. 0) then
        org_udt_file_head = plt1%field_file_prefix%charavalue
      end if
        if (iflag_debug.eq.1)                                           &
     &   write(*,*) 'org_udt_file_head: ', trim(org_udt_file_head)
!
!
!
      ndomain_org = 1
      if (plt1%ndomain_ctl%iflag .gt. 0) then
        ndomain_org = plt1%ndomain_ctl%intvalue
      end if
!
      nprocs_2nd = ndomain_org
      if (iflag_debug.eq.1)   write(*,*) 'ndomain_org', nprocs_2nd
!
      if (new_plt%ndomain_ctl%iflag .gt. 0) then
        ndomain_dest = new_plt%ndomain_ctl%intvalue
      else
        ndomain_dest = 1
      end if
!
      call choose_file_format                                           &
     &   (new_plt%mesh_file_fmt_ctl, itp_dest_mesh_file%iflag_format)
!
      call choose_file_format                                           &
     &   (fmt_itp_table_file_ctl, ifmt_itp_table_file)
!
      call choose_para_file_format                                      &
     &   (plt1%restart_file_fmt_ctl, ifmt_org_rst_file)
      call choose_para_file_format                                      &
     &   (new_plt%restart_file_fmt_ctl, ifmt_itp_rst_file)
!
      call choose_ucd_file_format(plt1%field_file_fmt_ctl%charavalue,   &
     &    plt1%field_file_fmt_ctl%iflag, itype_org_udt_file)
      call choose_ucd_file_format                                       &
     &   (new_plt%field_file_fmt_ctl%charavalue,                        &
     &    new_plt%field_file_fmt_ctl%iflag, itype_itp_udt_file)
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
      subroutine set_ctl_4_itp_steps(tctl)
!
      use m_machine_parameter
      use m_t_step_parameter
      use t_ctl_data_4_time_steps
!
      type(time_data_control), intent(in) :: tctl
!
!   parameters for time evolution
!
        i_step_init   = 0
        if (tctl%i_step_init_ctl%iflag .gt. 0) then
          i_step_init   = tctl%i_step_init_ctl%intvalue
        end if
!
        if (tctl%i_step_number_ctl%iflag .eq. 0) then
          e_message = 'Set step number to finish'
            call calypso_MPI_abort(ierr_evo, e_message)
        else
          i_step_number = tctl%i_step_number_ctl%intvalue
        end if
!
        i_step_output_rst = 0
        if (tctl%i_step_rst_ctl%intvalue .gt. 0) then
          i_step_output_rst = tctl%i_step_rst_ctl%intvalue
        end if
!
        i_step_output_ucd = 0
        if (tctl%i_step_ucd_ctl%iflag .gt. 0) then
          i_step_output_ucd = tctl%i_step_ucd_ctl%intvalue
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
