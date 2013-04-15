!
!      module set_ctl_diff_udt
!
      module set_ctl_diff_udt
!
!     Written by H. Matsui on July, 2006
!     Modified by H. Matsui on JUne, 2007
!
      use m_parallel_var_dof
      use m_machine_parameter
      use m_ctl_params_4_diff_udt
!
      use m_precision
!
      implicit none
!
!      subroutine set_ctl_params_correlate_udt
!      subroutine set_ctl_params_diff_udt
!      subroutine s_set_ctl_4_diff_udt_steps
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_correlate_udt
!
      use m_ctl_data_4_fem_int_pts
      use m_fem_gauss_int_coefs
      use set_control_nodal_data
      use set_control_ele_layering
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_diff_udt'
      call set_ctl_params_diff_udt
!
      if (iflag_debug.eq.1) write(*,*) 's_set_control_ele_layering'
      call s_set_control_ele_layering
!
      if (iflag_debug.eq.1) write(*,*) 's_set_control_nodal_data'
      call s_set_control_nodal_data(ierr)
      if (ierr .ne. 0) call parallel_abort(ierr, e_message)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_ctl_4_diff_udt_steps'
      call s_set_ctl_4_diff_udt_steps
!
      if(i_intgration_points .gt. 0) then
        max_int_point = integration_points_ctl
      end if
!
      end subroutine set_ctl_params_correlate_udt
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_diff_udt
!
      use m_read_mesh_data
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
      use m_ctl_data_diff_udt
      use set_control_platform_data
      use set_control_4_2nd_files
      use m_file_format_switch
      use m_ucd_data
!
!
      if (nprocs .ne. num_subdomain_ctl) then
        write(e_message,*) 'Number of processes should be num. of mesh'
        call  parallel_abort(4000, e_message)
      end if
!
!      write(*,*) 'set_control_smp_def'
      call set_control_smp_def
!      write(*,*) 'set_control_mesh_def'
      call set_control_mesh_def
      call set_control_org_field_file_def
!
!
!   set data format
!
      call choose_ucd_file_format(udt_file_fmt_ctl, i_udt_files_fmt,    &
     &    itype_ucd_data_file)
!
!   set field data name
!
      if (i_ref_udt_head_ctl .ne. 0) then
        ref_udt_file_head = ref_udt_head_ctl
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'ref_udt_file_head: ', trim(ref_udt_file_head)
      end if
!
      if (i_tgt_udt_head_ctl .ne. 0) then
        tgt_udt_file_head = tgt_udt_head_ctl
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'tgt_udt_file_head: ', trim(tgt_udt_file_head)
      end if
!
      if (i_group_mesh_head .ne. 0) then
        grouping_mesh_head = group_mesh_head_ctl
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'grouping_mesh_head: ', trim(grouping_mesh_head)
      end if
!
!   field setting
!
      if (i_udt_header .ne. 0) then
        diff_udt_file_head = udt_file_head_ctl
        ave_udt_file_head =  udt_file_head_ctl
        prod_udt_file_head = udt_file_head_ctl
      else
        diff_udt_file_head = "field_diff/out"
        ave_udt_file_head =  "out_average"
        prod_udt_file_head = "field_new/out"
      end if
!
      if (i_prod_name .ne. 0) then
        product_field_name = product_field_ctl
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'product_field_name ', trim(product_field_name)
      end if
!
      if (i_corr_fld_name .ne. 0) then
        correlate_field_name = correlate_fld_ctl
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'correlate_field_name ', trim(correlate_field_name)
      end if
!
      if (i_corr_cmp_name .ne. 0) then
        correlate_comp_name = correlate_cmp_ctl
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'correlate_comp_name ', trim(correlate_comp_name)
      end if
!
      if (i_correlate_coord .ne. 0) then
        if     (correlate_coord_ctl .eq. 'cartesian'                    &
     &     .or. correlate_coord_ctl .eq. 'Cartesian'                    &
     &     .or. correlate_coord_ctl .eq. 'CARTESIAN'                    &
     &     .or. correlate_coord_ctl .eq. 'xyz'                          &
     &     .or. correlate_coord_ctl .eq. 'XYZ') then
          iflag_correlate_coord = 0
        else if(correlate_coord_ctl .eq. 'spherical'                    &
     &     .or. correlate_coord_ctl .eq. 'Spherical'                    &
     &     .or. correlate_coord_ctl .eq. 'SPHERICAL'                    &
     &     .or. correlate_coord_ctl .eq. 'rtp'                          &
     &     .or. correlate_coord_ctl .eq. 'RTP') then
          iflag_correlate_coord = 1
        else if(correlate_coord_ctl .eq. 'cyrindrical'                  &
     &     .or. correlate_coord_ctl .eq. 'Cyrindrical'                  &
     &     .or. correlate_coord_ctl .eq. 'CYRINDRICAL'                  &
     &     .or. correlate_coord_ctl .eq. 'spz'                          &
     &     .or. correlate_coord_ctl .eq. 'SPZ') then
          iflag_correlate_coord = 2
        end if
      else
        iflag_correlate_coord = 0
      end if
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'iflag_correlate_coord ', iflag_correlate_coord
!
!
      end subroutine set_ctl_params_diff_udt
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_ctl_4_diff_udt_steps
!
      use m_t_step_parameter
      use m_t_int_parameter
      use m_ctl_data_4_time_steps
      use cal_num_digits
!
!
!   parameters for time evolution
!
        if (i_i_step_init.eq.0) then
          i_step_init   = 0
        else
          i_step_init   = i_step_init_ctl
        end if
!
        if (i_i_step_number.eq.0) then
          e_message = 'Set step number to finish'
            call parallel_abort(90, e_message)
        else
          i_step_number = i_step_number_ctl
        end if
!
        if (i_i_step_ucd.eq.0) then
          i_step_output_ucd = 1
        else
          i_step_output_ucd = i_step_ucd_ctl
        end if
!
        if (i_i_diff_steps.eq.0) then
          i_diff_steps = 1
        else
          i_diff_steps = i_diff_steps_ctl
        end if
!
        if (i_dt.eq.0) then
          dt = 1.0d0
        else
          dt = dt_ctl
        end if
!
        if (dt .eq. 0.0d0) then
          ddt = 1.0d30
        else
          ddt = 1.0d0 / dt
        end if
!
        if (iflag_debug.eq.1) then
          write(*,*) 'i_step_init ',       i_step_init
          write(*,*) 'i_step_number ',     i_step_number
          write(*,*) 'i_step_output_ucd ', i_step_output_ucd
          write(*,*) 'i_diff_steps ',      i_diff_steps
        end if
!
      end subroutine s_set_ctl_4_diff_udt_steps
!
! -----------------------------------------------------------------------
!
      end module set_ctl_diff_udt
