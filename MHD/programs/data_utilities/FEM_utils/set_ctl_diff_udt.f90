!
!      module set_ctl_diff_udt
!
!     Written by H. Matsui on July, 2006
!     Modified by H. Matsui on JUne, 2007
!
!!      subroutine set_ctl_params_correlate_udt                         &
!!     &         (mesh_file, udt_org_param, nod_fld, ucd, ucd_step)
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!        type(field_IO_params), intent(inout) :: udt_org_param
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(ucd_data), intent(inout) :: ucd
!!        type(IO_step_param), intent(inout) :: ucd_step
!!      subroutine set_ctl_params_diff_udt                              &
!!     &         (mesh_file, udt_org_param, ucd)
!!      subroutine s_set_ctl_4_diff_udt_steps(tctl, ucd_step)
!!        type(time_data_control), intent(in) :: tctl
!!        type(IO_step_param), intent(inout) :: ucd_step
!
      module set_ctl_diff_udt
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_ctl_params_4_diff_udt
      use t_phys_data
      use t_file_IO_parameter
      use t_IO_step_parameter
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_correlate_udt                           &
     &         (mesh_file, udt_org_param, nod_fld, ucd, ucd_step)
!
      use t_ucd_data
      use m_ctl_data_diff_udt
      use m_fem_gauss_int_coefs
      use set_control_nodal_data
      use set_control_ele_layering
!
      type(field_IO_params), intent(inout) ::  mesh_file
      type(field_IO_params), intent(inout) :: udt_org_param
      type(phys_data), intent(inout) :: nod_fld
      type(ucd_data), intent(inout) :: ucd
      type(IO_step_param), intent(inout) :: ucd_step
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_diff_udt'
      call set_ctl_params_diff_udt(mesh_file, udt_org_param, ucd)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_control_ele_layering'
      call s_set_control_ele_layering(elayer_d_ctl)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_control_nodal_data'
      call s_set_control_nodal_data(fld_d_ctl%field_ctl, nod_fld, ierr)
      if (ierr .ne. 0) call calypso_MPI_abort(ierr, e_message)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_ctl_4_diff_udt_steps'
      call s_set_ctl_4_diff_udt_steps(t_d_ctl, ucd_step)
!
      if(fint_d_ctl%integration_points_ctl%iflag .gt. 0) then
        call maximum_integration_points                                 &
     &     (fint_d_ctl%integration_points_ctl%intvalue)
      end if
!
      end subroutine set_ctl_params_correlate_udt
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_diff_udt                                &
     &         (mesh_file, udt_org_param, ucd)
!
      use t_ucd_data
      use t_field_data_IO
      use m_ctl_data_diff_udt
      use m_geometry_constants
      use m_file_format_switch
      use m_default_file_prefix
      use set_ctl_parallel_platform
      use set_control_platform_data
      use ucd_IO_select
      use skip_comment_f
!
      type(field_IO_params), intent(inout) ::  mesh_file
      type(field_IO_params), intent(inout) :: udt_org_param
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: tmpchara
!
!
      call turn_off_debug_flag_by_ctl(my_rank, d_plt)
      call check_control_num_domains(d_plt)
      call set_control_smp_def(my_rank, d_plt)
      call set_control_mesh_def(d_plt, mesh_file)
      call set_control_mesh_file_def                                    &
     &   (def_org_ucd_header, org_d_plt, udt_org_param)
!
!
      call set_ucd_file_define(d_plt, ucd)
!
!   set field data name
!
      ref_udt_file_head = "field/out"
      if (ref_udt_head_ctl%iflag .ne. 0) then
        ref_udt_file_head = ref_udt_head_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'ref_udt_file_head: ', trim(ref_udt_file_head)
      end if
!
      tgt_udt_file_head = "field/out"
      if (tgt_udt_head_ctl%iflag .ne. 0) then
        tgt_udt_file_head = tgt_udt_head_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'tgt_udt_file_head: ', trim(tgt_udt_file_head)
      end if
!
      grouping_mesh_head =  "grouping_mesh"
      if (group_mesh_head_ctl%iflag .ne. 0) then
        grouping_mesh_head = group_mesh_head_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'grouping_mesh_head: ', trim(grouping_mesh_head)
      end if
!
!   field setting
!
      if (d_plt%field_file_prefix%iflag .ne. 0) then
        diff_udt_file_head = d_plt%field_file_prefix%charavalue
        ave_udt_file_head =  d_plt%field_file_prefix%charavalue
        prod_udt_file_head = d_plt%field_file_prefix%charavalue
      else
        diff_udt_file_head = "field_diff/out"
        ave_udt_file_head =  "out_average"
        prod_udt_file_head = "field_new/out"
      end if
!
      call choose_ucd_file_format(d_plt%field_file_fmt_ctl%charavalue,  &
     &    d_plt%field_file_fmt_ctl%iflag, ifmt_diff_udt_file)
!
      product_field_name = "velocity"
      if (product_field_ctl%iflag .ne. 0) then
        product_field_name = product_field_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'product_field_name ', trim(product_field_name)
      end if
!
      correlate_field_name =  "velocity"
      if (correlate_fld_ctl%iflag .ne. 0) then
        correlate_field_name = correlate_fld_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'correlate_field_name ', trim(correlate_field_name)
      end if
!
      correlate_comp_name =  "norm"
      if (correlate_cmp_ctl%iflag .ne. 0) then
        correlate_comp_name = correlate_cmp_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'correlate_comp_name ', trim(correlate_comp_name)
      end if
!
      tmpchara = "Cartesian"
      if (correlate_coord_ctl%iflag .ne. 0) then
        tmpchara = correlate_coord_ctl%charavalue
      end if
!
      if     (cmp_no_case(tmpchara, 'cartesian')                        &
     &   .or. cmp_no_case(tmpchara , 'xyz')) then
        iflag_correlate_coord = iflag_certecian
      else if(cmp_no_case(tmpchara, 'spherical')                        &
     &   .or. cmp_no_case(tmpchara, 'rtp')) then
        iflag_correlate_coord = iflag_spherical
      else if(cmp_no_case(tmpchara, 'cyrindrical')                      &
     &   .or. cmp_no_case(tmpchara, 'spz')) then
        iflag_correlate_coord = iflag_cylindrical
      end if
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'iflag_correlate_coord ', iflag_correlate_coord
!
!
      end subroutine set_ctl_params_diff_udt
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_ctl_4_diff_udt_steps(tctl, ucd_step)
!
      use m_t_step_parameter
      use m_error_IDs
      use t_ctl_data_4_time_steps
      use cal_num_digits
!
      type(time_data_control), intent(in) :: tctl
      type(IO_step_param), intent(inout) :: ucd_step
!
!   parameters for time evolution
!
        init_d1%i_time_step   = 0
        if (tctl%i_step_init_ctl%iflag .gt. 0) then
          init_d1%i_time_step   = tctl%i_step_init_ctl%intvalue
        end if
!
        if (tctl%i_step_number_ctl%iflag .eq. 0) then
          e_message = 'Set step number to finish'
            call calypso_MPI_abort(ierr_evo, e_message)
        else
          finish_d1%i_end_step = tctl%i_step_number_ctl%intvalue
        end if
!
        ucd_step%increment = 1
        if (tctl%i_step_ucd_ctl%iflag .gt. 0) then
          ucd_step%increment = tctl%i_step_ucd_ctl%intvalue
        end if
!
        i_diff_steps = 1
        if (tctl%i_diff_steps_ctl%iflag .gt. 0) then
          i_diff_steps = tctl%i_diff_steps_ctl%intvalue
        end if
!
        dt = 1.0d0
        if (tctl%dt_ctl%iflag .gt. 0) then
          dt = tctl%dt_ctl%realvalue
        end if
!
        if (iflag_debug.eq.1) then
          write(*,*) 'i_step_init ',       init_d1%i_time_step
          write(*,*) 'i_step_number ',     finish_d1%i_end_step
          write(*,*) 'i_step_output_ucd ', ucd_step%increment
          write(*,*) 'i_diff_steps ',      i_diff_steps
        end if
!
      end subroutine s_set_ctl_4_diff_udt_steps
!
! -----------------------------------------------------------------------
!
      end module set_ctl_diff_udt
