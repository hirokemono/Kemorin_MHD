!
!      module set_ctl_diff_udt
!
!     Written by H. Matsui on July, 2006
!     Modified by H. Matsui on JUne, 2007
!
!!      subroutine set_ctl_params_correlate_udt                         &
!!     &         (d_plt, org_d_plt, diff_ctl,                           &
!!     &          mesh_file, udt_org_param, nod_fld, time_U)
!!        type(platform_data_control), intent(in) :: d_plt
!!        type(platform_data_control), intent(in) :: org_d_plt
!!        type(diff_model_ctl), intent(in) :: diff_ctl
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!        type(field_IO_params), intent(inout) :: udt_org_param
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(time_step_param), intent(inout) :: time_U
!!      subroutine set_ctl_params_diff_udt                              &
!!     &         (d_plt, org_d_plt, diff_ctl, mesh_file, udt_org_param)
!!        type(platform_data_control), intent(in) :: d_plt
!!        type(platform_data_control), intent(in) :: org_d_plt
!!        type(diff_model_ctl), intent(in) :: diff_ctl
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!        type(field_IO_params), intent(inout) :: udt_org_param
!!      subroutine s_set_ctl_4_diff_udt_steps(tctl, time_U)
!!        type(time_data_control), intent(in) :: tctl
!!        type(time_step_param), intent(inout) :: time_U
!
      module set_ctl_diff_udt
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_ctl_params_4_diff_udt
      use t_step_parameter
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
     &         (d_plt, org_d_plt, diff_ctl,                             &
     &          mesh_file, udt_org_param, nod_fld, time_U)
!
      use t_ctl_data_diff_udt
      use set_control_nodal_data
      use set_control_ele_layering
!
      type(platform_data_control), intent(in) :: d_plt
      type(platform_data_control), intent(in) :: org_d_plt
      type(diff_model_ctl), intent(in) :: diff_ctl
!
      type(field_IO_params), intent(inout) ::  mesh_file
      type(field_IO_params), intent(inout) :: udt_org_param
      type(phys_data), intent(inout) :: nod_fld
      type(time_step_param), intent(inout) :: time_U
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_diff_udt'
      call set_ctl_params_diff_udt(d_plt, org_d_plt, diff_ctl,          &
     &    mesh_file, udt_org_param)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_control_ele_layering'
      call s_set_control_ele_layering(diff_ctl%elayer_d_ctl)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_control_nodal_data'
      call s_set_control_nodal_data                                     &
     &   (diff_ctl%fld_d_ctl%field_ctl, nod_fld, ierr)
      if (ierr .ne. 0) call calypso_MPI_abort(ierr, e_message)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_ctl_4_diff_udt_steps'
      call s_set_ctl_4_diff_udt_steps(diff_ctl%t_d_ctl, time_U)
!
      end subroutine set_ctl_params_correlate_udt
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_diff_udt                                &
     &         (d_plt, org_d_plt, diff_ctl, mesh_file, udt_org_param)
!
      use t_field_data_IO
      use t_ctl_data_diff_udt
      use m_geometry_constants
      use m_file_format_switch
      use m_default_file_prefix
      use set_ctl_parallel_platform
      use set_control_platform_data
      use ucd_IO_select
      use skip_comment_f
!
      type(platform_data_control), intent(in) :: d_plt
      type(platform_data_control), intent(in) :: org_d_plt
      type(diff_model_ctl), intent(in) :: diff_ctl
!
      type(field_IO_params), intent(inout) ::  mesh_file
      type(field_IO_params), intent(inout) :: udt_org_param
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
!   set field data name
!
      if(diff_ctl%ref_udt_head_ctl%iflag .ne. 0) then
        first_ucd_param%file_prefix                                     &
     &       = diff_ctl%ref_udt_head_ctl%charavalue
      else
        first_ucd_param%file_prefix = ref_udt_file_head
      end if
      first_ucd_param%iflag_format = udt_org_param%iflag_format
!
      if(diff_ctl%tgt_udt_head_ctl%iflag .ne. 0) then
        second_ucd_param%file_prefix                                    &
     &        = diff_ctl%tgt_udt_head_ctl%charavalue
      else
        second_ucd_param%file_prefix = tgt_udt_file_head
      end if
      second_ucd_param%iflag_format = udt_org_param%iflag_format
!
      grouping_mesh_head =  "grouping_mesh"
      if(diff_ctl%group_mesh_head_ctl%iflag .ne. 0) then
        grouping_mesh_head = diff_ctl%group_mesh_head_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'grouping_mesh_head: ', trim(grouping_mesh_head)
      end if
!
!   field setting
!
      if (d_plt%field_file_prefix%iflag .ne. 0) then
        diff_ucd_param%file_prefix = d_plt%field_file_prefix%charavalue
        ave_ucd_param%file_prefix =  d_plt%field_file_prefix%charavalue
      else
        diff_ucd_param%file_prefix = diff_udt_file_head
        ave_ucd_param%file_prefix =  ave_udt_file_head
      end if
      diff_ucd_param%iflag_format = udt_org_param%iflag_format
      ave_ucd_param%iflag_format =  udt_org_param%iflag_format
!
      product_field_name = "velocity"
      if(diff_ctl%product_field_ctl%iflag .ne. 0) then
        product_field_name = diff_ctl%product_field_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'product_field_name ', trim(product_field_name)
      end if
!
      correlate_field_name =  "velocity"
      if(diff_ctl%correlate_fld_ctl%iflag .ne. 0) then
        correlate_field_name = diff_ctl%correlate_fld_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'correlate_field_name ', trim(correlate_field_name)
      end if
!
      correlate_comp_name =  "norm"
      if(diff_ctl%correlate_cmp_ctl%iflag .ne. 0) then
        correlate_comp_name = diff_ctl%correlate_cmp_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'correlate_comp_name ', trim(correlate_comp_name)
      end if
!
      tmpchara = "Cartesian"
      if(diff_ctl%correlate_coord_ctl%iflag .ne. 0) then
        tmpchara = diff_ctl%correlate_coord_ctl%charavalue
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
      subroutine s_set_ctl_4_diff_udt_steps(tctl, time_U)
!
      use m_error_IDs
      use t_ctl_data_4_time_steps
      use cal_num_digits
!
      type(time_data_control), intent(in) :: tctl
      type(time_step_param), intent(inout) :: time_U
!
      integer(kind = kint) :: ierr
!
!   parameters for time evolution
!
      call set_fixed_time_step_params                                   &
     &         (tctl, time_U, ierr, e_message)
!
      time_U%time_d%dt = 1.0d0
      if (tctl%dt_ctl%iflag .gt. 0) then
        time_U%time_d%dt = tctl%dt_ctl%realvalue
      end if
!
      if (iflag_debug.eq.1) then
        write(*,*) 'i_step_init ',       time_U%init_d%i_time_step
        write(*,*) 'i_step_number ',     time_U%finish_d%i_end_step
        write(*,*) 'i_step_output_ucd ', time_U%ucd_step%increment
      end if
!
      end subroutine s_set_ctl_4_diff_udt_steps
!
! -----------------------------------------------------------------------
!
      end module set_ctl_diff_udt
