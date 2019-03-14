!
!      module set_control_assemble
!      Written by H. Matsui
!
!!      subroutine set_control_4_merge(mgd_ctl, asbl_param, num_pe)
!!      subroutine set_control_4_newrst                                 &
!!     &         (num_pe, mgd_ctl, asbl_param, ierr)
!!      subroutine set_control_4_newudt                                 &
!!     &         (num_pe, mgd_ctl, asbl_param, ierr)
!!        type(control_data_4_merge), intent(in) :: mgd_ctl
!!        type(control_param_assemble), intent(inout) :: asbl_param
!!      subroutine set_assemble_field_list(mgd_ctl, asbl_tbl)
!!        type(control_data_4_merge), intent(in) :: mgd_ctl
!!        type(assemble_field_list), intent(inout) :: asbl_tbl
!
      module set_control_assemble
!
      use m_precision
      use m_machine_parameter
      use t_file_IO_parameter
      use t_control_param_assemble
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
       subroutine set_control_4_merge(mgd_ctl, asbl_param, num_pe)
!
      use t_file_IO_parameter
!
      use m_file_format_switch
      use m_field_file_format
!
      use t_control_data_4_merge
      use set_parallel_file_name
      use set_control_platform_data
      use ucd_IO_select
      use parallel_ucd_IO_select
!
      type(control_data_4_merge), intent(in) :: mgd_ctl
      type(control_param_assemble), intent(inout) :: asbl_param
      integer, intent(inout) :: num_pe
!
!
      if (mgd_ctl%source_plt%ndomain_ctl%iflag .gt. 0) then
        num_pe = int(mgd_ctl%source_plt%ndomain_ctl%intvalue)
      else
        write(*,*) 'Set number of subdomains'
        stop
      end if
!
      call set_control_mesh_def                                         &
     &   (mgd_ctl%source_plt, asbl_param%org_mesh_file)
!
      end subroutine set_control_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_newrst                                   &
     &         (num_pe, mgd_ctl, asbl_param, ierr)
!
      use t_control_data_4_merge
      use m_file_format_switch
      use m_default_file_prefix
      use set_control_platform_data
!
      integer, intent(in) :: num_pe
      type(control_data_4_merge), intent(in) :: mgd_ctl
      type(control_param_assemble), intent(inout) :: asbl_param
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      if(mgd_ctl%assemble_plt%ndomain_ctl%iflag .eq. 0) then
        write(*,*) 'Set number of subdomains for new grid'
        ierr = 1
        return
      end if
      if(mgd_ctl%assemble_plt%ndomain_ctl%intvalue .ne. num_pe) then
        ierr = 1
        return
      end if
!
      call set_control_mesh_file_def(def_new_mesh_head,                 &
     &    mgd_ctl%assemble_plt, asbl_param%new_mesh_file)
!
      call set_delete_flag_4_assemble(mgd_ctl%assemble_plt, asbl_param)
!
!
      call set_assemble_rst_file_param                                  &
     &   (mgd_ctl%source_plt, mgd_ctl%assemble_plt, asbl_param)
!
      call set_magnetic_ratio_4_assemble                                &
     &   (mgd_ctl%magnetic_ratio_ctl, asbl_param)
!
      call set_assemble_step_4_rst(mgd_ctl%t_mge_ctl, asbl_param)
!
      end subroutine set_control_4_newrst
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_newudt                                   &
     &         (num_pe, mgd_ctl, asbl_param, ierr)
!
      use t_control_data_4_merge
      use m_file_format_switch
      use m_default_file_prefix
      use skip_comment_f
      use set_control_platform_data
!
      integer, intent(in) :: num_pe
      type(control_data_4_merge), intent(in) :: mgd_ctl
      type(control_param_assemble), intent(inout) :: asbl_param
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      if(mgd_ctl%assemble_plt%ndomain_ctl%iflag .eq. 0) then
        write(*,*) 'Set number of subdomains for new grid'
        ierr = 1
        return
      end if
      if(mgd_ctl%assemble_plt%ndomain_ctl%intvalue .ne. num_pe) then
        ierr = 1
        return
      end if
!
      call set_control_mesh_file_def(def_new_mesh_head,                 &
     &    mgd_ctl%assemble_plt, asbl_param%new_mesh_file)
      call set_assemble_ucd_file_param                                  &
     &   (mgd_ctl%source_plt, mgd_ctl%assemble_plt, asbl_param)
!
      call set_delete_flag_4_assemble(mgd_ctl%assemble_plt, asbl_param)
!
      call set_assemble_step_4_ucd(mgd_ctl%t_mge_ctl, asbl_param)
!
      end subroutine set_control_4_newudt
!
! -----------------------------------------------------------------------
!
       subroutine set_assemble_field_list(mgd_ctl, asbl_tbl)
!
      use t_control_data_4_merge
      use assemble_nodal_fields
!
      type(control_data_4_merge), intent(in) :: mgd_ctl
      type(assemble_field_list), intent(inout) :: asbl_tbl
!
      integer(kind = kint) :: i, icou
      character(len = kchara) :: tmpchara
!
!
       asbl_tbl%nfld_label = 0
       do i = 1, mgd_ctl%fld_mge_ctl%field_ctl%num
         tmpchara = mgd_ctl%fld_mge_ctl%field_ctl%c2_tbl(i)
         if(cmp_no_case(tmpchara,'Viz_On')) then
           asbl_tbl%nfld_label = asbl_tbl%nfld_label + 1
         end if
       end do
!
       call alloc_assemble_field_list(asbl_tbl)
!
       icou = 0
       do i = 1, mgd_ctl%fld_mge_ctl%field_ctl%num
         tmpchara = mgd_ctl%fld_mge_ctl%field_ctl%c2_tbl(i)
         if(cmp_no_case(tmpchara,'Viz_On')) then
           icou = icou + 1
           asbl_tbl%ucd_on_label(icou)                                  &
     &          = mgd_ctl%fld_mge_ctl%field_ctl%c1_tbl(i)
         end if
       end do
!
       if(iflag_debug .gt. 0) then
         write(*,*) 'ucd_on_label', asbl_tbl%nfld_label
         do i = 1, asbl_tbl%nfld_label
           write(*,*) i, trim(asbl_tbl%ucd_on_label(i))
         end do
       end if
!
      end subroutine set_assemble_field_list
!
! -----------------------------------------------------------------------
!
      end module set_control_assemble
