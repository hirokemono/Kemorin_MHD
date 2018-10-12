!
!      module m_control_param_merge
!      Written by H. Matsui
!
!      subroutine deallocate_control_4_merge
!
!!      subroutine set_control_4_merge(mgd_ctl, asbl_param, num_pe)
!!      subroutine set_control_4_newrst                                 &
!!     &         (nprocs, mgd_ctl, asbl_param, ierr)
!!      subroutine set_control_4_newudt                                 &
!!     &         (nprocs, mgd_ctl, asbl_param, ierr)
!!        type(control_data_4_merge), intent(in) :: mgd_ctl
!!        type(control_param_assemble), intent(inout) :: asbl_param
!
      module m_control_param_merge
!
      use m_precision
      use m_machine_parameter
      use t_file_IO_parameter
      use t_control_param_assemble
!
      implicit    none
!
!
      type(field_IO_params), save :: merge_org_mesh_file
      type(field_IO_params), save :: merged_mesh_file
!
      type(field_IO_params), save :: org_fst_param
      type(field_IO_params), save :: new_fst_param
!
      type(field_IO_params), save :: original_ucd_param
      type(field_IO_params), save :: assemble_ucd_param
!
      integer(kind=kint ) :: num_nod_phys
!
      character(len=kchara), dimension(:), allocatable :: ucd_on_label
!       setting for merged data
!
      character(len=kchara), parameter                                  &
     &      :: def_new_udt_head = 'field_new/out'
!
      character(len=kchara), parameter                                  &
     &                      :: org_rst_def_head =   "restart/rst"
      character(len=kchara), parameter                                  &
     &                      :: new_rst_def_head =   "rst_new/rst"
!
      private :: def_new_udt_head
      private :: org_rst_def_head, new_rst_def_head
!
      private :: allocate_control_4_merge
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_control_4_merge
!
!
       allocate ( ucd_on_label(num_nod_phys) )
!
      end subroutine allocate_control_4_merge
!
!------------------------------------------------------------------
!
      subroutine deallocate_control_4_merge
!
       deallocate ( ucd_on_label )
!
      end subroutine deallocate_control_4_merge
!
!------------------------------------------------------------------
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
      integer(kind = kint), intent(inout) :: num_pe
!
      integer(kind = kint) :: i, icou
      character(len = kchara) :: tmpchara
!
!
      if (mgd_ctl%source_plt%ndomain_ctl%iflag .gt. 0) then
        num_pe = mgd_ctl%source_plt%ndomain_ctl%intvalue
      else
        write(*,*) 'Set number of subdomains'
        stop
      end if
!
      call set_control_mesh_def                                         &
     &   (mgd_ctl%source_plt, merge_org_mesh_file)
!
      call set_merged_ucd_file_define                                   &
     &   (mgd_ctl%source_plt, original_ucd_param)
      call set_merged_ucd_file_ctl                                      &
     &   (def_new_udt_head, mgd_ctl%assemble_plt%field_file_prefix,     &
     &    mgd_ctl%assemble_plt%field_file_fmt_ctl, assemble_ucd_param)
!
!
       num_nod_phys = 0
       do i = 1, mgd_ctl%fld_mge_ctl%field_ctl%num
         tmpchara = mgd_ctl%fld_mge_ctl%field_ctl%c2_tbl(i)
         if(cmp_no_case(tmpchara,'Viz_On')) then
           num_nod_phys = num_nod_phys + 1
         end if
       end do
!
       call allocate_control_4_merge
!
       icou = 0
       do i = 1, mgd_ctl%fld_mge_ctl%field_ctl%num
         tmpchara = mgd_ctl%fld_mge_ctl%field_ctl%c2_tbl(i)
         if(cmp_no_case(tmpchara,'Viz_On')) then
           icou = icou + 1
           ucd_on_label(icou) = mgd_ctl%fld_mge_ctl%field_ctl%c1_tbl(i)
         end if
       end do
!
       if(iflag_debug .gt. 0) then
         write(*,*) 'ucd_on_label', num_nod_phys
         do i = 1, num_nod_phys
           write(*,*) i, trim(ucd_on_label(i))
         end do
       end if
!
      call set_assemble_step_4_ucd(mgd_ctl%t_mge_ctl, asbl_param)
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &          'istep_start, istep_end, increment_step',               &
     &           asbl_param%istep_start, asbl_param%istep_end,          &
     &           asbl_param%increment_step
!
      end subroutine set_control_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_newrst                                   &
     &         (nprocs, mgd_ctl, asbl_param, ierr)
!
      use t_control_data_4_merge
      use m_file_format_switch
      use set_control_platform_data
!
      integer(kind = kint), intent(in) :: nprocs
      type(control_data_4_merge), intent(in) :: mgd_ctl
      type(control_param_assemble), intent(inout) :: asbl_param
      integer(kind = kint), intent(inout) :: ierr
!
!
      call set_control_4_newudt(nprocs, mgd_ctl, asbl_param, ierr)
      if(ierr .gt. 0) return
!
      call set_parallel_file_ctl_params(org_rst_def_head,               &
     &    mgd_ctl%source_plt%restart_file_prefix,                       &
     &    mgd_ctl%source_plt%restart_file_fmt_ctl, org_fst_param)
      call set_parallel_file_ctl_params(new_rst_def_head,               &
     &    mgd_ctl%assemble_plt%restart_file_prefix,                     &
     &    mgd_ctl%assemble_plt%restart_file_fmt_ctl, new_fst_param)
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
     &         (nprocs, mgd_ctl, asbl_param, ierr)
!
      use t_control_data_4_merge
      use m_file_format_switch
      use m_default_file_prefix
      use skip_comment_f
      use set_control_platform_data
!
      integer(kind = kint), intent(in) :: nprocs
      type(control_data_4_merge), intent(in) :: mgd_ctl
      type(control_param_assemble), intent(inout) :: asbl_param
      integer(kind = kint), intent(inout) :: ierr
!
      character(len = kchara) :: tmpchara
!
!
      ierr = 0
      if(mgd_ctl%assemble_plt%ndomain_ctl%iflag .eq. 0) then
        write(*,*) 'Set number of subdomains for new grid'
        ierr = 1
        return
      end if
      if(mgd_ctl%assemble_plt%ndomain_ctl%intvalue .ne. nprocs) then
        ierr = 1
        return
      end if
!
      call set_control_mesh_file_def                                    &
     &   (def_new_mesh_head, mgd_ctl%assemble_plt, merged_mesh_file)
!
      call set_delete_flag_4_assemble(mgd_ctl%assemble_plt, asbl_param)
!
      end subroutine set_control_4_newudt
!
! -----------------------------------------------------------------------
!
      end module m_control_param_merge
