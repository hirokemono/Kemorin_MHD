!
!      module m_control_param_merge
!      Written by H. Matsui
!
!      subroutine deallocate_control_4_merge
!
!      subroutine set_control_4_merge(ucd)
!        type(ucd_data), intent(inout) :: ucd
!      subroutine set_control_4_newrst
!
!      subroutine set_control_4_newudt
!
      module m_control_param_merge
!
      use m_precision
      use t_file_IO_parameter
!
      implicit    none
!
!
      integer(kind=kint ), parameter ::  id_merged_ucd = 16
!
      type(field_IO_params), save :: merge_org_mesh_file
!
      type(field_IO_params), save :: merged_mesh_file
!merged_mesh_file%iflag_format
!
      character(len=kchara) :: org_rst_head
      character(len=kchara) :: new_rst_head
!
!
      integer(kind=kint ) :: istep_start, istep_end, increment_step
!
      integer(kind=kint ) :: num_nod_phys
!
      character(len=kchara), dimension(:), allocatable :: ucd_on_label
!       setting for merged data
!
      character(len=kchara), parameter                                  &
     &      :: def_newmesh_head = 'mesh_new/in'
      character(len=kchara), parameter                                  &
     &      :: def_merged_mesh_header = 'in_all'
!
      character(len=kchara), parameter                                  &
     &      :: def_new_udt_head = 'field_new/out'
!
      character(len=kchara), parameter                                  &
     &      :: def_org_udt_head =    'field/out'
      character(len=kchara), parameter                                  &
     &      :: def_dx_node_file_name =    'node_info.dat'
      character(len=kchara), parameter                                  &
     &      :: def_dx_connect_file_name = 'connect_info.dat'
      character(len=kchara), parameter                                  &
     &      :: def_dx_data_head = 'field/field_dx'
!
      character(len=kchara), parameter                                  &
     &                      :: org_rst_def_head =   "restart/rst"
      character(len=kchara), parameter                                  &
     &                      :: new_rst_def_head =   "rst_new/rst"
!
      character(len=kchara) :: udt_original_header = def_org_udt_head
      integer(kind = kint) :: itype_org_ucd_file
!
!
      character(len=kchara) :: merged_data_head = 'outall'
      character(len=kchara) :: new_udt_head
      integer(kind = kint) :: itype_assembled_data
!
      character(len=kchara) :: dx_node_fname
      character(len=kchara) :: dx_connect_fname
!
      integer(kind=kint ) :: iorg_mesh_file_fmt = 0
      integer(kind=kint ) :: iorg_rst_file_fmt =  0
!
      integer(kind=kint ) :: inew_rst_file_fmt =  0
!
      integer(kind=kint ) :: iflag_delete_org = 0
!
      real(kind = kreal) :: b_ratio
!
      private :: def_org_udt_head, def_merged_mesh_header
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
       subroutine set_control_4_merge(ucd)
!
      use t_ucd_data
!
      use m_geometry_data_4_merge
      use m_file_format_switch
      use m_field_file_format
!
      use m_control_data_4_merge
      use set_parallel_file_name
      use set_control_platform_data
      use ucd_IO_select
!
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind=kint ) :: i, icou
!
!
      if (source_plt%ndomain_ctl%iflag .gt. 0) then
        num_pe = source_plt%ndomain_ctl%intvalue
      else
        write(*,*) 'Set number of subdomains'
        stop
      end if
!
      call set_control_mesh_def(source_plt, merge_org_mesh_file)
!
      call set_ucd_file_define(source_plt, ucd)
!
      if(source_plt%field_file_prefix%iflag .gt. 0)                     &
     &    udt_original_header = source_plt%field_file_prefix%charavalue
      call choose_ucd_file_format                                       &
     &   (source_plt%field_file_fmt_ctl%charavalue,                     &
     &    source_plt%field_file_fmt_ctl%iflag, itype_org_ucd_file)
!
!
      if (assemble_plt%field_file_prefix%iflag .gt. 0) then
        new_udt_head =     assemble_plt%field_file_prefix%charavalue
        merged_data_head = assemble_plt%field_file_prefix%charavalue
      else
        new_udt_head = def_new_udt_head
      end if
      call choose_ucd_file_format                                       &
     &   (assemble_plt%field_file_fmt_ctl%charavalue,                   &
     &    assemble_plt%field_file_fmt_ctl%iflag, itype_assembled_data)
!
!
       num_nod_phys = 0
       do i = 1, fld_mge_ctl%field_ctl%num
         if(cmp_no_case(fld_mge_ctl%field_ctl%c2_tbl(i),'Viz_On')) then
           num_nod_phys = num_nod_phys + 1
         end if
       end do
!
       call allocate_control_4_merge
!
       icou = 0
       do i = 1, fld_mge_ctl%field_ctl%num
         if(cmp_no_case(fld_mge_ctl%field_ctl%c2_tbl(i),'Viz_On')) then
           icou = icou + 1
           ucd_on_label(icou) = fld_mge_ctl%field_ctl%c1_tbl(i)
         end if
       end do
!
       write(*,*) 'ucd_on_label', num_nod_phys
       do i = 1, num_nod_phys
         write(*,*) i, trim(ucd_on_label(i))
       end do
!
      istep_start = 1
      if(t_mge_ctl%i_step_init_ctl%iflag .gt. 0) then
        istep_start = t_mge_ctl%i_step_init_ctl%intvalue
      end if
!
      istep_end = 1
      if(t_mge_ctl%i_step_number_ctl%iflag .gt. 0) then
        istep_end = t_mge_ctl%i_step_number_ctl%intvalue
      end if
!
      increment_step = 1
      if(t_mge_ctl%i_step_ucd_ctl%iflag .gt. 0) then
        increment_step = t_mge_ctl%i_step_ucd_ctl%intvalue
      end if
!
      end subroutine set_control_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_newrst
!
      use m_control_data_4_merge
      use m_geometry_data_4_merge
      use m_2nd_geometry_4_merge
      use m_file_format_switch
!
!
      call set_control_4_newudt
!
!
      if (source_plt%restart_file_prefix%iflag .gt. 0) then
        org_rst_head = source_plt%restart_file_prefix%charavalue
      else
        org_rst_head = org_rst_def_head
      end if
!
      if(assemble_plt%restart_file_prefix%iflag .gt. 0) then
        new_rst_head = assemble_plt%restart_file_prefix%charavalue
      else
        new_rst_head = new_rst_def_head
      end if
!
      call choose_para_file_format                                      &
     &   (source_plt%restart_file_fmt_ctl, iorg_rst_file_fmt)
      call choose_para_file_format                                      &
     &   (assemble_plt%restart_file_fmt_ctl, inew_rst_file_fmt)
!
!
      if (magnetic_ratio_ctl%iflag .gt. 0) then
        b_ratio = magnetic_ratio_ctl%realvalue
      else
        b_ratio = 1.0d0
      end if
!
      increment_step = 1
      if (t_mge_ctl%i_step_rst_ctl%iflag .gt. 0) then
        increment_step = t_mge_ctl%i_step_rst_ctl%intvalue
      end if
!
      end subroutine set_control_4_newrst
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_newudt
!
      use m_control_data_4_merge
      use m_ctl_data_4_platforms
      use m_2nd_geometry_4_merge
      use m_file_format_switch
      use m_default_file_prefix
      use skip_comment_f
      use set_control_platform_data
!
!
      if (assemble_plt%ndomain_ctl%iflag .gt. 0) then
        num_pe2 = assemble_plt%ndomain_ctl%intvalue
      else
        write(*,*) 'Set number of subdomains for new grid'
        stop
      end if
!
      call set_control_mesh_file_def                                    &
     &   (def_new_mesh_head, assemble_plt, merged_mesh_file)
!
      if(assemble_plt%del_org_data_ctl%iflag .gt. 0) then
        if(yes_flag(assemble_plt%del_org_data_ctl%charavalue)) then
          iflag_delete_org = 1
        end if
      end if
!
      end subroutine set_control_4_newudt
!
! -----------------------------------------------------------------------
!
      end module m_control_param_merge
