!
!      module m_control_param_merge
!      Written by H. Matsui
!
!      subroutine deallocate_control_4_merge
!
!      subroutine set_control_4_merge
!      subroutine set_control_4_merged_mesh
!      subroutine set_control_4_newrst
!
!      subroutine set_control_4_newudt
!
      module m_control_param_merge
!
      use m_precision
!
      implicit    none
!
!
      integer(kind=kint ), parameter ::  id_merged_ucd = 16
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
      character(len=kchara) :: new_mesh_head
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
      integer(kind=kint ) :: inew_mesh_file_fmt = 0
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
       subroutine set_control_4_merge
!
      use m_geometry_data_4_merge
      use m_read_mesh_data
      use m_file_format_switch
      use m_field_file_format
!
      use m_control_data_4_merge
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
      use m_ctl_data_4_fields
      use m_ctl_data_4_time_steps
      use m_node_id_spherical_IO
      use set_parallel_file_name
      use set_control_platform_data
      use m_ucd_data
!
      integer(kind=kint ) :: i, icou
!
!
      if (i_num_subdomain .gt. 0) then
        num_pe = num_subdomain_ctl
      else
        write(*,*) 'Set number of subdomains'
        stop
      end if
!
      call set_control_mesh_def
      iorg_mesh_file_fmt = iflag_mesh_file_fmt
!
      call set_control_data_file_def
      udt_original_header = ucd_header_name
!
      call choose_ucd_file_format(udt_file_fmt_ctl, i_udt_files_fmt,    &
     &    itype_org_ucd_file)
!
!
      if (i_new_vtk_head .gt. 0) then
        itype_assembled_data = iflag_vtk
        merged_data_head = new_vtk_head_ctl
      end if
!
      if (i_new_udt_head .gt. 0) then
        new_udt_head =   new_udt_head_ctl
        merged_data_head = new_udt_head_ctl
      else
        new_udt_head = def_new_udt_head
      end if
      call choose_ucd_file_format(new_udt_file_fmt_ctl,                 &
     &    i_new_udt_files_fmt, itype_assembled_data)
!
!
       num_nod_phys = 0
       do i = 1, num_nod_phys_ctl
         if (visualize_ctl(i) .eq. 'Viz_On') then
           num_nod_phys = num_nod_phys + 1
         end if
       end do
!
       call allocate_control_4_merge
!
       icou = 0
       do i = 1, num_nod_phys_ctl
         if ( visualize_ctl(i) .eq. 'Viz_On' ) then
           icou = icou + 1
           ucd_on_label(icou) = phys_nod_name_ctl(i)
         end if
       end do
!
       write(*,*) 'ucd_on_label', num_nod_phys
       do i = 1, num_nod_phys
         write(*,*) i, trim(ucd_on_label(i))
       end do
!
       istep_start = i_step_init_ctl
       istep_end = i_step_number_ctl
       increment_step = i_step_ucd_ctl
!
      end subroutine set_control_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_merged_mesh
!
      use m_ctl_data_4_2nd_data
      use m_read_mesh_data
      use m_file_format_switch
!
!
      if (i_new_mesh_head .gt. 0) then
        new_mesh_head = new_mesh_head_ctl
      else
        new_mesh_head = def_merged_mesh_header
      end if
!
      call choose_file_format(new_mesh_file_fmt_ctl,                    &
     &     i_new_mesh_file_fmt, inew_mesh_file_fmt)
!
      end subroutine set_control_4_merged_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_newrst
!
      use m_control_data_4_merge
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
      use m_ctl_data_4_time_steps
      use m_geometry_data_4_merge
      use m_2nd_geometry_4_merge
      use m_field_data_IO
      use m_file_format_switch
!
!
      call set_control_4_newudt
!
!
      if (i_rst_header .gt. 0) then
        org_rst_head = rst_file_head_ctl
      else
        org_rst_head = org_rst_def_head
      end if
!
      if (i_new_rst_head .gt. 0) then
        new_rst_head = new_rst_head_ctl
      else
        new_rst_head = new_rst_def_head
      end if
!
      call choose_file_format(rst_files_fmt_ctl, i_rst_files_fmt,       &
     &    iorg_rst_file_fmt)
      call choose_file_format(new_rst_files_fmt_ctl,                    &
     &    i_new_rst_files_fmt, inew_rst_file_fmt)
!
!
      if (i_newrst_magne .gt. 0) then
        b_ratio = magnetic_field_ratio_ctl
      else
        b_ratio = 1.0d0
      end if
!
      increment_step = i_step_rst_ctl
!
      end subroutine set_control_4_newrst
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_newudt
!
      use m_control_data_4_merge
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
      use m_2nd_geometry_4_merge
      use m_file_format_switch
!
!
      if (i_num_new_domain .gt. 0) then
        num_pe2 = num_new_domain_ctl
      else
        write(*,*) 'Set number of subdomains for new grid'
        stop
      end if
!
      if (i_new_mesh_head .gt. 0) then
        new_mesh_head = new_mesh_head_ctl
      else
        new_mesh_head = def_newmesh_head
      end if
!
      call choose_file_format(new_mesh_file_fmt_ctl,                    &
     &     i_new_mesh_file_fmt, inew_mesh_file_fmt)
!
      if(i_del_org_data .gt. 0) then
        if(    del_org_data_ctl .eq. 'on'                               &
     &    .or. del_org_data_ctl .eq. 'On'                               &
     &    .or. del_org_data_ctl .eq. 'ON'                               &
     &    .or. del_org_data_ctl .eq. 'yes'                              &
     &    .or. del_org_data_ctl .eq. 'Yes'                              &
     &    .or. del_org_data_ctl .eq. 'YES') then
          iflag_delete_org = 1
        end if
      end if
!
      end subroutine set_control_4_newudt
!
! -----------------------------------------------------------------------
!
      end module m_control_param_merge
