!t_ctl_param_newdom_filter.f90
!      module t_ctl_param_newdom_filter
!
!      modified by H. Matsui on Apr., 2008
!
!!      subroutine set_control_filter_newdomain(org_plt, new_plt,       &
!!     &          ffile_ctl, org_fil_files_ctl, newfil_p, ierr)
!!        type(platform_data_control), intent(in) :: org_plt, new_plt
!!        type(filter_file_control), intent(in) :: ffile_ctl
!!        type(ctl_param_newdom_filter), intent(inout) :: newfil_p
!
      module t_ctl_param_newdom_filter
!
      use m_precision
      use t_file_IO_parameter
!
      implicit none
!
      type ctl_param_newdom_filter
        type(field_IO_params) :: org_mesh_file
        type(field_IO_params) :: tgt_mesh_file
!
        character(len=kchara) :: org_filter_elen_head
        character(len=kchara) :: org_filter_coef_head
        character(len=kchara) :: org_filter_moms_head
!
        character(len=kchara) :: new_filter_elen_head
        character(len=kchara) :: new_filter_coef_head
        character(len=kchara) :: new_filter_moms_head
!
        integer(kind = kint) :: iflag_set_filter_elen
        integer(kind = kint) :: iflag_set_filter_coef
        integer(kind = kint) :: iflag_set_filter_moms
      end type ctl_param_newdom_filter
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_filter_newdomain(org_plt, new_plt,         &
     &          ffile_ctl, org_fil_files_ctl, newfil_p, ierr)
!
      use calypso_mpi
      use m_machine_parameter
      use calypso_mpi
      use m_2nd_pallalel_vector
      use m_file_format_switch
!
      use m_default_file_prefix
      use t_ctl_data_3d_filter
      use t_ctl_data_4_platforms
      use t_ctl_data_filter_files
      use set_control_platform_data
!
      type(platform_data_control), intent(in) :: org_plt, new_plt
      type(filter_file_control), intent(in) :: ffile_ctl
      type(org_filter_prefix_ctls), intent(in) :: org_fil_files_ctl
!
      type(ctl_param_newdom_filter), intent(inout) :: newfil_p
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(new_plt%ndomain_ctl%intvalue .gt. 0) then
        if(nprocs_2nd .eq. 0) then
          nprocs_2nd = new_plt%ndomain_ctl%intvalue
        else
          if(nprocs_2nd .ne. new_plt%ndomain_ctl%intvalue) then
            write(e_message,'(a)')                                      &
     &             'set num. of new domain for numn. of processes'
            ierr = 1
            return
          end if
        end if
      else
        write(*,*) 'set target number of domain in control'
        ierr = 100
        return
      end if
!
!
      if(org_plt%ndomain_ctl%iflag .gt. 0) then
        nprocs = org_plt%ndomain_ctl%intvalue
      else
        write(*,*) 'set original number of domain'
        stop
      end if
!
!
      call set_control_mesh_def(org_plt, newfil_p%org_mesh_file)
      call set_control_mesh_file_def                                    &
     &   (def_new_mesh_head, new_plt, newfil_p%tgt_mesh_file)
!
!
      newfil_p%org_filter_elen_head = "org/filter_elength"
      newfil_p%iflag_set_filter_elen                                    &
     &      = org_fil_files_ctl%org_filter_elen_head_ctl%iflag
      if(newfil_p%iflag_set_filter_elen .gt. 0) then
        newfil_p%org_filter_elen_head                                   &
     &      = org_fil_files_ctl%org_filter_elen_head_ctl%charavalue
      end if
!
      newfil_p%org_filter_coef_head = "org/filter_coef"
      newfil_p%iflag_set_filter_coef                                    &
     &      = org_fil_files_ctl%org_filter_coef_head_ctl%iflag
      if(newfil_p%iflag_set_filter_coef .gt. 0) then
        newfil_p%org_filter_coef_head                                   &
     &      = org_fil_files_ctl%org_filter_coef_head_ctl%charavalue
      end if
!
      newfil_p%org_filter_moms_head = "org/filter_moms"
      newfil_p%iflag_set_filter_moms                                    &
     &      = org_fil_files_ctl%org_filter_moms_head_ctl%iflag
      if(newfil_p%iflag_set_filter_moms .gt. 0) then
        newfil_p%org_filter_moms_head                                   &
     &      = org_fil_files_ctl%org_filter_moms_head_ctl%charavalue
      end if
!
!
      if(ffile_ctl%filter_elen_head_ctl%iflag .gt. 0) then
        newfil_p%new_filter_elen_head                                   &
     &         = ffile_ctl%filter_elen_head_ctl%charavalue
      else
        write(*,*) 'set target filter length file name'
        stop
      end if
!
      if(ffile_ctl%filter_coef_head_ctl%iflag .gt. 0) then
        newfil_p%new_filter_coef_head                                   &
     &         = ffile_ctl%filter_coef_head_ctl%charavalue
      else if(newfil_p%iflag_set_filter_coef .gt. 0) then
        write(*,*) 'set target filter coefficient file name'
        stop
      end if
!
      if(ffile_ctl%filter_moms_head_ctl%iflag .gt. 0) then
        newfil_p%new_filter_moms_head                                   &
     &         = ffile_ctl%filter_moms_head_ctl%charavalue
      else if(newfil_p%iflag_set_filter_moms .gt. 0) then
        write(*,*) 'set target filter moment file name'
        stop
      end if
!
      end subroutine set_control_filter_newdomain
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_param_newdom_filter
