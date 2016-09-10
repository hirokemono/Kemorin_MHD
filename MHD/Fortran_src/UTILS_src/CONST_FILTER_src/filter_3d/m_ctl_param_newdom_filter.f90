!m_ctl_param_newdom_filter.f90
!      module m_ctl_param_newdom_filter
!
!      modified by H. Matsui on Apr., 2008
!
!      subroutine set_control_filter_newdomain(ierr)
!
      module m_ctl_param_newdom_filter
!
      use m_precision
!
      implicit none
!
      character(len=kchara) :: target_mesh_head
      character(len=kchara) :: org_mesh_head
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
!
      integer(kind = kint), parameter :: id_org_filter_coef = 23
      integer(kind = kint), parameter :: id_org_filter_elen = 25
      integer(kind = kint), parameter :: id_new_filter_coef = 33
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_filter_newdomain(ierr)
!
      use calypso_mpi
      use m_machine_parameter
      use calypso_mpi
      use m_2nd_pallalel_vector
      use m_file_format_switch
!
      use m_read_mesh_data
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
      use m_ctl_data_filter_files
      use m_ctl_data_org_filter_name
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(num_new_domain_ctl%intvalue .gt. 0) then
        if(nprocs_2nd .eq. 0) then
          nprocs_2nd = num_new_domain_ctl%intvalue
        else
          if(nprocs_2nd .ne. num_new_domain_ctl%intvalue) then
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
      if(ndomain_ctl%iflag .gt. 0) then
        nprocs = ndomain_ctl%intvalue
      else
        write(*,*) 'set original number of domain'
        stop
      end if
!
!
      if(new_mesh_prefix%iflag .gt. 0) then
        target_mesh_head = new_mesh_prefix%charavalue
      else
        write(*,*) 'set target mesh file name'
        stop
      end if
!
      if(mesh_file_prefix%iflag .gt. 0) then
        org_mesh_head =  mesh_file_prefix%charavalue
        mesh_file_head = mesh_file_prefix%charavalue
      else
        write(*,*) 'set original mesh file name'
        stop
      end if
!
      iflag_mesh_file_fmt = id_ascii_file_fmt
!
      iflag_set_filter_elen = i_org_filter_elen_head
      if(iflag_set_filter_elen .gt. 0) then
        org_filter_elen_head = org_filter_elen_head_ctl
      end if
!
      iflag_set_filter_coef = i_org_filter_coef_head
      if(iflag_set_filter_coef .gt. 0) then
        org_filter_coef_head = org_filter_coef_head_ctl
      end if
!
      iflag_set_filter_moms = i_org_filter_moms_head
      if (iflag_set_filter_moms .gt. 0) then
        org_filter_moms_head = org_filter_moms_head_ctl
      end if
!
!
      if(filter_elen_head_ctl%iflag .gt. 0) then
        new_filter_elen_head = filter_elen_head_ctl%charavalue
      else
        write(*,*) 'set target filter length file name'
        stop
      end if
!
      if(filter_coef_head_ctl%iflag .gt. 0) then
        new_filter_coef_head = filter_coef_head_ctl%charavalue
      else if(iflag_set_filter_coef .gt. 0) then
        write(*,*) 'set target filter coefficient file name'
        stop
      end if
!
      if(filter_moms_head_ctl%iflag .gt. 0) then
        new_filter_moms_head = filter_moms_head_ctl%charavalue
      else if(iflag_set_filter_moms .gt. 0) then
        write(*,*) 'set target filter moment file name'
        stop
      end if
!
      end subroutine set_control_filter_newdomain
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_param_newdom_filter
