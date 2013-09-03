!m_ctl_param_newdom_filter.f90
!      module m_ctl_param_newdom_filter
!
      module m_ctl_param_newdom_filter
!
!      modified by H. Matsui on Apr., 2008
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
!
!      subroutine set_control_filter_newdomain
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_filter_newdomain
!
      use m_machine_parameter
      use m_parallel_var_dof
      use m_2nd_pallalel_vector
      use m_filter_elength
      use m_file_format_switch
!
      use m_read_mesh_data
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
      use m_ctl_data_filter_files
      use m_ctl_data_org_filter_name
!
!
      if(i_num_new_domain .gt. 0) then
        if(nprocs_2nd .eq. 0) then
          nprocs_2nd = num_new_domain_ctl
        else
          if(nprocs_2nd .ne. num_new_domain_ctl) then
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
      if(i_num_subdomain .gt. 0) then
        nprocs = num_subdomain_ctl
      else
        write(*,*) 'set original number of domain'
        stop
      end if
!
!
      if(i_new_mesh_head .gt. 0) then
        target_mesh_head = new_mesh_prefix
      else
        write(*,*) 'set target mesh file name'
        stop
      end if
!
      if(i_mesh_header .gt. 0) then
        org_mesh_head =  mesh_file_prefix
        mesh_file_head = mesh_file_prefix
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
      if(i_filter_elen_head_ctl .gt. 0) then
        new_filter_elen_head = filter_elen_head_ctl
      else
        write(*,*) 'set target filter length file name'
        stop
      end if
!
      if(i_filter_coef_head_ctl .gt. 0) then
        new_filter_coef_head = filter_coef_head_ctl
      else if(iflag_set_filter_coef .gt. 0) then
        write(*,*) 'set target filter coefficient file name'
        stop
      end if
!
      if(i_filter_moms_head_ctl .gt. 0) then
        new_filter_moms_head = filter_moms_head_ctl
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
