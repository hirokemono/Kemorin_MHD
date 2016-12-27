!m_control_param_newsph.f90
!      module m_control_param_newsph
!
!      Written by H. Matsui
!
!      subroutine set_control_4_newsph
!
      module m_control_param_newsph
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit    none
!
!
      integer(kind = kint) :: np_sph_org
      integer(kind = kint) :: np_sph_new
!
      integer(kind=kint ) :: istep_start, istep_end, increment_step
!
      character(len=kchara) :: org_sph_head = 'mesh_org/in_rj'
      character(len=kchara) :: new_sph_head = 'mesh_new/in_rj'
!
      integer(kind=kint ) :: ifmt_org_sph_file =      0
      integer(kind=kint ) :: ifmt_new_sph_file =      0
!
!>      File prefix for new restart data
      character(len=kchara) :: org_sph_fst_head = "restart/rst"
!>      File prefix for new restart data
      character(len=kchara) :: new_sph_fst_head = "rst_new/rst"
!
      integer(kind=kint ) :: ifmt_org_sph_fst =       0
      integer(kind=kint ) :: ifmt_new_sph_fst =       0
!
      integer(kind=kint ) :: iflag_delete_org_sph =   0
!
!>      multiply the amplitude
      real(kind = kreal) :: b_sph_ratio
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine bcast_ctl_param_newsph
!
!
      call MPI_Bcast(np_sph_org, ione, CALYPSO_INTEGER,                 &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(np_sph_new, ione, CALYPSO_INTEGER,                 &
     &               izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(istep_start, ione, CALYPSO_INTEGER,                &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(istep_end, ione, CALYPSO_INTEGER,                  &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(increment_step, ione, CALYPSO_INTEGER,             &
     &               izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(org_sph_head, kchara, CALYPSO_CHARACTER,           &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(new_sph_head ,kchara, CALYPSO_CHARACTER,           &
     &               izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(ifmt_org_sph_file, ione, CALYPSO_INTEGER,          &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(ifmt_new_sph_file, ione, CALYPSO_INTEGER,          &
     &               izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(org_sph_fst_head, kchara, CALYPSO_CHARACTER,       &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(new_sph_fst_head ,kchara, CALYPSO_CHARACTER,       &
     &               izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(ifmt_org_sph_fst, ione, CALYPSO_INTEGER,           &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(ifmt_new_sph_fst ,ione, CALYPSO_INTEGER,           &
     &               izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(iflag_delete_org_sph ,ione, CALYPSO_INTEGER,       &
     &               izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(b_sph_ratio ,ione, CALYPSO_REAL,                   &
     &               izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_param_newsph
!
!------------------------------------------------------------------
!
      subroutine set_control_4_newsph
!
      use m_control_data_4_merge
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
      use m_ctl_data_4_time_steps
      use m_file_format_switch
      use set_control_platform_data
      use new_SPH_restart
      use skip_comment_f
!
!
      if (plt1%ndomain_ctl%iflag .gt. 0) then
        np_sph_org = plt1%ndomain_ctl%intvalue
      else
        write(*,*) 'Set number of subdomains'
        stop
      end if
!
      if (new_plt%ndomain_ctl%iflag .gt. 0) then
        np_sph_new = new_plt%ndomain_ctl%intvalue
      else
        write(*,*) 'Set number of subdomains for new grid'
        stop
      end if
!
      if(plt1%sph_file_prefix%iflag .gt. 0) then
        org_sph_head = plt1%sph_file_prefix%charavalue
      end if
      if (new_plt%sph_file_prefix%iflag .gt. 0) then
        new_sph_head = new_plt%sph_file_prefix%charavalue
      end if
!
      call choose_para_file_format                                      &
     &   (plt1%sph_file_fmt_ctl, ifmt_org_sph_file)
      call choose_para_file_format                                      &
     &   (new_plt%sph_file_fmt_ctl, ifmt_new_sph_file)
!
!
      if (plt1%restart_file_prefix%iflag .gt. 0) then
        org_sph_fst_head = plt1%restart_file_prefix%charavalue
      end if
!
      if(new_plt%restart_file_prefix%iflag .gt. 0) then
        new_sph_fst_head = new_plt%restart_file_prefix%charavalue
      end if
!
      call choose_para_file_format                                      &
     &   (plt1%restart_file_fmt_ctl, ifmt_org_sph_fst)
      call choose_para_file_format                                      &
     &   (new_plt%restart_file_fmt_ctl, ifmt_new_sph_fst)
!
      if((ifmt_new_sph_fst/iflag_single) .gt. 0                         &
     &     .and. np_sph_new .ne. nprocs) then
        ifmt_new_sph_fst = ifmt_new_sph_fst - iflag_single
        write(*,*) 'Turn off Merged data IO ',                          &
     &             'when number of MPI prosesses is not ',              &
     &             'the number of target subdomains.'
      end if
!
      if(new_plt%del_org_data_ctl%iflag .gt. 0) then
        if(yes_flag(new_plt%del_org_data_ctl%charavalue)) then
          iflag_delete_org_sph = 1
        end if
      end if
!
      b_sph_ratio = 1.0d0
      if (magnetic_ratio_ctl%iflag .gt. 0) then
        b_sph_ratio = magnetic_ratio_ctl%realvalue
      end if
!
      istep_start = 1
      if(i_step_init_ctl%iflag .gt. 0) then
        istep_start = i_step_init_ctl%intvalue
      end if
!
      istep_end =  1
      if(i_step_number_ctl%iflag .gt. 0) then
        istep_end = i_step_number_ctl%intvalue
      end if
!
      increment_step = 1
      if (i_step_rst_ctl%iflag .gt. 0) then
        increment_step = i_step_rst_ctl%intvalue
      end if
!
      end subroutine set_control_4_newsph
!
! -----------------------------------------------------------------------
!
      end module m_control_param_newsph
