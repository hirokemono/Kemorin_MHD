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
      use m_file_format_switch
      use set_control_platform_data
      use new_SPH_restart
      use skip_comment_f
!
!
      if (source_plt%ndomain_ctl%iflag .gt. 0) then
        np_sph_org = source_plt%ndomain_ctl%intvalue
      else
        write(*,*) 'Set number of subdomains'
        stop
      end if
!
      if (assemble_plt%ndomain_ctl%iflag .gt. 0) then
        np_sph_new = assemble_plt%ndomain_ctl%intvalue
      else
        write(*,*) 'Set number of subdomains for new grid'
        stop
      end if
!
      if(source_plt%sph_file_prefix%iflag .gt. 0) then
        org_sph_head = source_plt%sph_file_prefix%charavalue
      end if
      if (assemble_plt%sph_file_prefix%iflag .gt. 0) then
        new_sph_head = assemble_plt%sph_file_prefix%charavalue
      end if
!
      call choose_para_file_format                                      &
     &   (source_plt%sph_file_fmt_ctl, ifmt_org_sph_file)
      call choose_para_file_format                                      &
     &   (assemble_plt%sph_file_fmt_ctl, ifmt_new_sph_file)
!
!
      if (source_plt%restart_file_prefix%iflag .gt. 0) then
        org_sph_fst_head = source_plt%restart_file_prefix%charavalue
      end if
!
      if(assemble_plt%restart_file_prefix%iflag .gt. 0) then
        new_sph_fst_head = assemble_plt%restart_file_prefix%charavalue
      end if
!
      call choose_para_file_format                                      &
     &   (source_plt%restart_file_fmt_ctl, ifmt_org_sph_fst)
      call choose_para_file_format                                      &
     &   (assemble_plt%restart_file_fmt_ctl, ifmt_new_sph_fst)
!
      if((ifmt_new_sph_fst/iflag_single) .gt. 0                         &
     &     .and. np_sph_new .ne. nprocs) then
        ifmt_new_sph_fst = ifmt_new_sph_fst - iflag_single
        write(*,*) 'Turn off Merged data IO ',                          &
     &             'when number of MPI prosesses is not ',              &
     &             'the number of target subdomains.'
      end if
!
      if(assemble_plt%del_org_data_ctl%iflag .gt. 0) then
        if(yes_flag(assemble_plt%del_org_data_ctl%charavalue)) then
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
      if(t_mge_ctl%i_step_init_ctl%iflag .gt. 0) then
        istep_start = t_mge_ctl%i_step_init_ctl%intvalue
      end if
!
      istep_end =  1
      if(t_mge_ctl%i_step_number_ctl%iflag .gt. 0) then
        istep_end = t_mge_ctl%i_step_number_ctl%intvalue
      end if
!
      increment_step = 1
      if (t_mge_ctl%i_step_rst_ctl%iflag .gt. 0) then
        increment_step = t_mge_ctl%i_step_rst_ctl%intvalue
      end if
!
      end subroutine set_control_4_newsph
!
! -----------------------------------------------------------------------
!
      end module m_control_param_newsph
