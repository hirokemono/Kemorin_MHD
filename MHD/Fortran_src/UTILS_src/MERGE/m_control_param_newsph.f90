!m_control_param_newsph.f90
!      module m_control_param_newsph
!
!      Written by H. Matsui
!
!!      subroutine bcast_ctl_param_newsph
!!      subroutine set_control_4_newsph
!
      module m_control_param_newsph
!
      use m_precision
      use m_constants
      use calypso_mpi
      use t_file_IO_parameter
      use t_control_param_assemble
!
      implicit    none
!
!
      integer(kind = kint) :: np_sph_org
      integer(kind = kint) :: np_sph_new
!
      type(field_IO_params), save :: org_sph_fst_param
      type(field_IO_params), save :: new_sph_fst_param
!
      integer(kind=kint ) :: istep_start, istep_end, increment_step
!
      integer(kind = kint) :: iflag_newtime = 0
      integer(kind = kint) :: istep_new_rst, increment_new_step
      real(kind = kreal) :: time_new
!
      character(len=kchara) :: org_sph_head = 'mesh_org/in_rj'
      character(len=kchara) :: new_sph_head = 'mesh_new/in_rj'
!
      integer(kind=kint ) :: ifmt_org_sph_file =      0
      integer(kind=kint ) :: ifmt_new_sph_file =      0
!
!>      File prefix for new restart data
      character(len=kchara), parameter                                  &
     &                    :: def_org_sph_fst = "restart/rst"
!>      File prefix for new restart data
      character(len=kchara), parameter                                  &
     &                    :: def_new_sph_fst = "rst_new/rst"
!
      integer(kind=kint ) :: iflag_delete_org_sph =   0
!
      private :: set_control_original_step, sset_control_new_step
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine bcast_ctl_param_newsph(asbl_param)
!
      type(control_param_assemble), intent(inout) :: asbl_param
!
!
      call MPI_Bcast(np_sph_org, ione, CALYPSO_INTEGER,                 &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(np_sph_new, ione, CALYPSO_INTEGER,                 &
     &               izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(iflag_newtime, ione, CALYPSO_INTEGER,              &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(istep_new_rst, ione, CALYPSO_INTEGER,              &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(increment_new_step, ione, CALYPSO_INTEGER,         &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(time_new ,ione, CALYPSO_REAL,                      &
     &               izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(np_sph_new, ione, CALYPSO_INTEGER,                 &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(np_sph_new, ione, CALYPSO_INTEGER,                 &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(np_sph_new, ione, CALYPSO_INTEGER,                 &
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
      call MPI_Bcast(org_sph_fst_param%file_prefix, kchara,             &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(org_sph_fst_param%iflag_IO ,ione,                  &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(org_sph_fst_param%iflag_format, ione,              &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(new_sph_fst_param%file_prefix, kchara,             &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(new_sph_fst_param%iflag_IO ,ione,                  &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(new_sph_fst_param%iflag_format, ione,              &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(iflag_delete_org_sph, ione, CALYPSO_INTEGER,       &
     &               izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(asbl_param%b_ratio ,ione, CALYPSO_REAL,            &
     &               izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_param_newsph
!
!------------------------------------------------------------------
!
      subroutine set_control_4_newsph(mgd_ctl, asbl_param)
!
      use t_control_data_4_merge
      use m_file_format_switch
      use set_control_platform_data
      use new_SPH_restart
      use skip_comment_f
!
      type(control_data_4_merge), intent(in) :: mgd_ctl
      type(control_param_assemble), intent(inout) :: asbl_param
      character(len = kchara) :: tmpchara
!
!
      if (mgd_ctl%source_plt%ndomain_ctl%iflag .gt. 0) then
        np_sph_org = mgd_ctl%source_plt%ndomain_ctl%intvalue
      else
        write(*,*) 'Set number of subdomains'
        stop
      end if
!
      if (mgd_ctl%assemble_plt%ndomain_ctl%iflag .gt. 0) then
        np_sph_new = mgd_ctl%assemble_plt%ndomain_ctl%intvalue
      else
        write(*,*) 'Set number of subdomains for new grid'
        stop
      end if
!
      if(mgd_ctl%source_plt%sph_file_prefix%iflag .gt. 0) then
        org_sph_head = mgd_ctl%source_plt%sph_file_prefix%charavalue
      end if
      if (mgd_ctl%assemble_plt%sph_file_prefix%iflag .gt. 0) then
        new_sph_head = mgd_ctl%assemble_plt%sph_file_prefix%charavalue
      end if
!
      ifmt_org_sph_file                                                 &
     &    = choose_para_file_format                                     &
     &    (mgd_ctl%source_plt%sph_file_fmt_ctl)
      ifmt_new_sph_file = choose_para_file_format                       &
     &                (mgd_ctl%assemble_plt%sph_file_fmt_ctl)
!
!
      call set_parallel_file_ctl_params(def_org_sph_fst,                &
     &    mgd_ctl%source_plt%restart_file_prefix,                       &
     &    mgd_ctl%source_plt%restart_file_fmt_ctl, org_sph_fst_param)
      call set_parallel_file_ctl_params(def_new_sph_fst,                &
     &    mgd_ctl%assemble_plt%restart_file_prefix,                     &
     &    mgd_ctl%assemble_plt%restart_file_fmt_ctl, new_sph_fst_param)
!
!
      if((new_sph_fst_param%iflag_format/iflag_single) .gt. 0           &
     &     .and. np_sph_new .ne. nprocs) then
        new_sph_fst_param%iflag_format                                  &
     &            = new_sph_fst_param%iflag_format - iflag_single
        write(*,*) 'Turn off Merged data IO ',                          &
     &             'when number of MPI prosesses is not ',              &
     &             'the number of target subdomains.'
      end if
!
      if(mgd_ctl%assemble_plt%del_org_data_ctl%iflag .gt. 0) then
        tmpchara = mgd_ctl%assemble_plt%del_org_data_ctl%charavalue
        if(yes_flag(tmpchara)) iflag_delete_org_sph = 1
      end if
!
      call set_magnetic_ratio_4_assemble                                &
     &   (mgd_ctl%magnetic_ratio_ctl, asbl_param)
!
      call set_control_original_step(mgd_ctl%t_mge_ctl)
      call sset_control_new_step(mgd_ctl%t2_mge_ctl)
!
      end subroutine set_control_4_newsph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_original_step(t_mge_ctl)
!
      use t_ctl_data_4_time_steps
!
      type(time_data_control), intent(in) :: t_mge_ctl
!
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
      end subroutine set_control_original_step
!
! -----------------------------------------------------------------------
!
      subroutine sset_control_new_step(t2_mge_ctl)
!
      use t_ctl_data_4_time_steps
!
      type(time_data_control), intent(in) :: t2_mge_ctl
!
!
      if(t2_mge_ctl%i_step_init_ctl%iflag .gt. 0) then
        istep_new_rst = t2_mge_ctl%i_step_init_ctl%intvalue
      else
        istep_new_rst = istep_start
      end if
      if (t2_mge_ctl%i_step_rst_ctl%iflag .gt. 0) then
        increment_new_step = t2_mge_ctl%i_step_rst_ctl%intvalue
      else
        increment_new_step = increment_step
      end if
      if(t2_mge_ctl%time_init_ctl%iflag .gt. 0) then
        time_new = t2_mge_ctl%time_init_ctl%realvalue
      end if
!
      if      (t2_mge_ctl%time_init_ctl%iflag .gt. 0                    &
     &   .and. t2_mge_ctl%i_step_init_ctl%iflag .gt. 0                  &
     &   .and. t2_mge_ctl%i_step_rst_ctl%iflag .gt. 0) then
        if(istep_start .ne. istep_end) then
          stop 'Choose one snapshot to change time step information'
        else
          iflag_newtime = 1
        end if
      else
        iflag_newtime = 0
      end if
!
      end subroutine sset_control_new_step
!
! -----------------------------------------------------------------------
!
      end module m_control_param_newsph
