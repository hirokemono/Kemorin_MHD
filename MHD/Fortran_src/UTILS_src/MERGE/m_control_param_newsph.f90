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
      character(len=kchara), parameter, private                         &
     &         :: def_org_sph_head = 'mesh_org/in_rj'
      character(len=kchara), parameter, private                         &
     &         :: def_new_sph_head = 'mesh_new/in_rj'
!
!>      File prefix for new restart data
      character(len=kchara), parameter, private                         &
     &                    :: def_org_sph_fst = "restart/rst"
!>      File prefix for new restart data
      character(len=kchara), parameter, private                         &
     &                    :: def_new_sph_fst = "rst_new/rst"
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine bcast_ctl_param_newsph(asbl_param)
!
      use bcast_file_IO_parameter
!
      type(control_param_assemble), intent(inout) :: asbl_param
!
!
      call MPI_Bcast(np_sph_org, ione, CALYPSO_INTEGER,                 &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(np_sph_new, ione, CALYPSO_INTEGER,                 &
     &               izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(asbl_param%iflag_newtime, ione,                    &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(asbl_param%istep_new_rst, ione,                    &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(asbl_param%increment_new_step, ione,               &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(asbl_param%time_new ,ione,                         &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(asbl_param%istep_start, ione, CALYPSO_INTEGER,     &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(asbl_param%istep_end, ione, CALYPSO_INTEGER,       &
     &               izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(asbl_param%increment_step, ione, CALYPSO_INTEGER,  &
     &               izero, CALYPSO_COMM, ierr_MPI)
!
      call bcast_field_IO_parameter(asbl_param%org_mesh_file)
      call bcast_field_IO_parameter(asbl_param%new_mesh_file)
!
      call bcast_field_IO_parameter(asbl_param%org_fld_file)
      call bcast_field_IO_parameter(asbl_param%new_fld_file)
!
      call MPI_Bcast(asbl_param%iflag_delete_org, ione,                 &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(asbl_param%b_ratio ,ione,                          &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
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
      call set_parallel_file_ctl_params(def_org_sph_head,               &
     &    mgd_ctl%source_plt%sph_file_prefix,                           &
     &    mgd_ctl%source_plt%sph_file_fmt_ctl,                          &
     &    asbl_param%org_mesh_file)
      call set_parallel_file_ctl_params(def_new_sph_head,               &
     &    mgd_ctl%assemble_plt%sph_file_prefix,                         &
     &    mgd_ctl%assemble_plt%sph_file_fmt_ctl,                        &
     &    asbl_param%new_mesh_file)
!
!
      call set_assemble_rst_file_param                                  &
     &   (mgd_ctl%source_plt, mgd_ctl%assemble_plt, asbl_param)
!
!
      if((asbl_param%new_fld_file%iflag_format/iflag_single) .gt. 0     &
     &     .and. np_sph_new .ne. nprocs) then
        asbl_param%new_fld_file%iflag_format                            &
     &            = asbl_param%new_fld_file%iflag_format - iflag_single
        write(*,*) 'Turn off Merged data IO ',                          &
     &             'when number of MPI prosesses is not ',              &
     &             'the number of target subdomains.'
      end if
!
      call set_delete_flag_4_assemble(mgd_ctl%assemble_plt, asbl_param)
!
      call set_magnetic_ratio_4_assemble                                &
     &   (mgd_ctl%magnetic_ratio_ctl, asbl_param)
!
      call set_assemble_step_4_rst(mgd_ctl%t_mge_ctl, asbl_param)
      call set_control_new_step(mgd_ctl%t2_mge_ctl, asbl_param)
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &          'istep_start, istep_end, increment_step',               &
     &           asbl_param%istep_start, asbl_param%istep_end,          &
     &           asbl_param%increment_step
!
      end subroutine set_control_4_newsph
!
! -----------------------------------------------------------------------
!
      end module m_control_param_newsph
