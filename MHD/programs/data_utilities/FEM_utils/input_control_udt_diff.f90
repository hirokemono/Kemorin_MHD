!
!      module input_control_udt_diff
!
!     Written by H. Matsui on July, 2006
!
!!      subroutine s_input_control_udt_diff                             &
!!     &         (mesh_file, udt_org_param, nod_fld, time_U)
!!      subroutine s_input_control_ave_udt                              &
!!     &         (mesh_file, udt_org_param, nod_fld, time_U)
!!      subroutine s_input_control_corr_udt                             &
!!     &         (mesh_file, udt_org_param, nod_fld, time_U)
!!      subroutine s_input_control_grp_patch(mesh_file, udt_org_param)
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!        type(field_IO_params), intent(inout) :: udt_org_param
!!        type(phys_data), intent(inout) :: nod_fld
!!  !!        type(time_step_param), intent(inout) :: time_U
!
      module input_control_udt_diff
!
      use m_precision
      use m_machine_parameter
!
      use calypso_mpi
      use t_step_parameter
      use t_phys_data
      use t_file_IO_parameter
      use t_IO_step_parameter
      use t_ctl_data_diff_udt
!
      implicit none
!
      character(len = kchara), parameter                                &
     &                 :: fname_diff_ctl = "ctl_diff_udt"
      character(len = kchara), parameter                                &
     &                 :: fname_ave_ctl =  "ctl_ave_udt"
      character(len = kchara), parameter                                &
     &                 :: fname_corr_ctl = "ctl_correlate_udt"
      character(len = kchara), parameter                                &
     &                 :: fname_grp_patch_ctl = "ctl_med_group_patch"
!
!
!     Top level for difference
      character(len=kchara), parameter ::                               &
     &                   hd_diff_control = 'difference_udts'
!     Top level for average
      character(len=kchara), parameter ::                               &
     &                   hd_ave_control = 'averaging_udts'
!     Top level for correlation
      character(len=kchara), parameter ::                               &
     &                   hd_corr_control = 'correlate_udts'
!     Top level for meridional patch
      character(len=kchara), parameter ::                               &
     &                   hd_med_grp_patch = 'meridional_group_patch'
!
      type(diff_udt_ctl), save, private :: diff_udt_c1
!
      private :: fname_diff_ctl, fname_ave_ctl
      private :: fname_corr_ctl, fname_grp_patch_ctl
!
      private :: hd_diff_control, hd_ave_control
      private :: hd_corr_control, hd_med_grp_patch
!
      private :: laod_control_4_diff_udt
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine laod_control_4_diff_udt                                &
     &         (file_name, hd_block, diff_udt_c)
!
      use bcast_ctl_data_diff_udt
!
      character(len=kchara), intent(in) :: file_name, hd_block
      type(diff_udt_ctl), intent(inout)  :: diff_udt_c
!
!
      if(my_rank .eq. 0) then
        call read_control_4_diff_udt(file_name, hd_block, diff_udt_c)
      end if
!
      call bcast_diff_control_data(diff_udt_c)
!
      if(diff_udt_c%i_diff_control .ne. 1) then
        call calypso_MPI_abort(diff_udt_c%i_diff_control,               &
     &                             'control file is broken')
      end if
!
      end subroutine laod_control_4_diff_udt
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine s_input_control_udt_diff                               &
     &         (mesh_file, udt_org_param, nod_fld, time_U)
!
      use m_ctl_params_4_diff_udt
!
      use bcast_ctl_data_diff_udt
      use set_ctl_diff_udt
      use set_field_data_w_SGS
!
      type(field_IO_params), intent(inout) ::  mesh_file
      type(field_IO_params), intent(inout) :: udt_org_param
      type(phys_data), intent(inout) :: nod_fld
      type(time_step_param), intent(inout) :: time_U
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.eq.1) write(*,*) 'laod_control_4_diff_udt'
      call laod_control_4_diff_udt                                      &
     &   (fname_diff_ctl, hd_diff_control, diff_udt_c1)
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_diff_udt'
      call set_ctl_params_diff_udt(diff_udt_c1%d_plt,                   &
     &    diff_udt_c1%org_d_plt, diff_udt_c1%diff_ctl,                  &
     &    mesh_file, udt_org_param)
!
      if (iflag_debug.eq.1) write(*,*) 'set_SGS_field_ctl_by_viz'
      call set_SGS_field_ctl_by_viz                                     &
     &   (diff_udt_c1%diff_ctl%fld_d_ctl%field_ctl, nod_fld, ierr)
      if (ierr .ne. 0) call calypso_MPI_abort(ierr, e_message)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_ctl_4_diff_udt_steps'
      call s_set_ctl_4_diff_udt_steps                                   &
     &   (diff_udt_c1%diff_ctl%t_d_ctl, time_U)
      call dealloc_diff_control_data(diff_udt_c1)
!
      end subroutine s_input_control_udt_diff
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_ave_udt                                &
     &         (mesh_file, udt_org_param, nod_fld, time_U)
!
      use m_ctl_params_4_diff_udt
!
      use bcast_ctl_data_diff_udt
      use set_ctl_diff_udt
      use set_field_data_w_SGS
!
      type(field_IO_params), intent(inout) ::  mesh_file
      type(field_IO_params), intent(inout)  :: udt_org_param
      type(phys_data), intent(inout) :: nod_fld
      type(time_step_param), intent(inout) :: time_U
!
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.eq.1) write(*,*) 'laod_control_4_diff_udt'
      call laod_control_4_diff_udt                                      &
     &   (fname_ave_ctl, hd_ave_control, diff_udt_c1)
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_diff_udt'
      call set_ctl_params_diff_udt(diff_udt_c1%d_plt,                   &
     &    diff_udt_c1%org_d_plt, diff_udt_c1%diff_ctl,                  &
     &    mesh_file, udt_org_param)
!
      if (iflag_debug.eq.1) write(*,*) 'set_SGS_field_ctl_by_viz'
      call set_SGS_field_ctl_by_viz                                     &
     &   (diff_udt_c1%diff_ctl%fld_d_ctl%field_ctl, nod_fld, ierr)
      if (ierr .ne. 0) call calypso_MPI_abort(ierr, e_message)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_ctl_4_diff_udt_steps'
      call s_set_ctl_4_diff_udt_steps                                   &
     &   (diff_udt_c1%diff_ctl%t_d_ctl, time_U)
      call dealloc_diff_control_data(diff_udt_c1)
!
      end subroutine s_input_control_ave_udt
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_corr_udt                               &
     &         (mesh_file, udt_org_param, nod_fld, time_U)
!
      use bcast_ctl_data_diff_udt
      use m_ctl_params_4_diff_udt
!
      use set_ctl_diff_udt
!
      type(field_IO_params), intent(inout) ::  mesh_file
      type(field_IO_params), intent(inout)  :: udt_org_param
      type(phys_data), intent(inout) :: nod_fld
      type(time_step_param), intent(inout) :: time_U
!
!
      if (iflag_debug.eq.1) write(*,*) 'laod_control_4_diff_udt'
      call laod_control_4_diff_udt                                      &
     &   (fname_corr_ctl, hd_corr_control, diff_udt_c1)
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_correlate_udt'
      call set_ctl_params_correlate_udt(diff_udt_c1%d_plt,              &
     &    diff_udt_c1%org_d_plt, diff_udt_c1%diff_ctl,                  &
     &    mesh_file, udt_org_param, nod_fld, time_U)
!
      call dealloc_diff_control_data(diff_udt_c1)
!
      end subroutine s_input_control_corr_udt
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_grp_patch(mesh_file, udt_org_param)
!
      use m_ctl_params_4_diff_udt
!
      use bcast_ctl_data_diff_udt
      use set_ctl_diff_udt
!
      type(field_IO_params), intent(inout) ::  mesh_file
      type(field_IO_params), intent(inout)  :: udt_org_param
!
!
      if (iflag_debug.eq.1) write(*,*) 'laod_control_4_diff_udt'
      call laod_control_4_diff_udt                                      &
     &   (fname_grp_patch_ctl, hd_med_grp_patch, diff_udt_c1)
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_diff_udt'
      call set_ctl_params_diff_udt(diff_udt_c1%d_plt,                   &
     &    diff_udt_c1%org_d_plt, diff_udt_c1%diff_ctl,                  &
     &    mesh_file, udt_org_param)
!
      end subroutine s_input_control_grp_patch
!
! ----------------------------------------------------------------------
!
      end module input_control_udt_diff
