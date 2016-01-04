!
!      module input_control_udt_diff
!
!     Written by H. Matsui on July, 2006
!
!      subroutine s_input_control_udt_diff(nod_fld, ucd)
!      subroutine s_input_control_ave_udt(nod_fld, ucd)
!      subroutine s_input_control_corr_udt(nod_fld, ucd)
!      subroutine s_input_control_grp_patch(ucd)
!        type(phys_data), intent(inout) :: nod_fld
!        type(ucd_data), intent(inout) :: ucd
!
      module input_control_udt_diff
!
      use m_precision
      use m_machine_parameter
!
      use calypso_mpi
      use t_phys_data
      use t_ucd_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_udt_diff(nod_fld, ucd)
!
      use m_ctl_params_4_diff_udt
      use m_ctl_data_diff_udt
!
      use set_ctl_diff_udt
      use set_control_nodal_data
!
      type(phys_data), intent(inout) :: nod_fld
      type(ucd_data), intent(inout) :: ucd
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_diff_udt'
      call read_control_4_diff_udt
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_diff_udt'
      call set_ctl_params_diff_udt(ucd)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_control_nodal_data'
      call s_set_control_nodal_data(nod_fld, ierr)
      if (ierr .ne. 0) call calypso_MPI_abort(ierr, e_message)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_ctl_4_diff_udt_steps'
      call s_set_ctl_4_diff_udt_steps
!
      end subroutine s_input_control_udt_diff
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_ave_udt(nod_fld, ucd)
!
      use m_ctl_params_4_diff_udt
      use m_ctl_data_diff_udt
!
      use set_ctl_diff_udt
      use set_control_nodal_data
!
      type(phys_data), intent(inout) :: nod_fld
      type(ucd_data), intent(inout) :: ucd
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_ave_udt'
      call read_control_4_ave_udt
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_diff_udt'
      call set_ctl_params_diff_udt(ucd)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_control_nodal_data'
      call s_set_control_nodal_data(nod_fld, ierr)
      if (ierr .ne. 0) call calypso_MPI_abort(ierr, e_message)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_ctl_4_diff_udt_steps'
      call s_set_ctl_4_diff_udt_steps
!
      end subroutine s_input_control_ave_udt
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_corr_udt(nod_fld, ucd)
!
      use m_ctl_params_4_diff_udt
      use m_ctl_data_diff_udt
!
      use set_ctl_diff_udt
!
      type(phys_data), intent(inout) :: nod_fld
      type(ucd_data), intent(inout) :: ucd
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_corr_udt'
      call read_control_4_corr_udt
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_correlate_udt'
      call set_ctl_params_correlate_udt(nod_fld, ucd)
!
      end subroutine s_input_control_corr_udt
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_grp_patch(ucd)
!
      use m_ctl_params_4_diff_udt
      use m_ctl_data_diff_udt
!
      use set_ctl_diff_udt
!
      type(ucd_data), intent(inout) :: ucd
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_med_grp_patch'
      call read_control_med_grp_patch
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_diff_udt'
      call set_ctl_params_diff_udt(ucd)
!
      end subroutine s_input_control_grp_patch
!
! ----------------------------------------------------------------------
!
      end module input_control_udt_diff
