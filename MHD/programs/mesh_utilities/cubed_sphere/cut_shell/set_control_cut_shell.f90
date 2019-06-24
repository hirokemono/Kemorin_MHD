!set_control_cut_shell.f90
!      module set_control_cut_shell
!
!      Written by Kemorin on Oct., 2007
!
!!      subroutine s_set_control_4_cutshell(cutshell_ctl)
!!        type(ctl_data_cutshell), intent(in)  :: cutshell_ctl
!
      module set_control_cut_shell
!
      use m_precision
!
      implicit    none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_cutshell(cutshell_ctl)
!
      use m_default_file_prefix
      use t_control_data_4_cutshell
      use const_cutshell_mesh
      use set_control_platform_data
      use skip_comment_f
!
      type(ctl_data_cutshell), intent(in)  :: cutshell_ctl
!
      character(len=kchara) :: tmpchara
!
!
      call set_file_control_params(def_org_mesh_head,                   &
     &    cutshell_ctl%orginal_mesh_head_ctl,                           &
     &    cutshell_ctl%orginal_mesh_fmt_ctl, original_mesh_file)
      call set_file_control_params(def_mesh_file_head,                  &
     &    cutshell_ctl%cutshell_mesh_head_ctl,                          &
     &    cutshell_ctl%cutshell_mesh_fmt_ctl, modified_mesh_file)
!
      if(cutshell_ctl%cutshell_type_ctl%iflag .gt. 0) then
        tmpchara = cutshell_ctl%cutshell_type_ctl%charavalue
        if    (  cmp_no_case(tmpchara, cf_hemisphere1)                  &
     &      .or. cmp_no_case(tmpchara, cf_hemisphere2)) then
          iflag_reduce_type = 1
        else if (cmp_no_case(tmpchara, cf_cutshell)) then
          iflag_reduce_type = 2
        else if (cmp_no_case(tmpchara, cf_sph_shell)) then
          iflag_reduce_type = 3
        else if (cmp_no_case(tmpchara, cf_hemi_shell)) then
          iflag_reduce_type = 4
        end if
      end if
!
!
      end subroutine s_set_control_4_cutshell
!
! -----------------------------------------------------------------------
!
      end module set_control_cut_shell
