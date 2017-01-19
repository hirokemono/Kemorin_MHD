!set_control_cut_shell.f90
!      module set_control_cut_shell
!
      module set_control_cut_shell
!
!      Written by Kemorin on Oct., 2007
!
      use m_precision
!
      implicit    none
!
!
!      subroutine s_set_control_4_cutshell
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_cutshell
!
      use m_default_file_prefix
      use m_control_data_4_cutshell
      use const_cutshell_mesh
      use set_control_platform_data
      use skip_comment_f
!
      character(len=kchara) :: tmpchara
!
!
      call set_file_control_params(def_org_mesh_head,                   &
     &    orginal_mesh_head_ctl, orginal_mesh_fmt_ctl,                  &
     &    original_mesh_file)
      call set_file_control_params(def_mesh_file_head,                  &
     &    cutshell_mesh_head_ctl, cutshell_mesh_fmt_ctl,                &
     &    modified_mesh_file)
!
      if (cutshell_type_ctl%iflag .gt. 0) then
        tmpchara = cutshell_type_ctl%charavalue
        if    (  cmp_no_case(tmpchara, 'hemisphere')                     &
     &      .or. cmp_no_case(tmpchara, 'hemi')) then
          iflag_reduce_type = 1
        else if (cmp_no_case(tmpchara, 'cut_shell')) then
          iflag_reduce_type = 2
        else if (cmp_no_case(tmpchara, 'spherical_shell')) then
          iflag_reduce_type = 3
        else if (cmp_no_case(tmpchara, 'hemispherical_shell')) then
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
