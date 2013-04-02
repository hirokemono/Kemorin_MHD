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
      use m_control_data_4_cutshell
      use const_cutshell_mesh
!
!
      if (i_org_f_ctl .gt. 0) then
        original_mesh_head = orginal_mesh_head_ctl
      else
        write(*,*) 'set original mesh header'
        stop
      end if
!
      if (i_cutshell_f_ctl .gt. 0) then
        modified_mesh_head = cutshell_mesh_head_ctl
      else
        write(*,*) 'set refined mesh header'
        stop
      end if
!
!
      if (i_cutshell_type .gt. 0) then
        if    (   cutshell_type_ctl .eq. 'hemisphere'                   &
     &       .or. cutshell_type_ctl .eq. 'hemisphere'                   &
     &       .or. cutshell_type_ctl .eq. 'HEMiSPHERE'                   &
     &       .or. cutshell_type_ctl .eq. 'hemi'                         &
     &       .or. cutshell_type_ctl .eq. 'Hemi'                         &
     &       .or. cutshell_type_ctl .eq. 'HEMI') then
          iflag_reduce_type = 1
        else if ( cutshell_type_ctl .eq. 'cut_shell'                    &
     &       .or. cutshell_type_ctl .eq. 'Cut_shell'                    &
     &       .or. cutshell_type_ctl .eq. 'CUT_SHELL') then
          iflag_reduce_type = 2
        else if ( cutshell_type_ctl .eq. 'spherical_shell'              &
     &       .or. cutshell_type_ctl .eq. 'Spherical_shell'              &
     &       .or. cutshell_type_ctl .eq. 'SPHERICAL_SHELL') then
          iflag_reduce_type = 3
        else if ( cutshell_type_ctl .eq. 'hemispherical_shell'          &
     &       .or. cutshell_type_ctl .eq. 'Hemispherical_shell'          &
     &       .or. cutshell_type_ctl .eq. 'HEMISPHERICAL_SHELL') then
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
