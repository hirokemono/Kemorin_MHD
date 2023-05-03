!bcast_control_add_ele_grp.f90
!      module bcast_control_add_ele_grp
!
!      Written by H. Matsui on Mar., 2008
!
!!      subroutine load_control_add_elegrp(addgrp_c)
!!        type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
      module bcast_control_add_ele_grp
!
      use m_precision
!
      use calypso_mpi
      use t_control_data_add_ele_grp
!
      implicit none
!
      character (len = kchara), parameter, private                      &
     &         :: control_file_name = 'ctl_add_ele_grp'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine load_control_add_elegrp(addgrp_c)
!
      type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
!
      if(my_rank .eq. 0) then
        call read_control_add_elegrp(control_file_name, addgrp_c)
      end if
!
      call bcast_control_add_elegrp(addgrp_c)
!
      end subroutine load_control_add_elegrp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_control_add_elegrp(addgrp_c)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
!
      type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
!
      call bcast_ctl_data_4_add_2d_egrp(addgrp_c)
!
      call bcast_ctl_data_4_platform(addgrp_c%source_plt)
      call bcast_ctl_data_4_platform(addgrp_c%added_plt)
!
      call calypso_mpi_bcast_one_int(addgrp_c%i_add_ele_grp_ctl, 0)
!
      end subroutine bcast_control_add_elegrp
!
! -----------------------------------------------------------------------
!
      subroutine bcast_ctl_data_4_add_2d_egrp(addgrp_c)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
!
      call bcast_ctl_array_cr2(addgrp_c%r_ele_grouping_ctl)
      call bcast_ctl_array_cr2(addgrp_c%s_ele_grouping_ctl)
      call bcast_ctl_array_cr2(addgrp_c%t_ele_grouping_ctl)
      call bcast_ctl_array_cr2(addgrp_c%z_ele_grouping_ctl)
!
      call bcast_ctl_type_c1(addgrp_c%sph_grp_direction_ctl)
!
      call calypso_mpi_bcast_one_int(addgrp_c%i_add_ele_grp_para, 0)
!
      end subroutine bcast_ctl_data_4_add_2d_egrp
!
! -----------------------------------------------------------------------
!
      end module bcast_control_add_ele_grp

