!m_ctl_params_test_bc_temp.f90
!      module m_ctl_params_test_bc_temp
!
!     Written by H. Matsui on July, 2006
!
!      subroutine set_ctl_params_sph_bc_temp
!
      module m_ctl_params_test_bc_temp
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: l_sph_bc =  1
      integer(kind = kint) :: m_sph_bc = 0
      integer(kind = kint) :: igrp_nod_bc
      character(len = kchara) :: grp_name_nod_bc = 'CMB'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_sph_bc_temp
!
      use calypso_mpi
      use m_machine_parameter
      use m_file_format_switch
      use m_read_mesh_data
      use m_ctl_data_4_platforms
      use m_ctl_data_test_bc_temp
!
!
      if (i_mesh_header .ne. 0) then
        mesh_file_head = mesh_file_prefix
      else
        mesh_file_head = def_mesh_file_head
      end if
      if(iflag_debug.gt.0) write(*,*) 'mesh_file_head ', mesh_file_head
!
      np_smp = num_smp_ctl
      if (iflag_debug.gt.0) write(*,*) 'np_smp', np_smp
!
      call choose_file_format(mesh_file_fmt_ctl, i_mesh_file_fmt,       &
     &          iflag_mesh_file_fmt)
!
!
      l_sph_bc = hermonic_degree_ctl
      m_sph_bc = hermonic_order_ctl
!
      if(i_nod_grp_t .gt. 0) then
        grp_name_nod_bc = temp_nod_grp_name
      end if
!
      end subroutine set_ctl_params_sph_bc_temp
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_params_test_bc_temp
