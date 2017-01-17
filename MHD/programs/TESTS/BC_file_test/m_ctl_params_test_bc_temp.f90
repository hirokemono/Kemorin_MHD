!m_ctl_params_test_bc_temp.f90
!      module m_ctl_params_test_bc_temp
!
!     Written by H. Matsui on July, 2006
!
!!      subroutine set_ctl_params_sph_bc_temp(mesh_file)
!!        type(field_IO_params), intent(inout) :: mesh_file
!
      module m_ctl_params_test_bc_temp
!
      use m_precision
      use t_file_IO_parameter
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
      subroutine set_ctl_params_sph_bc_temp(mesh_file)
!
      use calypso_mpi
      use m_machine_parameter
      use m_file_format_switch
      use m_ctl_data_test_bc_temp
      use set_control_platform_data
!
      type(field_IO_params), intent(inout) :: mesh_file
!
!
      call set_control_mesh_def(bc_test_plt, mesh_file)
      if(iflag_debug.gt.0) write(*,*)                                   &
     &      'mesh_file_head ', mesh_file%file_prefix
!
      np_smp = 1
      if(bc_test_plt%num_smp_ctl%iflag .gt. 0) then
         np_smp = bc_test_plt%num_smp_ctl%intvalue
      end if
      if (iflag_debug.gt.0) write(*,*) 'np_smp', np_smp
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
