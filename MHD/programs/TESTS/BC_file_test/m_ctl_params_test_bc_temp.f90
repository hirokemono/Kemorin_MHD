!m_ctl_params_test_bc_temp.f90
!      module m_ctl_params_test_bc_temp
!
!     Written by H. Matsui on July, 2006
!
!!      subroutine input_control_bc_temp(ctl_file_name, mesh_file)
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
      private :: set_ctl_params_sph_bc_temp
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine input_control_bc_temp(ctl_file_name, mesh_file)
!
      use bcast_ctl_data_test_bc_temp
!
      character(len = kchara), intent(in) :: ctl_file_name
      type(field_IO_params), intent(inout) :: mesh_file
!
      type(ctl_data_bc_temp_test) :: bc_temp_test_ctl
!
!
      if(my_rank .eq. 0) then
        call read_control_4_bc_temp(ctl_file_name, bc_temp_test_ctl)
      end if
!
      call bcast_test_mesh_ctl_data(bc_temp_test_ctl)
!
      if(bc_temp_test_ctl%i_mesh_test_ctl .ne. 1) then
        call calypso_MPI_abort(bc_temp_test_ctl%i_mesh_test_ctl,        &
     &                             'control file is broken')
      end if
!
      call set_ctl_params_sph_bc_temp(bc_temp_test_ctl, mesh_file)
!
      end subroutine input_control_bc_temp
!
!  ---------------------------------------------------------------------
!
      subroutine set_ctl_params_sph_bc_temp                             &
     &         (bc_temp_test_ctl, mesh_file)
!
      use calypso_mpi
      use m_machine_parameter
      use m_file_format_switch
      use t_ctl_data_test_bc_temp
      use set_control_platform_data
!
      type(ctl_data_bc_temp_test), intent(in) :: bc_temp_test_ctl
      type(field_IO_params), intent(inout) :: mesh_file
!
!
      call set_control_parallel_mesh(bc_temp_test_ctl%bc_test_plt,      &
     &                               mesh_file)
      if(iflag_debug.gt.0) write(*,*)                                   &
     &      'mesh_file_head ', mesh_file%file_prefix
!
      np_smp = 1
      if(bc_temp_test_ctl%bc_test_plt%num_smp_ctl%iflag .gt. 0) then
         np_smp = bc_temp_test_ctl%bc_test_plt%num_smp_ctl%intvalue
      end if
      if (iflag_debug.gt.0) write(*,*) 'np_smp', np_smp
!
!
      l_sph_bc = 1
      m_sph_bc = 0
      if(bc_temp_test_ctl%hermonic_degree_ctl%iflag .gt. 0) then
        l_sph_bc = bc_temp_test_ctl%hermonic_degree_ctl%intvalue
      end if
      if(bc_temp_test_ctl%hermonic_order_ctl%iflag .gt. 0) then
        m_sph_bc = bc_temp_test_ctl%hermonic_order_ctl%intvalue
      end if
!
      if(bc_temp_test_ctl%temp_nod_grp_name%iflag .gt. 0) then
        grp_name_nod_bc = bc_temp_test_ctl%temp_nod_grp_name%charavalue
      end if
!
      end subroutine set_ctl_params_sph_bc_temp
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_params_test_bc_temp
