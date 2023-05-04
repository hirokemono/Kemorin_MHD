!bcast_ctl_data_test_bc_temp.f90
!      module bcast_ctl_data_test_bc_temp
!
!      Written by H. Matsui on July, 2006
!      Mmodified by H. Matsui on June, 2007
!
!!      subroutine load_control_4_bc_temp(bc_temp_test_ctl)
!!        type(ctl_data_bc_temp_test), intent(inout) :: bc_temp_test_ctl
!!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin mesh_test
!!    begin data_files_def
!!      num_subdomain_ctl           2
!!      num_smp_ctl                 1
!!  
!!      mesh_file_prefix         'mesh/in'
!!      mesh_file_fmt_ctl        'gzip'
!!    end data_files_def
!!  
!!    begin boundary_ctl
!!      node_grp_name_ctl      'CMB'
!!      harmonics_degree_ctl      2
!!      harmonics_order_ctl      -2
!!    end boundary_ctl
!!  end mesh_test
!!
!!    -------------------------------------------------------------------
!
      module bcast_ctl_data_test_bc_temp
!
      use m_precision
      use calypso_mpi
      use m_machine_parameter
      use t_ctl_data_test_bc_temp
!
      implicit  none
!
      character(len = kchara), parameter                                &
     &                        :: fname_test_mesh_ctl = "ctl_bc_temp"
!
      private :: bcast_test_mesh_ctl_data
      private :: bcast_ctl_data_4_temp_nod_bc
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine load_control_4_bc_temp(bc_temp_test_ctl)
!
      use skip_comment_f
!
      type(ctl_data_bc_temp_test), intent(inout) :: bc_temp_test_ctl
!
!
      if(my_rank .eq. 0) then
        call read_control_4_bc_temp(fname_test_mesh_ctl,                &
     &                              bc_temp_test_ctl)
      end if
!
      call bcast_test_mesh_ctl_data(bc_temp_test_ctl)
!
      end subroutine load_control_4_bc_temp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_test_mesh_ctl_data(bc_temp_test_ctl)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
!
      type(ctl_data_bc_temp_test), intent(inout) :: bc_temp_test_ctl
!
!
      call bcast_ctl_data_4_temp_nod_bc(bc_temp_test_ctl)
      call bcast_ctl_data_4_platform(bc_temp_test_ctl%bc_test_plt)
!
      call calypso_mpi_bcast_one_int                                    &
     &   (bc_temp_test_ctl%i_mesh_test_ctl, 0)
!
      end subroutine bcast_test_mesh_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_data_4_temp_nod_bc(bc_temp_test_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(ctl_data_bc_temp_test), intent(inout) :: bc_temp_test_ctl
!
!
      call bcast_ctl_type_c1(bc_temp_test_ctl%temp_nod_grp_name)
      call bcast_ctl_type_i1(bc_temp_test_ctl%hermonic_degree_ctl)
      call bcast_ctl_type_i1(bc_temp_test_ctl%hermonic_order_ctl)
!
      call calypso_mpi_bcast_one_int(bc_temp_test_ctl%i_bc_def, 0)
!
      end subroutine bcast_ctl_data_4_temp_nod_bc
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_data_test_bc_temp
