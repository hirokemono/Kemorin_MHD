!bcast_ctl_data_diff_udt.f90
!      module bcast_ctl_data_diff_udt
!
!      Written by H. Matsui on Nov., 2006
!
!     required module for 3rd level
!
!!      subroutine laod_control_4_diff_udt                              &
!!     &         (file_name, hd_block, diff_udt_c)
!!      subroutine dealloc_diff_control_data(diff_udt_c)
!
      module bcast_ctl_data_diff_udt
!
      use m_precision
      use m_machine_parameter
      use t_ctl_data_diff_udt
      use calypso_mpi
!
!
      implicit  none
!
      private :: bcast_diff_control_data
      private :: bcast_diff_files_ctl, bcast_diff_model_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine laod_control_4_diff_udt                                &
     &         (file_name, hd_block, diff_udt_c)
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
      end subroutine laod_control_4_diff_udt
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_diff_control_data(diff_udt_c)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
!
      type(diff_udt_ctl), intent(inout)  :: diff_udt_c
!
!
      call bcast_diff_files_ctl(diff_udt_c%diff_ctl)
      call bcast_diff_model_ctl(diff_udt_c%diff_ctl)
!
      call bcast_ctl_data_4_platform(diff_udt_c%d_plt)
      call bcast_ctl_data_4_platform(diff_udt_c%org_d_plt)
!
      call calypso_mpi_bcast_one_int(diff_udt_c%i_diff_control, 0)
!
      end subroutine bcast_diff_control_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_diff_files_ctl(diff_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(diff_model_ctl), intent(inout)  :: diff_ctl
!
!
      call bcast_ctl_type_c1(diff_ctl%ref_udt_head_ctl)
      call bcast_ctl_type_c1(diff_ctl%tgt_udt_head_ctl)
!
      call calypso_mpi_bcast_one_int(diff_ctl%i_diff_files, 0)
!
      end subroutine bcast_diff_files_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_diff_model_ctl(diff_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
      use bcast_4_field_ctl
      use bcast_4_time_step_ctl
      use bcast_4_filter_files_ctl
      use bcast_4_fem_int_pts_ctl
!
      type(diff_model_ctl), intent(inout)  :: diff_ctl
!
!
      call bcast_phys_data_ctl(diff_ctl%fld_d_ctl)
      call bcast_ctl_data_4_time_step(diff_ctl%t_d_ctl)
      call bcast_ele_layers_control(diff_ctl%elayer_d_ctl)
      call bcast_control_fem_int_points(diff_ctl%fint_d_ctl)
!
      call bcast_ctl_type_c1(diff_ctl%product_field_ctl)
      call bcast_ctl_type_c1(diff_ctl%correlate_fld_ctl)
      call bcast_ctl_type_c1(diff_ctl%correlate_cmp_ctl)
      call bcast_ctl_type_c1(diff_ctl%correlate_coord_ctl)
      call bcast_ctl_type_c1(diff_ctl%group_mesh_head_ctl)
!
      call calypso_mpi_bcast_one_int(diff_ctl%i_diff_model, 0)
!
      end subroutine bcast_diff_model_ctl
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_data_diff_udt
