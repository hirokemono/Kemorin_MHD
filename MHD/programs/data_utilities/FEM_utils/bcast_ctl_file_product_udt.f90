!bcast_ctl_file_product_udt.f90
!      module bcast_ctl_file_product_udt
!
!      Written by H. Matsui on Nov., 2006
!
!!      subroutine load_control_4_prod_udt(prod_udt_c)
!!        type(product_udt_ctl), intent(inout) :: prod_udt_c
!
!
      module bcast_ctl_file_product_udt
!
      use m_precision
      use calypso_mpi
      use m_machine_parameter
      use t_ctl_data_product_udt
!
!
      implicit  none
!
!
      character(len = kchara), parameter                                &
     &                 :: fname_prod_ctl = "ctl_prod_udt"
!
      private :: bcast_prod_control_data
      private :: bcast_prod_files_ctl, bcast_product_model_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine load_control_4_prod_udt(prod_udt_c)
!
      type(product_udt_ctl), intent(inout) :: prod_udt_c
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        call read_control_4_prod_udt(fname_prod_ctl, prod_udt_c)
      end if
!
      call bcast_prod_control_data(prod_udt_c)
!
      end subroutine load_control_4_prod_udt
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_prod_control_data(prod_udt_c)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
!
      type(product_udt_ctl), intent(inout) :: prod_udt_c
!
!
      call bcast_prod_files_ctl(prod_udt_c%prod_ctl)
      call bcast_product_model_ctl(prod_udt_c%prod_ctl)
!
      call bcast_ctl_data_4_platform(prod_udt_c%pu_plt)
      call bcast_ctl_data_4_platform(prod_udt_c%org_pu_plt)
!
      call calypso_mpi_bcast_one_int(prod_udt_c%i_prod_control, 0)
!
      end subroutine bcast_prod_control_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_prod_files_ctl(prod_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(product_model_ctl), intent(inout) :: prod_ctl
!
!
      call bcast_ctl_type_c1(prod_ctl%product_udt_1_head_ctl)
      call bcast_ctl_type_c1(prod_ctl%product_udt_2_head_ctl)
!
      call calypso_mpi_bcast_one_int(prod_ctl%i_prod_files, 0)
!
      end subroutine bcast_prod_files_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_product_model_ctl(prod_ctl)
!
      use calypso_mpi_int
      use bcast_4_time_step_ctl
      use bcast_control_arrays
!
      type(product_model_ctl), intent(inout) :: prod_ctl
!
!
      call bcast_ctl_data_4_time_step(prod_ctl%t_pu_ctl)
!
      call bcast_ctl_type_c1(prod_ctl%result_field_ctl)
      call bcast_ctl_type_c1(prod_ctl%product_field_1_ctl)
      call bcast_ctl_type_c1(prod_ctl%product_field_2_ctl)
      call bcast_ctl_type_c1(prod_ctl%product_type_ctl)
!
      call calypso_mpi_bcast_one_int(prod_ctl%i_prod_model, 0)
!
      end subroutine bcast_product_model_ctl
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_file_product_udt
