!input_control_prod_udt.f90
!      module input_control_prod_udt
!
!      Written by H. Matsui on Nov., 2006
!
!!      subroutine s_input_control_prod_udt(ctl_file_name, prod_udt_c,  &
!!     &                                    futil, t_param)
!!        character(len = kchara), intent(in) ::  ctl_file_name
!!        type(product_udt_ctl), intent(inout) :: prod_udt_c
!!        type(FEM_utils), intent(inout) :: futil
!!        type(time_step_param), intent(inout) :: t_param
!
!
      module input_control_prod_udt
!
      use m_precision
      use calypso_mpi
      use m_machine_parameter
      use t_ctl_data_product_udt
!
      implicit  none
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
      subroutine s_input_control_prod_udt(ctl_file_name, prod_udt_c,    &
     &                                    futil, t_param)
!
      use t_FEM_utils
      use t_step_parameter
      use m_ctl_params_4_prod_udt
!
      character(len = kchara), intent(in) ::  ctl_file_name
      type(product_udt_ctl), intent(inout) :: prod_udt_c
      type(FEM_utils), intent(inout) :: futil
      type(time_step_param), intent(inout) :: t_param
!
      integer(kind = kint) :: ierr
!
!
!  Read control file
      if(my_rank .eq. 0) then
        call read_control_4_prod_udt(ctl_file_name, prod_udt_c)
      end if
!
      call bcast_prod_control_data(prod_udt_c)
!
      if(prod_udt_c%i_prod_control .ne. 1) then
        call calypso_MPI_abort(prod_udt_c%i_prod_control,               &
     &                             'control file is broken')
      end if
!
!  Set control data
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_prod_udt'
      call set_ctl_params_prod_udt(prod_udt_c%pu_plt,                   &
     &    prod_udt_c%org_pu_plt, prod_udt_c%prod_ctl,                   &
     &    futil%mesh_file, futil%udt_file)
      call set_fixed_time_step_params                                   &
     &   (prod_udt_c%prod_ctl%t_pu_ctl, t_param, ierr, e_message)
!
      end subroutine s_input_control_prod_udt
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
      end module input_control_prod_udt
