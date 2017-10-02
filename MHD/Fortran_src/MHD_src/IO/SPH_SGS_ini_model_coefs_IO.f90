!
!     module SPH_SGS_ini_model_coefs_IO

!
!     programmed by H.Matsui in 2005
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine read_alloc_SPH_Csim_file                             &
!!     &         (Csim_file_IO, init_d, rst_step, i_step_sgs_coefs,     &
!!     &          SGS_param, dynamic_SPH)
!!        type(field_IO_params), intent(in) :: Csim_file_IO
!!        type(time_data), intent(in) :: init_d
!!        type(IO_step_param), intent(inout) :: rst_step
!!        type(SGS_model_control_params), intent(inout) :: SGS_param
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!      subroutine init_SPH_Csim_file(dynamic_SPH)
!!      subroutine write_SPH_Csim_file(i_step_sgs_coefs, Csim_file_IO,  &
!!     &          rst_step, time_d, dynamic_SPH)
!!        type(field_IO_params), intent(in) :: Csim_file_IO
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(time_data), intent(in) :: time_d
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!
      module SPH_SGS_ini_model_coefs_IO
!
      use m_precision
!
      use calypso_mpi
      use m_constants
!
      use t_SGS_control_parameter
      use t_sph_filtering
      use t_SGS_model_coefs
      use t_file_IO_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_SPH_Csim_file                               &
     &         (Csim_file_IO, init_d, rst_step, i_step_sgs_coefs,       &
     &          SGS_param, dynamic_SPH)
!
      use t_ele_info_4_dynamic
      use field_IO_select
      use set_ini_sgs_model_coefs_IO
!
      type(field_IO_params), intent(in) :: Csim_file_IO
      type(time_data), intent(in) :: init_d
      type(IO_step_param), intent(in) :: rst_step
      type(SGS_model_control_params), intent(inout) :: SGS_param
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      integer(kind = kint), intent(inout) :: i_step_sgs_coefs
!
      integer(kind = kint) :: ierr
!
!
      ierr = check_step_FEM_field_file(my_rank,                         &
     &      rst_step%istep_file, Csim_file_IO)
      if(ierr .gt. 0) then
        SGS_param%iflag_rst_sgs_coef_code = 0
        return
      end if
!
      call sel_read_alloc_step_FEM_file(nprocs, my_rank,                &
     &    rst_step%istep_file, Csim_file_IO,                            &
     &    dynamic_SPH%Csim_time_S_IO, dynamic_SPH%Csim_S_IO)
!
      call set_SPH_Csim_from_IO                                         &
     &   (dynamic_SPH%Csim_time_S_IO, dynamic_SPH%Csim_S_IO,            &
     &    init_d, i_step_sgs_coefs, dynamic_SPH%wk_sgs, ierr)
!
      call dealloc_phys_data_IO(dynamic_SPH%Csim_S_IO)
      call dealloc_phys_name_IO(dynamic_SPH%Csim_S_IO)
!
      end subroutine read_alloc_SPH_Csim_file
!
! -----------------------------------------------------------------------
!
      subroutine init_SPH_Csim_file(dynamic_SPH)
!
      use t_sph_filtering
      use set_ini_sgs_model_coefs_IO
!
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      call count_SPH_Csim_to_IO                                         &
     &   (dynamic_SPH%wk_sgs, dynamic_SPH%Csim_S_IO)
!
      end subroutine init_SPH_Csim_file
!
!-----------------------------------------------------------------------
!
      subroutine write_SPH_Csim_file(i_step_sgs_coefs, Csim_file_IO,    &
     &          rst_step, time_d, dynamic_SPH)
!
      use t_ele_info_4_dynamic
      use t_sph_filtering
      use field_IO_select
      use cal_minmax_and_stacks
      use set_ini_sgs_model_coefs_IO
!
      integer(kind = kint), intent(in) :: i_step_sgs_coefs
      type(field_IO_params), intent(in) :: Csim_file_IO
      type(IO_step_param), intent(in) :: rst_step
      type(time_data), intent(in) :: time_d
!
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      call set_SPH_Csim_to_IO                                           &
     &   (i_step_sgs_coefs, time_d, dynamic_SPH%wk_sgs,                 &
     &    dynamic_SPH%Csim_time_S_IO, dynamic_SPH%Csim_S_IO)
!
      call sel_write_step_FEM_field_file(nprocs, my_rank,               &
     &    rst_step%istep_file, Csim_file_IO,                            &
     &    dynamic_SPH%Csim_time_S_IO, dynamic_SPH%Csim_S_IO)
!
      end subroutine write_SPH_Csim_file
!
! -----------------------------------------------------------------------
!
      end module SPH_SGS_ini_model_coefs_IO
