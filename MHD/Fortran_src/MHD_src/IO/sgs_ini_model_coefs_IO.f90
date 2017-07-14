!
!     module sgs_ini_model_coefs_IO
!
!     programmed by H.Matsui in 2005
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine read_alloc_SPH_Csim_file                             &
!!     &         (init_d, rst_step, i_step_sgs_coefs, wk_sgs)
!!      subroutine write_SPH_Csim_file                                  &
!!     &        (i_step_sgs_coefs, rst_step, time_d, dynamic_SPH)
!!
!!      subroutine read_alloc_FEM_Csim_file                             &
!!     &         (rst_step, init_d, SGS_param, cmt_param,               &
!!     &          ele, fluid, layer_tbl, i_step_sgs_coefs,              &
!!     &          wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!!        type(IO_step_param), intent(inout) :: rst_step
!!        type(time_data), intent(in) :: init_d
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!      subroutine write_FEM_Csim_file                                  &
!!     &         (i_step_sgs_coefs, time_d, rst_step,                   &
!!     &          SGS_param, cmt_param, wk_sgs, wk_diff)
!
      module sgs_ini_model_coefs_IO
!
      use m_precision
!
      use calypso_mpi
      use m_constants
!
      use t_SGS_control_parameter
      use t_ele_info_4_dynamic
      use t_SGS_model_coefs
      use t_time_data
      use t_field_data_IO
!
      implicit none
!
      integer (kind = kint) :: iflag_rst_sgs_coef_code = 0
      integer (kind = kint) :: iflag_rst_sgs_comm_code = 0
      character(len=kchara), parameter                                  &
     &                      :: def_rst_sgs_coef =  'rst_model_coefs'
      character(len=kchara), parameter                                  &
     &                      :: def_rst_comm_coef = 'rst_diff_coefs'
!
      type(time_data), save :: Csim1_time
      type(field_IO), save :: Csim1_IO
      type(field_IO), save :: Cdiff1_IO
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_SPH_Csim_file                               &
     &         (init_d, rst_step, i_step_sgs_coefs, wk_sgs)
!
      use t_ele_info_4_dynamic
      use field_IO_select
      use set_ini_sgs_model_coefs_IO
!
      type(time_data), intent(in) :: init_d
      type(IO_step_param), intent(inout) :: rst_step
      type(dynamic_model_data), intent(inout) :: wk_sgs
      integer(kind = kint), intent(inout) :: i_step_sgs_coefs
!
      integer(kind = kint) :: ierr, istep_rst
!
!
      if (init_d%i_time_step .eq. -1) then
        istep_rst = init_d%i_time_step
      else
        rst_step%istep_file = init_d%i_time_step / rst_step%increment
        istep_rst = rst_step%istep_file
      end if
!
      ierr = check_step_FEM_field_file(my_rank, istep_rst, Csim1_IO)
      if(ierr .gt. 0) then
        iflag_rst_sgs_coef_code = 0
        return
      end if
!
      call sel_read_alloc_step_FEM_file(nprocs, my_rank,                &
     &    rst_step%istep_file, Csim1_time, Csim1_IO)
!
      call set_SPH_Csim_from_IO(Csim1_time, Csim1_IO, init_d,           &
     &    i_step_sgs_coefs, wk_sgs, ierr)
!
      call dealloc_phys_data_IO(Csim1_IO)
      call dealloc_phys_name_IO(Csim1_IO)
!
      end subroutine read_alloc_SPH_Csim_file
!
! -----------------------------------------------------------------------
!
      subroutine write_SPH_Csim_file                                    &
     &        (i_step_sgs_coefs, rst_step, time_d, dynamic_SPH)
!
      use t_ele_info_4_dynamic
      use sph_filtering
      use field_IO_select
      use cal_minmax_and_stacks
      use set_ini_sgs_model_coefs_IO
!
      integer(kind = kint), intent(in) :: i_step_sgs_coefs
      type(IO_step_param), intent(in) :: rst_step
      type(time_data), intent(in) :: time_d
!
      type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
!
!
      call set_SPH_Csim_to_IO                                           &
     &   (i_step_sgs_coefs, time_d, dynamic_SPH%wk_sgs,                 &
     &    Csim1_time, Csim1_IO)
!
      call sel_write_step_FEM_field_file(nprocs, my_rank,               &
     &    rst_step%istep_file, Csim1_time, Csim1_IO)
!
      call dealloc_phys_data_IO(Csim1_IO)
      call dealloc_phys_name_IO(Csim1_IO)
!
      end subroutine write_SPH_Csim_file
!
! -----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_alloc_FEM_Csim_file                               &
     &         (rst_step, init_d, SGS_param, cmt_param,                 &
     &          ele, fluid, layer_tbl, i_step_sgs_coefs,                &
     &          wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      use t_geometry_data
      use t_geometry_data_MHD
      use t_layering_ele_list
      use field_IO_select
      use set_ini_sgs_model_coefs_IO
!
      type(time_data), intent(in) :: init_d
      type(IO_step_param), intent(in) :: rst_step
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
!
      integer(kind = kint), intent(inout) :: i_step_sgs_coefs
      type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      integer(kind = kint) :: ierr, istep_rst
!
!
      if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
!
      ierr = check_step_FEM_field_file(my_rank, istep_rst, Csim1_IO)
      if(ierr .gt. 0) then
        iflag_rst_sgs_coef_code = 0
        return
      end if
!
      call sel_read_alloc_step_FEM_file(nprocs, my_rank,                &
     &    rst_step%istep_file, Csim1_time, Csim1_IO)
!
      call set_FEM_Csim_from_IO(Csim1_time, Csim1_IO, init_d,           &
     &    i_step_sgs_coefs, wk_sgs, ierr)
!
      call dealloc_phys_data_IO(Csim1_IO)
      call dealloc_phys_name_IO(Csim1_IO)
!
      if (cmt_param%iflag_commute .gt. id_SGS_commute_OFF) then
        call sel_read_alloc_step_FEM_file(nprocs, my_rank,              &
     &      rst_step%istep_file, Csim1_time, Csim1_IO)
!
        call set_FEM_Csim_from_IO(Csim1_time, Cdiff1_IO, init_d,        &
     &      i_step_sgs_coefs, wk_diff, ierr)
!
        call dealloc_phys_data_IO(Cdiff1_IO)
        call dealloc_phys_name_IO(Cdiff1_IO)
      end if
!
      call set_initial_model_coefs_ele                                  &
     &   (cmt_param, ele, fluid, layer_tbl%e_grp,                       &
     &    wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      end subroutine read_alloc_FEM_Csim_file
!
!-----------------------------------------------------------------------
!
      subroutine write_FEM_Csim_file                                    &
     &         (i_step_sgs_coefs, time_d, rst_step,                     &
     &          SGS_param, cmt_param, wk_sgs, wk_diff)
!
      use field_IO_select
      use sgs_model_coefs_IO
      use set_parallel_file_name
      use set_ini_sgs_model_coefs_IO
!
      type(IO_step_param), intent(in) :: rst_step
      type(time_data), intent(in) :: time_d
!
      integer(kind = kint), intent(in) :: i_step_sgs_coefs
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!
!
      if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
      if (my_rank .ne. 0) return
!
      call set_FEM_Csim_to_IO(i_step_sgs_coefs, time_d, wk_sgs,         &
     &   Csim1_time, Csim1_IO)
!
      call sel_write_step_FEM_field_file(nprocs, my_rank,               &
     &    rst_step%istep_file, Csim1_time, Csim1_IO)
!
      call dealloc_phys_data_IO(Csim1_IO)
      call dealloc_phys_name_IO(Csim1_IO)
!
      if (cmt_param%iflag_commute .gt. id_SGS_commute_OFF) then
        call set_FEM_Csim_to_IO(i_step_sgs_coefs, time_d, wk_diff,      &
     &     Csim1_time, Cdiff1_IO)
        call sel_write_step_FEM_field_file(nprocs, my_rank,             &
     &      rst_step%istep_file, Csim1_time, Cdiff1_IO)
!
        call dealloc_phys_data_IO(Cdiff1_IO)
        call dealloc_phys_name_IO(Cdiff1_IO)
      end if
!
      end subroutine write_FEM_Csim_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      end module sgs_ini_model_coefs_IO
