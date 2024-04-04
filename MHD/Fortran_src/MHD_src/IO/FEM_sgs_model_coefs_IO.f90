!>@file   FEM_sgs_model_coefs_IO.f90
!!        module FEM_sgs_model_coefs_IO
!!
!!@author H. Matsui
!!@date   Programmed in 2005
!>        modified in Aug., 2007
!!
!!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine s_output_sgs_model_coefs                             &
!!     &       (i_step_max, MHD_step, SGS_par, MHD_prop, FEM_SGS_wk)
!!        type(time_data), intent(in) :: time_d
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(work_FEM_dynamic_SGS), intent(in) :: FEM_SGS_wk
!!        type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!!@endverbatim
      module FEM_sgs_model_coefs_IO
!
      use m_precision
!
      use calypso_mpi
      use t_physical_property
      use t_SGS_control_parameter
      use t_work_FEM_dynamic_SGS
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_output_sgs_model_coefs                               &
     &       (i_step_max, MHD_step, SGS_par, MHD_prop, FEM_SGS_wk)
!
      use t_control_parameter
      use t_time_data
      use t_MHD_step_parameter
      use m_FEM_sgs_vol_mdl_coefs_IO
      use m_FEM_sgs_lyr_mdl_coefs_IO
!
      integer(kind = kint), intent(in) :: i_step_max
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SGS_paremeters), intent(in) :: SGS_par
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(work_FEM_dynamic_SGS), intent(in) :: FEM_SGS_wk
!
!
      if(SGS_par%model_p%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
      if(output_IO_flag(i_step_max,MHD_step%sgs_IO_step)                &
     &     .eqv. .FALSE.) return
      if(my_rank .ne. 0) return
!
      call output_layered_model_coefs_file                              &
     &   (MHD_step%time_d%i_time_step, MHD_step%time_d%time,            &
     &    SGS_par%model_p, MHD_prop%cd_prop, FEM_SGS_wk%wk_sgs)
      call output_whole_model_coefs_file                                &
     &   (MHD_step%time_d%i_time_step, MHD_step%time_d%time,            &
     &    SGS_par%model_p, MHD_prop%cd_prop, FEM_SGS_wk%wk_sgs)
!
      if (SGS_par%commute_p%iflag_commute .gt. id_SGS_commute_OFF) then
        call output_whole_diff_coefs_file                               &
     &     (MHD_step%time_d%i_time_step, MHD_step%time_d%time,          &
     &      MHD_prop%cd_prop, FEM_SGS_wk%wk_diff)
!
        if (SGS_par%commute_p%iflag_layerd_DIFF_coefs .eq. 1) then
          call output_layered_diff_coefs_file                           &
     &       (MHD_step%time_d%i_time_step, MHD_step%time_d%time,        &
     &        MHD_prop%cd_prop, FEM_SGS_wk%wk_diff)
        end if
      end if
!
      end subroutine s_output_sgs_model_coefs
!
!-----------------------------------------------------------------------
!
      end module FEM_sgs_model_coefs_IO
