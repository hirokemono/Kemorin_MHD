!
!     module set_FEM_sgs_model_coefs_IO
!
!     programmed by H.Matsui in 2005
!     modified by H. Matsui on Aug., 2007
!
!
!!      subroutine set_FEM_Csim_from_IO(Csim_time, Csim_IO, time_d,     &
!!     &          i_step_sgs_coefs, wk_sgs, ierr)
!!      subroutine count_FEM_Csim_to_IO(wk_sgs, Csim_IO)
!!      subroutine set_FEM_Csim_to_IO(i_step_sgs_coefs, time_d,         &
!!     &          wk_sgs, Csim_time, Csim_IO)
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(group_data), intent(in) :: layer_egrp
!!        type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_commutation_coefs), intent(inout) :: diff_coefs
!!
!!      subroutine set_initial_model_coefs_ele                          &
!!     &         (cmt_param, ele, fluid, layer_egrp,                    &
!!     &          wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      module set_FEM_sgs_model_coefs_IO
!
      use m_precision
!
      use calypso_mpi
      use m_constants
!
      use t_ele_info_4_dynamic
      use t_SGS_model_coefs
      use t_time_data
      use t_field_data_IO
!
      implicit none
!
      private :: sel_diff_coefs_layer_ele
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_FEM_Csim_from_IO(Csim_time, Csim_IO, time_d,       &
     &          i_step_sgs_coefs, wk_sgs, ierr)
!
      use cal_minmax_and_stacks
!
      type(time_data), intent(in) :: Csim_time
      type(field_IO), intent(in) :: Csim_IO
      type(time_data), intent(in) :: time_d
!
      integer(kind = kint), intent(inout) :: ierr
      integer(kind = kint), intent(inout) :: i_step_sgs_coefs
      type(dynamic_model_data), intent(inout) :: wk_sgs
!
      integer(kind = kint) :: i_fld, j_fld, i
!
!
      ierr = 0
      if(time_d%i_time_step .ne. Csim_time%i_time_step) then
        e_message = 'Time step data in Csim restart file is wrong'
        ierr = 1
      end if
      if(time_d%time .ne. Csim_time%time) then
        e_message = 'Time data in Csim restart file is wrong'
        ierr = 1
      end if
      if(time_d%time .ne. Csim_time%time) then
        e_message = 'Time data in Csim restart file is wrong'
        ierr = 1
      end if
      if(wk_sgs%nlayer-1 .ne. Csim_IO%nnod_IO) then
        e_message = 'number of node in Csim restart file is wrong'
        ierr = 1
      end if
      if(ierr .gt. 0) return
!
      if(i_step_sgs_coefs .eq. 0) then
        i_step_sgs_coefs = int((Csim_time%dt / time_d%dt),              &
     &                    KIND(i_step_sgs_coefs))
      end if
!
      do i_fld = 1, wk_sgs%num_kinds
        do j_fld = 1, Csim_IO%num_field_IO
          if(Csim_IO%fld_name(j_fld) .eq. wk_sgs%name(i_fld)) then
!$omp parallel do
            do i = 1, wk_sgs%nlayer
              wk_sgs%fld_clip(i,i_fld) = Csim_IO%d_IO(i,j_fld)
            end do
!$omp end parallel do
            wk_sgs%fld_whole_clip(i_fld)                                &
     &          = Csim_IO%d_IO(Csim_IO%nnod_IO,j_fld)
            exit
          end if
        end do
      end do
!
      end subroutine set_FEM_Csim_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine count_FEM_Csim_to_IO(wk_sgs, Csim_IO)
!
      use cal_minmax_and_stacks
      use const_global_element_ids
!
      type(dynamic_model_data), intent(in) :: wk_sgs
      type(field_IO), intent(inout) :: Csim_IO
!
!
      Csim_IO%nnod_IO =      wk_sgs%nlayer + 1
      Csim_IO%num_field_IO = wk_sgs%num_kinds
      call alloc_phys_name_IO(Csim_IO)
!
      Csim_IO%fld_name(1:wk_sgs%num_kinds)                              &
     &      = wk_sgs%name(1:wk_sgs%num_kinds)
      Csim_IO%num_comp_IO(1:Csim_IO%num_field_IO) = 1
      call s_cal_total_and_stacks                                       &
     &   (Csim_IO%num_field_IO, Csim_IO%num_comp_IO, izero,             &
     &    Csim_IO%istack_comp_IO, Csim_IO%ntot_comp_IO)
      call alloc_phys_data_IO(Csim_IO)
!
      call alloc_merged_field_stack(nprocs, Csim_IO)
      call count_number_of_node_stack                                   &
     &   (Csim_IO%nnod_IO, Csim_IO%istack_numnod_IO)
!
      end subroutine count_FEM_Csim_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine set_FEM_Csim_to_IO(i_step_sgs_coefs, time_d,           &
     &          wk_sgs, Csim_time, Csim_IO)
!
      integer(kind = kint), intent(in) :: i_step_sgs_coefs
      type(time_data), intent(in) :: time_d
      type(dynamic_model_data), intent(in) :: wk_sgs
!
      type(time_data), intent(inout) :: Csim_time
      type(field_IO), intent(inout) :: Csim_IO
!
      integer(kind = kint) :: i_fld, i
!
!
      Csim_time%i_time_step = time_d%i_time_step
      Csim_time%time = time_d%time
      Csim_time%dt = time_d%dt * dble(i_step_sgs_coefs)
!
      do i_fld = 1, wk_sgs%num_kinds
!$omp parallel do
        do i = 1, wk_sgs%nlayer
          Csim_IO%d_IO(i,i_fld) = wk_sgs%fld_clip(i,i_fld)
        end do
!$omp end parallel do
        Csim_IO%d_IO(Csim_IO%nnod_IO,i_fld)                             &
     &       = wk_sgs%fld_whole_clip(i_fld)
      end do
!
      end subroutine set_FEM_Csim_to_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_initial_model_coefs_ele                            &
     &         (cmt_param, ele, fluid, layer_egrp,                      &
     &          wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      use t_geometry_data
      use t_group_data
      use t_geometry_data_MHD
      use t_SGS_control_parameter
      use set_sgs_diff_model_coefs
!
      type(commutation_control_params), intent(in) :: cmt_param
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(group_data), intent(in) :: layer_egrp
      type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_commutation_coefs), intent(inout) :: diff_coefs
!
!
      call sel_model_coefs_2_ele(ele, layer_egrp, izero,                &
     &                           wk_sgs, sgs_coefs%Csim_SGS_uxb)
      call sel_model_coefs_2_ele(ele, layer_egrp, izero,                &
     &                           wk_sgs, sgs_coefs%Csim_SGS_lor)
      call sel_model_coefs_2_ele(ele, layer_egrp, izero,                &
     &                           wk_sgs, sgs_coefs%Csim_SGS_mf)
      call sel_model_coefs_2_ele(ele, layer_egrp, izero,                &
     &                           wk_sgs, sgs_coefs%Csim_SGS_hf)
      call sel_model_coefs_2_ele(ele, layer_egrp, izero,                &
     &                           wk_sgs, sgs_coefs%Csim_SGS_cf)
!
      if (cmt_param%iflag_commute .gt. id_SGS_commute_OFF) then
        call sel_diff_coefs_layer_ele(cmt_param, ele, fluid,            &
     &      layer_egrp, wk_diff, diff_coefs%Cdiff_velo)
        call sel_diff_coefs_layer_ele(cmt_param, ele, fluid,            &
     &      layer_egrp, wk_diff, diff_coefs%Cdiff_magne)
        call sel_diff_coefs_layer_ele(cmt_param, ele, fluid,            &
     &      layer_egrp, wk_diff, diff_coefs%Cdiff_temp)
        call sel_diff_coefs_layer_ele(cmt_param, ele, fluid,            &
     &      layer_egrp, wk_diff, diff_coefs%Cdiff_light)
!
        call sel_diff_coefs_layer_ele(cmt_param, ele, fluid,            &
     &      layer_egrp, wk_diff, diff_coefs%Cdiff_SGS_uxb)
        call sel_diff_coefs_layer_ele(cmt_param, ele, fluid,            &
     &      layer_egrp, wk_diff, diff_coefs%Cdiff_SGS_lor)
        call sel_diff_coefs_layer_ele(cmt_param, ele, fluid,            &
     &      layer_egrp, wk_diff, diff_coefs%Cdiff_SGS_mf)
        call sel_diff_coefs_layer_ele(cmt_param, ele, fluid,            &
     &      layer_egrp, wk_diff, diff_coefs%Cdiff_SGS_hf)
        call sel_diff_coefs_layer_ele(cmt_param, ele, fluid,            &
     &      layer_egrp, wk_diff, diff_coefs%Cdiff_SGS_cf)
      end if
!
      end subroutine set_initial_model_coefs_ele
!
!-----------------------------------------------------------------------
!
      subroutine sel_diff_coefs_layer_ele                               &
     &         (cmt_param, ele, fluid, layer_egrp, wk_diff, Cdiff)
!
      use t_geometry_data
      use t_group_data
      use t_geometry_data_MHD
      use t_SGS_control_parameter
      use set_sgs_diff_model_coefs
!
      type(commutation_control_params), intent(in) :: cmt_param
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(group_data), intent(in) :: layer_egrp
      type(dynamic_model_data), intent(in) :: wk_diff
!
      type(SGS_model_coefficient), intent(inout) :: Cdiff
!
!
      if(Cdiff%num_comp .le. 0) return
      if(cmt_param%iflag_layerd_DIFF_coefs .eq. 0) then
        call set_diff_coefs_whole_ele(ele, fluid%istack_ele_fld_smp,    &
     &      wk_diff%fld_whole_clip(Cdiff%iak_Csim), Cdiff%coef(1,1))
      else
        call set_diff_coefs_layer_ele                                   &
     &     (ele, layer_egrp%num_grp, layer_egrp%num_item,               &
     &      layer_egrp%istack_grp_smp, layer_egrp%item_grp,             &
     &      wk_diff%fld_clip(1,Cdiff%iak_Csim), Cdiff%coef(1,1))
      end if
!
      end subroutine sel_diff_coefs_layer_ele
!
!-----------------------------------------------------------------------
!
      end module set_FEM_sgs_model_coefs_IO
