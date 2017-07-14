!
!     module set_ini_sgs_model_coefs_IO
!
!     programmed by H.Matsui in 2005
!     modified by H. Matsui on Aug., 2007
!
!
!!      subroutine set_SPH_Csim_from_IO(Csim_time, Csim_IO, time_d,     &
!!     &          i_step_sgs_coefs, wk_sgs, ierr)
!!        type(time_data), intent(in) :: Csim_time
!!        type(field_IO), intent(in) :: Csim_IO
!!        type(time_data), intent(in) :: time_d
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!      subroutine set_SPH_Csim_to_IO(i_step_sgs_coefs, time_d,         &
!!     &          wk_sgs, Csim_time, Csim_IO)
!!        type(time_data), intent(in) :: time_d
!!        type(dynamic_model_data), intent(in) :: wk_sgs
!!        type(time_data), intent(inout) :: Csim_time
!!        type(field_IO), intent(inout) :: Csim_IO
!!
!!      subroutine set_FEM_Csim_from_IO(Csim_time, Csim_IO, time_d,     &
!!     &          i_step_sgs_coefs, wk_sgs, ierr)
!!      subroutine set_FEM_Csim_to_IO(i_step_sgs_coefs, time_d,         &
!!     &          wk_sgs, Csim_time, Csim_IO)
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(group_data), intent(in) :: layer_egrp
!!        type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!
!!      subroutine set_initial_model_coefs_ele                          &
!!     &         (cmt_param, ele, fluid, layer_egrp,                    &
!!     &          wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      module set_ini_sgs_model_coefs_IO
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
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_SPH_Csim_from_IO(Csim_time, Csim_IO, time_d,       &
     &          i_step_sgs_coefs, wk_sgs, ierr)
!
      type(time_data), intent(in) :: Csim_time
      type(field_IO), intent(in) :: Csim_IO
      type(time_data), intent(in) :: time_d
!
      integer(kind = kint), intent(inout) :: ierr
      integer(kind = kint), intent(inout) :: i_step_sgs_coefs
      type(dynamic_model_data), intent(inout) :: wk_sgs
!
      integer(kind = kint) :: i_fld, j_fld
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
      if(wk_sgs%nlayer .ne. Csim_IO%nnod_IO) then
        e_message = 'number of node in Csim restart file is wrong'
        ierr = 1
      end if
      if(ierr .gt. 0) return
!
      if(i_step_sgs_coefs .eq. 0) then
        i_step_sgs_coefs = int(Csim_time%dt / time_d%dt)
      end if
!
      do i_fld = 1, wk_sgs%num_kinds
        do j_fld = 1, Csim_IO%num_field_IO
          if(Csim_IO%fld_name(j_fld) .eq. wk_sgs%name(i_fld)) then
!$omp parallel workshare
            wk_sgs%fld_coef(:,i_fld) = Csim_IO%d_IO(:,j_fld)
!$omp end parallel workshare
            exit
          end if
        end do
      end do
!
      end subroutine set_SPH_Csim_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine set_SPH_Csim_to_IO(i_step_sgs_coefs, time_d,           &
     &          wk_sgs, Csim_time, Csim_IO)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: i_step_sgs_coefs
      type(time_data), intent(in) :: time_d
      type(dynamic_model_data), intent(in) :: wk_sgs
!
      type(time_data), intent(inout) :: Csim_time
      type(field_IO), intent(inout) :: Csim_IO
!
!
      Csim_time%i_time_step = time_d%i_time_step
      Csim_time%time = time_d%time
      Csim_time%dt = time_d%dt * dble(i_step_sgs_coefs)
!
      Csim_IO%nnod_IO =      wk_sgs%nlayer
      Csim_IO%num_field_IO = wk_sgs%num_kinds
      call alloc_phys_name_IO(Csim_IO)
!
      Csim_IO%fld_name(1:wk_sgs%num_kinds)                              &
     &      = wk_sgs%name(1:wk_sgs%num_kinds)
      Csim_IO%num_comp_IO(1:Csim_IO%num_field_IO) = 1
!
      call s_cal_total_and_stacks                                       &
     &   (Csim_IO%num_field_IO, Csim_IO%num_comp_IO, izero,             &
     &    Csim_IO%istack_comp_IO, Csim_IO%ntot_comp_IO)
!
      call alloc_phys_data_IO(Csim_IO)
!
!$omp parallel workshare
      Csim_IO%d_IO(:,:) = wk_sgs%fld_coef(:,:)
!$omp end parallel workshare
!
      end subroutine set_SPH_Csim_to_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
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
        i_step_sgs_coefs = int(Csim_time%dt / time_d%dt)
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
      subroutine set_FEM_Csim_to_IO(i_step_sgs_coefs, time_d,           &
     &          wk_sgs, Csim_time, Csim_IO)
!
      use cal_minmax_and_stacks
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
      use t_sgs_control_parameter
      use set_sgs_diff_model_coefs
!
      type(commutation_control_params), intent(in) :: cmt_param
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(group_data), intent(in) :: layer_egrp
      type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      integer(kind = kint) :: i, ist
!
      do i = 1, sgs_coefs%num_field
         ist = sgs_coefs%istack_comps(i-1) + 1
         call clear_model_coefs_2_ele(ele, sgs_coefs%num_comps(i), ist, &
     &      sgs_coefs%ntot_comp, sgs_coefs%ak)
         call set_model_coefs_2_ele(ele, izero, sgs_coefs%num_comps(i), &
     &       i, ist, layer_egrp%num_grp, layer_egrp%num_item,           &
     &       layer_egrp%istack_grp_smp, layer_egrp%item_grp,            &
     &       sgs_coefs%num_field, sgs_coefs%ntot_comp,                  &
     &       wk_sgs%fld_clip, wk_sgs%comp_clip, sgs_coefs%ak)
      end do
!
      if (cmt_param%iflag_commute .gt. id_SGS_commute_OFF) then
        if (cmt_param%iset_DIFF_coefs .eq. 0) then
          do i = 1, diff_coefs%num_field
            call set_diff_coefs_whole_ele                               &
     &         (ele, fluid%istack_ele_fld_smp, i, diff_coefs%ntot_comp, &
     &          wk_diff%fld_whole_clip, diff_coefs%ak)
          end do
        else
          do i = 1, diff_coefs%num_field
            call set_diff_coefs_layer_ele                               &
     &         (ele, i, layer_egrp%num_grp, layer_egrp%num_item,        &
     &          layer_egrp%istack_grp_smp, layer_egrp%item_grp,         &
     &          diff_coefs%ntot_comp, wk_diff%fld_clip, diff_coefs%ak)
          end do
        end if
      end if
!
      end subroutine set_initial_model_coefs_ele
!
!-----------------------------------------------------------------------
!
      end module set_ini_sgs_model_coefs_IO
