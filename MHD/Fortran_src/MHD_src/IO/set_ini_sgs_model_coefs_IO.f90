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
!!      subroutine count_SPH_Csim_to_IO(wk_sgs, Csim_IO)
!!      subroutine set_SPH_Csim_to_IO(i_step_sgs_coefs, time_d,         &
!!     &          wk_sgs, Csim_time, Csim_IO)
!!        type(time_data), intent(in) :: time_d
!!        type(dynamic_model_data), intent(in) :: wk_sgs
!!        type(time_data), intent(inout) :: Csim_time
!!        type(field_IO), intent(inout) :: Csim_IO
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
      subroutine count_SPH_Csim_to_IO(wk_sgs, Csim_IO)
!
      use cal_minmax_and_stacks
      use const_global_element_ids
!
      type(dynamic_model_data), intent(in) :: wk_sgs
      type(field_IO), intent(inout) :: Csim_IO
!
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
      call alloc_merged_field_stack(nprocs, Csim_IO)
      call count_number_of_node_stack                                   &
     &   (Csim_IO%nnod_IO, Csim_IO%istack_numnod_IO)
!
      end subroutine count_SPH_Csim_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine set_SPH_Csim_to_IO(i_step_sgs_coefs, time_d,           &
     &          wk_sgs, Csim_time, Csim_IO)
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
!$omp parallel workshare
      Csim_IO%d_IO(:,:) = wk_sgs%fld_coef(:,:)
!$omp end parallel workshare
!
      end subroutine set_SPH_Csim_to_IO
!
! -----------------------------------------------------------------------
!
      end module set_ini_sgs_model_coefs_IO
