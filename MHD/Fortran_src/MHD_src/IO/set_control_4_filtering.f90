!>@file   set_control_4_filtering.f90
!!@brief  module set_control_4_filtering
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July, 2012
!
!> @brief set filtering parameters for SGS model
!!        from control data
!!
!!@verbatim
!!     subroutine s_set_control_4_filtering
!!@endverbatim
!
      module set_control_4_filtering
!
      use m_precision
!
      implicit  none
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_filtering
!
      use m_constants
      use m_machine_parameter
      use m_parallel_var_dof
      use m_file_format_switch
      use m_phys_labels
      use m_control_parameter
      use m_ctl_data_SGS_model
      use m_filter_file_names
      use m_ctl_data_filter_files
      use sgs_ini_model_coefs_IO
      use set_control_ele_layering
!
      integer(kind = kint) :: i
!
!
      if (i_SGS_filter .gt. 0) then
        if   (SGS_filter_name_ctl .eq. '3d'                             &
     &   .or. SGS_filter_name_ctl .eq. '3D'                             &
     &   .or. SGS_filter_name_ctl .eq. '3-D'                            &
     &   .or. SGS_filter_name_ctl .eq. '3-d'                            &
     &   .or. SGS_filter_name_ctl .eq. '3-dimensional'                  &
     &   .or. SGS_filter_name_ctl .eq. '3-Dimensional'                  &
     &   .or. SGS_filter_name_ctl .eq. '3-DIMENSIONAL'                  &
     &  )  iflag_SGS_filter = 1
!
        if   (SGS_filter_name_ctl .eq. 'line'                           &
     &   .or. SGS_filter_name_ctl .eq. 'Line'                           &
     &   .or. SGS_filter_name_ctl .eq. 'LINE'                           &
     &  )  iflag_SGS_filter = 2
!
        if   (SGS_filter_name_ctl .eq. 'plane'                          &
     &   .or. SGS_filter_name_ctl .eq. 'Plane'                          &
     &   .or. SGS_filter_name_ctl .eq. 'PLANE'                          &
     &  )  iflag_SGS_filter = 3
!
        if   (SGS_filter_name_ctl .eq. '3d_easy'                        &
     &   .or. SGS_filter_name_ctl .eq. '3D_easy'                        &
     &   .or. SGS_filter_name_ctl .eq. '3D_EASY'                        &
     &  )  iflag_SGS_filter = 11
!
        if   (SGS_filter_name_ctl .eq. '3d-smp'                         &
     &   .or. SGS_filter_name_ctl .eq. '3D-smp'                         &
     &   .or. SGS_filter_name_ctl .eq. '3D-SMP'                         &
     &   .or. SGS_filter_name_ctl .eq. '3-D-smp'                        &
     &   .or. SGS_filter_name_ctl .eq. '3-d-smp'                        &
     &   .or. SGS_filter_name_ctl .eq. '3-dimensional-smp'              &
     &   .or. SGS_filter_name_ctl .eq. '3-Dimensional-smp'              &
     &   .or. SGS_filter_name_ctl .eq. '3-DIMENSIONAL-SMP'              &
     &  )  iflag_SGS_filter = 21
!
        if   (SGS_filter_name_ctl .eq. '3d_easy_smp'                    &
     &   .or. SGS_filter_name_ctl .eq. '3D_easy_smp'                    &
     &   .or. SGS_filter_name_ctl .eq. '3D_EASY_SMP'                    &
     &  )  iflag_SGS_filter = 31
!
        if (iflag_debug .gt. 0)  write(*,*)                             &
     &       'iflag_SGS_filter',     iflag_SGS_filter
      end if
!
      if (iflag_SGS_model.eq.id_SGS_similarity                          &
     &     .or. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_SGS_filter .eq. 0) then
          e_message = 'Set filtering type for dynamic model'
          call parallel_abort(90, e_message)
        end if
      end if
!
!
!
      if (mod(iflag_SGS_filter,10) .eq. 1) then
        if (i_num_whole_filter_grp .gt. 0) then
          num_whole_filter_grp =   num_whole_filter_grp_ctl
        else
          num_whole_filter_grp =   1
        end if
        num_whole_w_filter_grp = num_whole_filter_grp
!
        allocate(whole_filter_grp(num_whole_filter_grp))
        allocate(id_whole_filter_grp(num_whole_filter_grp))
        allocate(whole_w_filter_grp(num_whole_w_filter_grp))
        allocate(id_whole_w_filter_grp(num_whole_w_filter_grp))
        id_whole_filter_grp =   0
        id_whole_w_filter_grp = 0
!
        if (i_num_whole_filter_grp .gt. 0) then
          whole_filter_grp(1:num_whole_filter_grp)                      &
     &         = whole_filter_grp_ctl(1:num_whole_filter_grp)
        else
          whole_filter_grp(1) =   'all'
          whole_w_filter_grp(1) = 'all'
        end if
        whole_w_filter_grp(1:num_whole_filter_grp)                      &
     &         = whole_filter_grp(1:num_whole_filter_grp)
!
!
        if (i_num_fluid_filter_grp .gt. 0) then
          num_fluid_filter_grp = num_fluid_filter_grp_ctl
        else
          num_fluid_filter_grp = 1
        end if
        num_fluid_w_filter_grp = num_fluid_filter_grp
!
        allocate(fluid_filter_grp(num_fluid_filter_grp))
        allocate(id_fluid_filter_grp(num_fluid_filter_grp))
        allocate(fluid_w_filter_grp(num_fluid_w_filter_grp))
        allocate(id_fluid_w_filter_grp(num_fluid_w_filter_grp))
        id_fluid_filter_grp =   0
        id_fluid_w_filter_grp = 0
!
        if (i_num_fluid_filter_grp .gt. 0) then
          fluid_filter_grp(1:num_fluid_filter_grp)                      &
     &         = fluid_filter_grp_ctl(1:num_fluid_filter_grp)
        else
          fluid_filter_grp(1) = 'all'
        end if
        fluid_w_filter_grp(1:num_fluid_filter_grp)                      &
     &         = fluid_filter_grp(1:num_fluid_filter_grp)
!
        if ( iflag_t_evo_4_temp.gt.0 ) then
          if (     heat_filter_ctl .eq. 'whole_filtering'               &
     &      .or.   heat_filter_ctl .eq. 'Whole_filtering'               &
     &      .or.   heat_filter_ctl .eq. 'WHOLE_FILTERING') then
            iflag_heat_filtering = 0
          else if (heat_filter_ctl .eq. 'fluid_filtering'               &
     &      .or.   heat_filter_ctl .eq. 'Fluid_filtering'               &
     &      .or.   heat_filter_ctl .eq. 'FLUID_FILTERING') then
            iflag_heat_filtering = 1
          else
            iflag_heat_filtering = 0
          end if
        end if
!
        if ( iflag_t_evo_4_velo.gt.0 ) then
          if (     momentum_filter_ctl .eq. 'whole_filtering'           &
     &      .or.   momentum_filter_ctl .eq. 'Whole_filtering'           &
     &      .or.   momentum_filter_ctl .eq. 'WHOLE_FILTERING') then
            iflag_momentum_filtering = 0
          else if (momentum_filter_ctl .eq. 'fluid_filtering'           &
     &      .or.   momentum_filter_ctl .eq. 'Fluid_filtering'           &
     &      .or.   momentum_filter_ctl .eq. 'FLUID_FILTERING') then
            iflag_momentum_filtering = 1
          else
            iflag_momentum_filtering = 0
          end if
        end if
!
        if ( (iflag_t_evo_4_magne+iflag_t_evo_4_vect_p) .gt. 0 ) then
          if (     induction_filter_ctl .eq. 'whole_filtering'          &
     &      .or.   induction_filter_ctl .eq. 'Whole_filtering'          &
     &      .or.   induction_filter_ctl .eq. 'WHOLE_FILTERING') then
            iflag_induction_filtering = 0
          else if (induction_filter_ctl .eq. 'fluid_filtering'          &
     &      .or.   induction_filter_ctl .eq. 'Fluid_filtering'          &
     &      .or.   induction_filter_ctl .eq. 'FLUID_FILTERING') then
            iflag_induction_filtering = 1
          else
            iflag_induction_filtering = 0
          end if
        end if
!
        if (iflag_debug.eq.1) then
          write(*,*) 'num_whole_filter_grp', num_whole_filter_grp
          write(*,*) 'whole_filter_grp'
          do i = 1, num_whole_filter_grp
            write(*,*) i, trim(whole_filter_grp(i))
          end do
          write(*,*) 'num_fluid_filter_grp', num_fluid_filter_grp
          write(*,*) 'fluid_filter_grp'
          do i = 1, num_fluid_filter_grp
            write(*,*) i, trim(fluid_filter_grp(i))
          end do
          write(*,*) 'iflag_heat_filtering',                            &
     &              iflag_heat_filtering
          write(*,*) 'iflag_momentum_filtering',                        &
     &              iflag_momentum_filtering
          write(*,*) 'iflag_induction_filtering',                       &
     &              iflag_induction_filtering
        end if
!
!     set filter file header
!
        if (i_filter_head_ctl .eq. 1) then
          filter_3d_head = filter_head_ctl
        else
          filter_3d_head = filter_3d_def_hd
        end if
!
        if (i_filter_wide_head .eq. 1) then
          filter_wide_head = filter_wide_head_ctl
        else
          filter_wide_head = filter_wide_def_hd
        end if
!
        call choose_file_format(filter_3d_format, i_filter_3d_fmt,      &
     &      ifmt_3d_filter)
        call choose_file_format(filter_wide_format, i_filter_wide_fmt,  &
     &      ifmt_wide_filter)
!
        if (iflag_debug .gt. 0)  then
          write(*,*) 'filter_3d_head: ',     trim(filter_3d_head)
          write(*,*) 'filter_3d_format: ',   ifmt_3d_filter
          write(*,*) 'filter_wide_head: ',   trim(filter_wide_head)
          write(*,*) 'filter_wide_format: ', ifmt_wide_filter
        end if
      end if
!
      if (iflag_SGS_filter .eq. 2) then
        if (i_filter_head_ctl .eq. 1) then
          filter_line_head = filter_head_ctl
        else
          filter_line_head = filter_l_def_hd
        end if
      end if
!
!
      end subroutine s_set_control_4_filtering
!
! -----------------------------------------------------------------------
!
      end module set_control_4_filtering
