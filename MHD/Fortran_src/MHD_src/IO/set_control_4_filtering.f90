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
!!      subroutine s_set_control_4_filtering                            &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop, SGS_param,        &
!!     &          SGS_filter_name_ctl, ffile_ctl, s3df_ctl, filter_param)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(read_character_item), intent(in) :: SGS_filter_name_ctl
!!        type(filter_file_control), intent(in) :: ffile_ctl
!!        type(SGS_3d_filter_control), intent(inout) :: s3df_ctl
!!        type(SGS_filtering_params), intent(inout) :: filter_param
!!@endverbatim
!
      module set_control_4_filtering
!
      use m_precision
      use calypso_mpi
      use m_constants
      use m_error_IDs
      use m_machine_parameter
!
      implicit  none
!
      private :: set_control_filter_area, filter_area_4_each_field
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_filtering                              &
     &         (fl_prop, cd_prop, ht_prop, cp_prop, SGS_param,          &
     &          SGS_filter_name_ctl, ffile_ctl, s3df_ctl, filter_param)
!
      use m_file_format_switch
      use m_phys_labels
      use m_filter_file_names
      use t_physical_property
      use t_SGS_control_parameter
      use t_ctl_data_SGS_filter
      use t_ctl_data_filter_files
      use t_read_control_arrays
      use SPH_SGS_ini_model_coefs_IO
      use set_control_ele_layering
      use skip_comment_f
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(read_character_item), intent(in) :: SGS_filter_name_ctl
      type(filter_file_control), intent(in) :: ffile_ctl
      type(SGS_3d_filter_control), intent(inout) :: s3df_ctl
      type(SGS_filtering_params), intent(inout) :: filter_param
!
      integer(kind = kint) :: iflag
      character(len=kchara) :: tmpchara
!
!
      if (SGS_filter_name_ctl%iflag .gt. 0) then
        tmpchara = SGS_filter_name_ctl%charavalue
        if   (cmp_no_case(tmpchara, '3D')                               &
     &   .or. cmp_no_case(tmpchara, '3-D')                              &
     &   .or. cmp_no_case(tmpchara, '3-Dimensional')                    &
     &  )  filter_param%iflag_SGS_filter = id_SGS_3D_FILTERING
!
        if   (cmp_no_case(tmpchara, 'Line')                             &
     &  )  filter_param%iflag_SGS_filter = id_SGS_LINE_FILTERING
!
        if   (cmp_no_case(tmpchara, 'Plane')                            &
     &  )  filter_param%iflag_SGS_filter = id_SGS_PLANE_FILTERING
!
        if   (cmp_no_case(tmpchara, '3D_easy')                          &
     &  )  filter_param%iflag_SGS_filter = id_SGS_3D_EZ_FILTERING
!
        if   (cmp_no_case(tmpchara, '3D_smp')                           &
     &   .or. cmp_no_case(tmpchara, '3-D_smp')                          &
     &   .or. cmp_no_case(tmpchara, '3-Dimensional_smp')                &
     &       )  filter_param%iflag_SGS_filter = id_SGS_3D_SMP_FILTERING
!
        if   (cmp_no_case(tmpchara, '3D_easy_sm')                       &
     &  )  filter_param%iflag_SGS_filter = id_SGS_3D_EZ_SMP_FILTERING
!
        if (iflag_debug .gt. 0)  write(*,*)                             &
     &       'iflag_SGS_filter', filter_param%iflag_SGS_filter
      end if
!
      if       (SGS_param%iflag_SGS.eq.id_SGS_similarity                &
     &     .or. SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        if(filter_param%iflag_SGS_filter .eq. id_SGS_NO_FILTERING) then
          e_message = 'Set filtering type for dynamic model'
          call calypso_MPI_abort(ierr_SGS, e_message)
        end if
      end if
!
!
!
      iflag = filter_param%iflag_SGS_filter
      if    (iflag .eq. id_SGS_3D_FILTERING                             &
     &  .or. iflag .eq. id_SGS_3D_EZ_FILTERING                          &
     &  .or. iflag .eq. id_SGS_3D_SMP_FILTERING                         &
     &  .or. iflag .eq. id_SGS_3D_EZ_SMP_FILTERING ) then
!
        if (iflag_debug.eq.1) write(*,*) 'whole_filter_grp'
        call set_control_filter_area                                    &
     &         (s3df_ctl%whole_filter_grp_ctl, filter_param%whole)
!
        if (iflag_debug.eq.1) write(*,*) 'whole_filter_grp'
        call set_control_filter_area                                    &
     &         (s3df_ctl%fluid_filter_grp_ctl, filter_param%fluid)
!
!
        if (ht_prop%iflag_scheme .gt. id_no_evolution) then
          filter_param%iflag_heat_filtering                             &
     &       = filter_area_4_each_field(s3df_ctl%heat_filter_ctl)
        end if
!
        if (cp_prop%iflag_scheme .gt. id_no_evolution) then
          filter_param%iflag_composition_filtering                      &
     &       = filter_area_4_each_field(s3df_ctl%compostion_filter_ctl)
        end if
!
        if (fl_prop%iflag_scheme .gt. id_no_evolution) then
          filter_param%iflag_momentum_filtering                         &
     &        = filter_area_4_each_field(s3df_ctl%momentum_filter_ctl)
        end if
!
        if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution              &
     &      .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
          filter_param%iflag_induction_filtering                        &
     &        = filter_area_4_each_field(s3df_ctl%induction_filter_ctl)
        end if
!
        if (iflag_debug.eq.1) then
          write(*,*) 'iflag_heat_filtering',                            &
     &              filter_param%iflag_heat_filtering
          write(*,*) 'iflag_momentum_filtering',                        &
     &              filter_param%iflag_momentum_filtering
          write(*,*) 'iflag_induction_filtering',                       &
     &              filter_param%iflag_induction_filtering
          write(*,*) 'iflag_composition_filtering',                     &
     &              filter_param%iflag_composition_filtering
        end if
!
!     set filter file header
!
        if (ffile_ctl%filter_head_ctl%iflag .eq. 1) then
          filter_3d_head = ffile_ctl%filter_head_ctl%charavalue
        end if
!
        if (ffile_ctl%filter_wide_head_ctl%iflag .eq. 1) then
          filter_wide_head = ffile_ctl%filter_wide_head_ctl%charavalue
        end if
!
        call choose_file_format                                         &
     &     (ffile_ctl%filter_3d_format, ifmt_3d_filter)
        call choose_file_format                                         &
     &     (ffile_ctl%filter_wide_format, ifmt_wide_filter)
!
        if (iflag_debug .gt. 0)  then
          write(*,*) 'filter_3d_head: ',     trim(filter_3d_head)
          write(*,*) 'filter_3d_format: ',   ifmt_3d_filter
          write(*,*) 'filter_wide_head: ',   trim(filter_wide_head)
          write(*,*) 'filter_wide_format: ', ifmt_wide_filter
        end if
      end if
!
      if (filter_param%iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
        if (ffile_ctl%filter_head_ctl%iflag .eq. 1) then
          filter_line_head = ffile_ctl%filter_head_ctl%charavalue
        else
          filter_line_head = filter_l_def_hd
        end if
      end if
!
!
      end subroutine s_set_control_4_filtering
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_filter_area(filter_grp_ctl, f_area)
!
      use t_SGS_control_parameter
      use t_read_control_arrays
!
      type(ctl_array_chara), intent(inout)  :: filter_grp_ctl
      type(SGS_filter_area_params), intent(inout) :: f_area
!
      integer(kind = kint) :: i
!
!
      if (filter_grp_ctl%icou .eq. 0) filter_grp_ctl%num =   1
      call alloc_filter_group_param(filter_grp_ctl%num, f_area)
!
      if (filter_grp_ctl%icou .gt. 0) then
        do i = 1, f_area%num_f_group
          f_area%f_gourp_name(i)  = filter_grp_ctl%c_tbl(i)
        end do
      else
        f_area%f_gourp_name(1) =   'all'
      end if
!
      if (iflag_debug.eq.1) then
        write(*,*) 'num_f_group', f_area%num_f_group
        do i = 1, f_area%num_f_group
          write(*,*) i, trim(f_area%f_gourp_name(i))
        end do
      end if
        call dealloc_control_array_chara(filter_grp_ctl)
!
      end subroutine set_control_filter_area
!
! -----------------------------------------------------------------------
!
      function filter_area_4_each_field(each_filter_ctl)
!
      use t_control_elements
      use skip_comment_f
!
      type(read_character_item), intent(in) :: each_filter_ctl
      integer(kind = kint) :: filter_area_4_each_field
!
!
      filter_area_4_each_field = 0
      if(cmp_no_case(each_filter_ctl%charavalue, 'Fluid_filtering'))    &
     &      filter_area_4_each_field = 1
!
      end function filter_area_4_each_field
!
! -----------------------------------------------------------------------
!
      end module set_control_4_filtering
