!>@file   t_ctl_SGS_3d_filter.f90
!!@brief  module t_ctl_SGS_3d_filter
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2012
!
!>@brief  Structure for filtering controls
!!
!!@verbatim
!!      subroutine read_3d_filtering_ctl                                &
!!     &         (id_control, hd_block, s3df_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(SGS_3d_filter_control), intent(inout) :: s3df_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_3d_filtering_ctl                               &
!!     &         (id_control, hd_block, s3df_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(SGS_3d_filter_control), intent(in) :: s3df_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_3d_filtering_ctl(s3df_ctl)
!!        type(SGS_3d_filter_control), intent(inout) :: s3df_ctl
!!      subroutine copy_3d_filtering_ctl(org_s3df_c, new_s3df_c)
!!        type(SGS_3d_filter_control), intent(in) :: org_s3df_c
!!        type(SGS_3d_filter_control), intent(inout) :: new_s3df_c
!!
!!!!!!!!!  filter Model !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    3d_filtering_ctl
!!      whole_area:   filtering groups for whole domain
!!      fluid_area:   filtering groups for fluid region
!!
!!    filter_4_eqs_ctl:   filtering area for each equation
!!                      (whole_filtering of fluid filtering)
!!          momentum_filter_ctl:     momentum equation
!!          heat_filter_ctl:         heat equation
!!          induction_filter_ctl:    inducition equation
!!          composition_filter_ctl:  composition equation
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      begin 3d_filtering_ctl
!!        array whole_filtering_grp_ctl
!!          whole_filtering_grp_ctl  Both   end
!!          whole_filtering_grp_ctl  whole  end
!!        end array 3d_filtering_ctl
!!
!!        array fluid_filtering_grp_ctl
!!            fluid_filtering_grp_ctl  Both   end
!!            fluid_filtering_grp_ctl  fluid  end
!!        end array fluid_filtering_grp_ctl
!!
!!        momentum_filter_ctl      fluid_filtering
!!        heat_filter_ctl          fluid_filtering
!!        induction_filter_ctl     whole_filtering
!!        composition_filter_ctl   fluid_filtering
!!      end 3d_filtering_ctl
!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
!
      module t_ctl_SGS_3d_filter
!
      use m_precision
!
      use m_machine_parameter
      use skip_comment_f
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_control_array_character
!
      implicit  none
!
!
      type SGS_3d_filter_control
!>        Structure for group list for filtering for whole area
!!@n        whole_filter_grp_ctl%c_tbl: element group name
        type(ctl_array_chara) :: whole_filter_grp_ctl
!>                Structure for field list for filtering in fluid
!!@n                fluid_filter_grp_ctl%c_tbl: element group name
        type(ctl_array_chara) :: fluid_filter_grp_ctl
!
        type(read_character_item) :: momentum_filter_ctl
        type(read_character_item) :: heat_filter_ctl
        type(read_character_item) :: induction_filter_ctl
        type(read_character_item) :: compostion_filter_ctl
!
        integer(kind = kint) :: i_SGS_3d_filter_ctl = 0
      end type SGS_3d_filter_control
!
!    5th level for 3d filtering
!
      character(len=kchara), parameter, private                         &
     &         :: hd_whole_filter_grp = 'whole_filtering_grp_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_fluid_filter_grp = 'fluid_filtering_grp_ctl'
!
      character(len=kchara), parameter, private                         &
     &         :: hd_momentum_filter_ctl = 'momentum_filter_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_heat_filter_ctl =  'heat_filter_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_induction_filter_ctl =  'induction_filter_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_comp_filter_ctl =  'composition_filter_ctl'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_3d_filtering_ctl                                  &
     &         (id_control, hd_block, s3df_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(SGS_3d_filter_control), intent(inout) :: s3df_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(s3df_ctl%i_SGS_3d_filter_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c1(id_control,                          &
     &      hd_whole_filter_grp, s3df_ctl%whole_filter_grp_ctl, c_buf)
        call read_control_array_c1(id_control,                          &
     &      hd_fluid_filter_grp, s3df_ctl%fluid_filter_grp_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_momentum_filter_ctl,         &
     &      s3df_ctl%momentum_filter_ctl)
        call read_chara_ctl_type(c_buf, hd_heat_filter_ctl,             &
     &      s3df_ctl%heat_filter_ctl)
        call read_chara_ctl_type(c_buf, hd_induction_filter_ctl,        &
     &      s3df_ctl%induction_filter_ctl)
        call read_chara_ctl_type(c_buf, hd_comp_filter_ctl,             &
     &      s3df_ctl%compostion_filter_ctl)
      end do
      s3df_ctl%i_SGS_3d_filter_ctl = 1
!
      end subroutine read_3d_filtering_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_3d_filtering_ctl                                 &
     &         (id_control, hd_block, s3df_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(SGS_3d_filter_control), intent(in) :: s3df_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(s3df_ctl%i_SGS_3d_filter_ctl .le. 0) return
!
      maxlen = len_trim(hd_momentum_filter_ctl)
      maxlen = max(maxlen, len_trim(hd_heat_filter_ctl))
      maxlen = max(maxlen, len_trim(hd_induction_filter_ctl))
      maxlen = max(maxlen, len_trim(hd_comp_filter_ctl))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_array_c1(id_control, level,                    &
     &    hd_whole_filter_grp, s3df_ctl%whole_filter_grp_ctl)
      call write_control_array_c1(id_control, level,                    &
     &    hd_fluid_filter_grp, s3df_ctl%fluid_filter_grp_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_momentum_filter_ctl, s3df_ctl%momentum_filter_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_heat_filter_ctl, s3df_ctl%heat_filter_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_induction_filter_ctl, s3df_ctl%induction_filter_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_comp_filter_ctl, s3df_ctl%compostion_filter_ctl)
      level = write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_3d_filtering_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_3d_filtering_ctl(s3df_ctl)
!
      type(SGS_3d_filter_control), intent(inout) :: s3df_ctl
!
!
      call dealloc_control_array_chara(s3df_ctl%whole_filter_grp_ctl)
      call dealloc_control_array_chara(s3df_ctl%fluid_filter_grp_ctl)
!
      s3df_ctl%momentum_filter_ctl%iflag =   0
      s3df_ctl%heat_filter_ctl%iflag =       0
      s3df_ctl%induction_filter_ctl%iflag =  0
      s3df_ctl%compostion_filter_ctl%iflag = 0
!
      s3df_ctl%i_SGS_3d_filter_ctl = 0
!
      end subroutine dealloc_3d_filtering_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_3d_filtering_ctl(org_s3df_c, new_s3df_c)
!
      type(SGS_3d_filter_control), intent(in) :: org_s3df_c
      type(SGS_3d_filter_control), intent(inout) :: new_s3df_c
!
!
      call copy_control_array_c1(org_s3df_c%whole_filter_grp_ctl%num,   &
     &                           org_s3df_c%whole_filter_grp_ctl,       &
     &                           new_s3df_c%whole_filter_grp_ctl)
      call copy_control_array_c1(org_s3df_c%fluid_filter_grp_ctl%num,   &
     &                           org_s3df_c%fluid_filter_grp_ctl,       &
     &                           new_s3df_c%fluid_filter_grp_ctl)
!
      call copy_chara_ctl(org_s3df_c%momentum_filter_ctl,               &
     &                    new_s3df_c%momentum_filter_ctl)
      call copy_chara_ctl(org_s3df_c%heat_filter_ctl,                   &
     &                    new_s3df_c%heat_filter_ctl)
      call copy_chara_ctl(org_s3df_c%induction_filter_ctl,              &
     &                    new_s3df_c%induction_filter_ctl)
      call copy_chara_ctl(org_s3df_c%compostion_filter_ctl,             &
     &                     new_s3df_c%compostion_filter_ctl)
!
      new_s3df_c%i_SGS_3d_filter_ctl = org_s3df_c%i_SGS_3d_filter_ctl
!
      end subroutine copy_3d_filtering_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_SGS_3d_filter
