!>@file   ctl_data_SGS_MHD_model_IO.f90
!!@brief  module ctl_data_SGS_MHD_model_IO
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine read_sph_sgs_mhd_model                               &
!!     &         (id_control, hd_block, model_ctl, sgs_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_model_control), intent(inout) :: model_ctl
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_sph_sgs_mhd_model                              &
!!     &         (id_control, hd_block, model_ctl, sgs_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_model_control), intent(in) :: model_ctl
!!        type(SGS_model_control), intent(in) :: sgs_ctl
!!        integer(kind = kint), intent(inout) :: level
!!@endverbatim
!
      module ctl_data_SGS_MHD_model_IO
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_SGS_model
      use t_ctl_data_MHD_model
!
      use skip_comment_f
!
      implicit none
!
!    label for entry of group
!
      character(len=kchara), parameter, private                         &
     &      :: hd_sgs_ctl = 'SGS_control'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_sph_sgs_mhd_model                                 &
     &         (id_control, hd_block, model_ctl, sgs_ctl, c_buf)
!
      use ctl_data_MHD_model_IO
      use ctl_data_SGS_model_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(mhd_model_control), intent(inout) :: model_ctl
      type(SGS_model_control), intent(inout) :: sgs_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(model_ctl%i_model .gt. 0) return
      call init_sph_mhd_model_label(hd_block, model_ctl)
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_sph_mhd_model_items(id_control, model_ctl, c_buf)
        call read_sgs_ctl                                               &
     &     (id_control, hd_sgs_ctl, sgs_ctl, c_buf)
      end do
      model_ctl%i_model = 1
!
      end subroutine read_sph_sgs_mhd_model
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_sgs_mhd_model                                &
     &         (id_control, hd_block, model_ctl, sgs_ctl, level)
!
      use ctl_data_MHD_model_IO
      use ctl_data_SGS_model_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(mhd_model_control), intent(in) :: model_ctl
      type(SGS_model_control), intent(in) :: sgs_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(model_ctl%i_model .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_sph_mhd_model_items(id_control, model_ctl, level)
      call write_sgs_ctl(id_control, sgs_ctl, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sph_sgs_mhd_model
!
!   --------------------------------------------------------------------
!
      end module ctl_data_SGS_MHD_model_IO
