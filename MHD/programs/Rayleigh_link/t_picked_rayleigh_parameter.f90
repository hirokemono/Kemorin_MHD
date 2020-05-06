!>@file   t_picked_rayleigh_parameter.f90
!!        module t_picked_rayleigh_parameter
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine init_picked_rayleigh_param(pick_ctl, pick_ra_param)
!!      subroutine dealloc_picked_rayleigh_param(pick_ra_param)
!!        type(pick_rayleigh_spectr_control), intent(in) :: pick_ctl
!!        type(picked_rayleigh_parameter), intent(inout) :: pick_ra_param
!!@endverbatim
!!
!!
      module t_picked_rayleigh_parameter
!
      use m_precision
!
      implicit none
!
      type picked_rayleigh_parameter
!>        Input directory
        character(len = kchara) :: Rayleigh_rst_dir
!>        output file prefix
        character(len = kchara) :: picked_prefix
!>        Time step
        integer(kind = kint) :: i_step
!>        Number of sphedrical harmonics mode to pickup
        integer(kind = kint) :: num_modes
!
!>        Number of sphedrical harmonics mode to pickup
        integer(kind = kint), allocatable :: l_pick(:)
!>        Number of sphedrical harmonics mode to pickup
        integer(kind = kint), allocatable :: m_pick(:)
      end type picked_rayleigh_parameter
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_picked_rayleigh_param(pick_ctl, pick_ra_param)
!
      use t_ctl_pick_rayleigh_spectr
!
      type(pick_rayleigh_spectr_control), intent(in) :: pick_ctl
      type(picked_rayleigh_parameter), intent(inout) :: pick_ra_param
!
!
      pick_ra_param%Rayleigh_rst_dir                                    &
     &    = pick_ctl%Rayleigh_rst_dir_ctl%charavalue
      pick_ra_param%picked_prefix                                       &
     &    = pick_ctl%picked_data_file_name%charavalue
      pick_ra_param%i_step = pick_ctl%Rayleigh_step_ctl%intvalue
!
      pick_ra_param%num_modes = pick_ctl%idx_rayleigh_ctl%num
      allocate(pick_ra_param%l_pick(pick_ra_param%num_modes))
      allocate(pick_ra_param%m_pick(pick_ra_param%num_modes))
!
      pick_ra_param%l_pick(1:pick_ra_param%num_modes)                   &
     &    = pick_ctl%idx_rayleigh_ctl%int1(1:pick_ra_param%num_modes)
      pick_ra_param%m_pick(1:pick_ra_param%num_modes)                   &
     &    = pick_ctl%idx_rayleigh_ctl%int2(1:pick_ra_param%num_modes)
!
      end subroutine init_picked_rayleigh_param
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_picked_rayleigh_param(pick_ra_param)
!
      type(picked_rayleigh_parameter), intent(inout) :: pick_ra_param
!
!
      deallocate(pick_ra_param%l_pick, pick_ra_param%m_pick)
!
      end subroutine dealloc_picked_rayleigh_param
!
! -----------------------------------------------------------------------
!
      end module t_picked_rayleigh_parameter
